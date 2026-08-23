;;; macher-agent-test-harness.el --- Shared testing utilities for Macher Agent -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'gptel)
(require 'macher-agent-core)
(require 'macher-agent-vfs)
(require 'macher-agent-presets)
(require 'macher-agent-gptel)
(require 'macher-agent-orchestration)
(require 'macher-agent-api)

(defun macher-agent--fsm-hijack-transform (callback fsm)
  "Mutate the FSM to inject media, capture prompts, protect callbacks, and trigger patches."
  (let* ((info (gptel-fsm-info fsm))
         (orig-cb (plist-get info :callback))
         (b-prop (macher-agent--extract-prop info :buffer))
         (target-buf (if (eq b-prop 'macher-missing) nil b-prop)))

    (when (and target-buf (buffer-live-p target-buf))
      (with-current-buffer target-buf
        (setq-local macher--fsm-latest fsm)))

    (let* ((prompt-start
            (if (or (bound-and-true-p gptel-mode) 
                    (bound-and-true-p gptel-track-response))
                (if (and (> (point-max) (point-min)) 
                         (get-text-property (1- (point-max)) 'gptel))
                    (point-max)
                  (or (previous-single-property-change (point-max) 'gptel) (point-min)))
              (point-min)))
           (raw-prompt
            (if (fboundp 'gptel--trim-prefixes)
                (gptel--trim-prefixes
                 (buffer-substring-no-properties prompt-start (point-max)))
              (string-trim (buffer-substring-no-properties prompt-start (point-max))))))
      (when (and raw-prompt (not (string-empty-p raw-prompt)))
        (setq info (plist-put info :prompt raw-prompt))
        (setf (gptel-fsm-info fsm) info)
        (let ((ctx (when (and target-buf (buffer-live-p target-buf))
                     (buffer-local-value 'macher-agent--persistent-context target-buf))))
          (when ctx
            (when (fboundp 'macher-agent--set-context-prompt)
              (macher-agent--set-context-prompt ctx raw-prompt))
            (when (fboundp 'macher-agent--set-context-data)
              (macher-agent--set-context-data ctx :prompt raw-prompt))
            (ignore-errors (setf (macher-context-prompt ctx) raw-prompt))))))

    (let ((cb (or orig-cb #'ignore)))
      (setq info
            (plist-put (gptel-fsm-info fsm) :callback
                       (lambda (response &rest args)
                         (let ((info-arg (car args)))
                           (when (or response (and (listp info-arg) (plist-get info-arg :tool-use)))
                             (apply cb (or response "") args))))))
      (setf (gptel-fsm-info fsm) info))
    
    (let* ((handlers (gptel-fsm-handlers fsm))
           (all-states (delete-dups (append (mapcar #'car handlers) '(WAIT DONE ERRS ABRT))))
           (augmented-handlers
            (cl-loop
             for state in all-states
             for funcs = (alist-get state handlers)
             collect (cons state 
                           (cond
                            ((eq state 'WAIT)
                             (cons #'macher-agent--inject-media-fsm-logic funcs))
                            ((memq state '(DONE ERRS ABRT))
                             (append funcs (list #'macher-agent-gptel--trigger-flush)))
                            (t funcs))))))
      (setf (gptel-fsm-handlers fsm) augmented-handlers)))
  
  (when (functionp callback)
    (funcall callback)))

(defmacro with-macher-agent-mock-fsm (ctx &rest body)
  "Execute BODY synchronously while pretending an FSM is active with CTX."
  (declare (indent 1))
  `(let* ((mock-info (list :macher-agent-context ,ctx))
          (mock-fsm (if (fboundp 'gptel-make-fsm)
                        (gptel-make-fsm :state 'RUNNING :info mock-info)
                      mock-info))
          (macher-agent--active-fsm mock-fsm)
          (gptel--fsm-last mock-fsm))
     ,@body))

(defmacro with-macher-agent-test-context (routing-alist call-counter &rest body)
  "Execute BODY with gptel's network requests mocked using raw string responses.
ROUTING-ALIST maps buffer name substrings to a list of raw string responses.
CALL-COUNTER is a symbol bound in the calling environment that increments on dispatch."
  (declare (indent 2))
  `(let* ((queues (copy-tree ,routing-alist))
          (ws (make-macher-agent-workspace :project-root default-directory))
          (ctx (macher--make-context :workspace ws :contents nil)))

     (setq-local macher-agent--persistent-context ctx)
     (puthash (expand-file-name default-directory) ctx macher-agent-active-workspaces)
     (puthash (expand-file-name (macher-agent-root default-directory)) ctx macher-agent-active-workspaces)
     (macher-agent-initialize-skills
      ctx (or (bound-and-true-p macher-agent--bundled-skills-dir)
              (bound-and-true-p macher-agent-bundled-skills-directory)))

     (let ((gptel-use-curl t)
           (gptel-confirm-tool-calls nil)
           (gptel-backend (gptel-make-openai "Mock" :key "mock-key" :models '(mock-model)))
           (gptel-model 'mock-model)
           (gptel-tools (hash-table-values (macher-agent-workspace-tools-registry ws))))

       (cl-letf (((symbol-function 'macher-agent-resolve-context)
                  (lambda (&optional input)
                    (cond
                     ((macher-agent-valid-context-p input) input)
                     ((and (bufferp input) (buffer-live-p input)
                           (buffer-local-value 'macher-agent--persistent-context input)))
                     ((and (stringp input) (get-buffer input) (buffer-live-p (get-buffer input))
                           (buffer-local-value 'macher-agent--persistent-context (get-buffer input))))
                     (t ctx))))
                 ((symbol-function 'gptel--get-api-key) 
                  (lambda (&rest _) "mock-key"))
                 ((symbol-function 'gptel-curl-get-response)
                  (lambda (info-or-fsm &optional callback)
                    (let* ((fsm (if (and (fboundp 'gptel-fsm-p) (gptel-fsm-p info-or-fsm)) info-or-fsm nil))
                           (info (if fsm (gptel-fsm-info fsm) info-or-fsm))
                           (cb (or callback (plist-get info :callback) #'gptel--insert-response))
                           (target-buf (plist-get info :buffer))
                           (buf-name (if target-buf (buffer-name target-buf) (buffer-name)))
                           (queue-pair (cl-find-if (lambda (pair) (string-match-p (car pair) buf-name)) queues))
                           (resp (and queue-pair (pop (cdr queue-pair)))))
                      
                      (when fsm
                        (setq info (plist-put info :macher-agent-context ctx))
                        (setf (gptel-fsm-info fsm) info)
                        (when target-buf
                          (with-current-buffer target-buf
                            (setq-local gptel--fsm-last fsm))))
                      
                      (let ((run-response
                             (lambda ()
                               (when (buffer-live-p target-buf)
                                 (when fsm
                                   (setq info (plist-put info :http-status "200"))
                                   (setq info (plist-put info :status "200 OK"))
                                   (setf (gptel-fsm-info fsm) info)
                                   (gptel--fsm-transition fsm))
                                 
                                 (if resp
                                     (progn
                                       (when (and fsm (plist-get resp :tool-use))
                                         (let ((tool-use-list nil))
                                           (cl-loop for tool-req in (plist-get resp :tool-use)
                                                    for i from 1
                                                    do
                                                    (let* ((tool-name (car tool-req))
                                                           (tool-vals (cdr tool-req))
                                                           (tool-spec (or (cl-find-if (lambda (ts) (equal (gptel-tool-name ts) tool-name))
                                                                                      (plist-get info :tools))
                                                                          (ignore-errors (gptel-get-tool tool-name)))))
                                                      (unless tool-spec
                                                        (error "Unknown mock tool: %s" tool-name))
                                                      (unless (cl-find-if (lambda (ts) (equal (gptel-tool-name ts) tool-name)) (plist-get info :tools))
                                                        (setq info (plist-put info :tools (append (plist-get info :tools) (list tool-spec))))
                                                        (setf (gptel-fsm-info fsm) info))
                                                      (let ((expected-args (gptel-tool-args tool-spec))
                                                            (args-plist nil))
                                                        (cl-loop for arg in expected-args
                                                                 for val in tool-vals
                                                                 do (setq args-plist (plist-put args-plist (intern (concat ":" (if (symbolp (plist-get arg :name)) (symbol-name (plist-get arg :name)) (plist-get arg :name)))) val)))
                                                        (push (list :id (format "call_%s_%d" buf-name i)
                                                                    :name tool-name
                                                                    :args args-plist)
                                                              tool-use-list))))
                                           (setq info (plist-put info :tool-use (nreverse tool-use-list)))
                                           (setf (gptel-fsm-info fsm) info)))
                                       
                                       (if (plist-get resp :text)
                                           (when (functionp cb) (funcall cb (plist-get resp :text) info))
                                         (when (functionp cb) (funcall cb nil info))))
                                   (when (functionp cb) (funcall cb "Fallback mock response." info)))
                                 
                                 (when fsm
                                   (gptel--fsm-transition fsm))
                                 (cl-incf ,call-counter)))))
                        (run-at-time 0.01 nil run-response)))))
                 ((symbol-function 'gptel--url-get-response)
                  (symbol-function 'gptel-curl-get-response)))
         ,@body))))

(provide 'macher-agent-test-harness)
;;; macher-agent-test-harness.el ends here
