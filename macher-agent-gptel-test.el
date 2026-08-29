;;; macher-agent-gptel-test.el --- Tests for Macher Agent GPTel Boundary -*- lexical-binding: t; -*-

(let* ((file (or load-file-name buffer-file-name))
       (test-dir (cond
                  (file (file-name-directory (expand-file-name file)))
                  ((file-exists-p (expand-file-name "macher-agent-test-setup.el" default-directory))
                   default-directory)
                  ((file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" default-directory))
                   (expand-file-name "tests" default-directory))
                  (t default-directory))))
  (add-to-list 'load-path test-dir)
  (add-to-list 'load-path (expand-file-name "helpers" test-dir)))

(require 'macher-agent-test-setup)
(require 'macher-agent-gptel)

(describe "Macher-Agent GPTel Boundary Suite"
  (macher-agent-test-setup-before-each)

  (describe "FSM Target Buffer & Context Extraction"
    (it "extracts target buffer and context from varied FSM structures and buffers"
      (let* ((buf (generate-new-buffer "test-gptel-fsm-buf"))
             (mock-ctx (macher-agent--make-context :project-root "/mock/proj/"))
             (fsm1 (gptel-make-fsm :info (list :buffer buf :macher-agent-context mock-ctx)))
             (fsm2 (gptel-make-fsm :info (list :buffer buf :context mock-ctx)))
             (fsm3 (gptel-make-fsm :info (list :buffer buf)))
             (fsm-empty (gptel-make-fsm :info nil)))
        (unwind-protect
            (progn
              ;; Buffer extraction
              (expect (macher-agent-gptel--fsm-target-buffer nil) :to-be nil)
              (expect (macher-agent-gptel--fsm-target-buffer fsm-empty) :to-be nil)
              (expect (macher-agent-gptel--fsm-target-buffer fsm1) :to-be buf)

              ;; Context extraction directly from info
              (expect (macher-agent-gptel--fsm-context nil) :to-be nil)
              (expect (macher-agent-gptel--fsm-context fsm1) :to-be mock-ctx)
              (expect (macher-agent-gptel--fsm-context fsm2) :to-be mock-ctx)
              (expect (macher-agent-gptel--fsm-context mock-ctx) :to-be mock-ctx)

              ;; Context extraction fallback from buffer-local state
              (with-current-buffer buf
                (setq-local macher-agent--persistent-context mock-ctx))
              (expect (macher-agent-gptel--fsm-context fsm3) :to-be mock-ctx)
              (expect (macher-agent-gptel--fsm-context nil buf) :to-be mock-ctx))
          (when (buffer-live-p buf)
            (kill-buffer buf))))))

  (describe "FSM Hijack Transformation & Handler Augmentation"
    (it "augments FSM handlers with media injection on WAIT and flush triggers on terminal states"
      (let* ((buf (generate-new-buffer "test-gptel-hijack-buf"))
             (cb-called nil)
             (orig-cb (lambda (resp &rest _args) (setq cb-called resp)))
             (fsm (gptel-make-fsm :info (list :buffer buf :callback orig-cb))))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (insert "User prompt to be captured"))
              (macher-agent--fsm-hijack-transform #'ignore fsm)

              ;; Handlers augmented
              (let ((handlers (gptel-fsm-handlers fsm)))
                (expect (memq #'macher-agent--inject-media-fsm-logic (alist-get 'WAIT handlers)) :to-be-truthy)
                (expect (memq #'macher-agent-gptel--trigger-flush (alist-get 'DONE handlers)) :to-be-truthy)
                (expect (memq #'macher-agent-gptel--trigger-flush (alist-get 'ABRT handlers)) :to-be-truthy)
                (expect (memq #'macher-agent-gptel--trigger-flush (alist-get 'ERRS handlers)) :to-be-truthy))

              ;; Protected callback invocation
              (let ((wrapped-cb (plist-get (gptel-fsm-info fsm) :callback)))
                (funcall wrapped-cb "test response")
                (expect cb-called :to-equal "test response")
                (funcall wrapped-cb nil (list :tool-use '((:name "test_tool"))))
                (expect cb-called :to-equal "")))
          (when (buffer-live-p buf)
            (kill-buffer buf))))))

  (describe "Prompt Transformation and Inline Skill Parsing"
    (it "extracts and strips inline skill tags and resolves exclusive presets"
      (let* ((known '((elisp-expert . (:exclusive t :system "Elisp system"))
                      (helper . (:system "Helper system"))))
             (orig-buf (generate-new-buffer "test-inline-skills-buf")))
        (unwind-protect
            (with-current-buffer orig-buf
              (setq-local gptel--known-presets known)
              (insert "@elisp-expert @helper please analyze this code")
              (let* ((extraction (macher-agent--extract-inline-skills (point-min) orig-buf))
                     (matched (car extraction))
                     (used (cdr extraction)))
                (expect matched :to-equal '(elisp-expert helper))
                (expect used :to-be t)
                (expect (buffer-string) :to-equal "please analyze this code"))

              ;; Exclusive skill resolution
              (let ((resolved (macher-agent--transformer-resolve-skills '(helper) '(elisp-expert) known)))
                (expect resolved :to-equal '(elisp-expert))))
          (when (buffer-live-p orig-buf)
            (kill-buffer orig-buf)))))

    (it "detects skill command redirect when prompt contains only the inline skill tag"
      (let ((orig-buf (generate-new-buffer "test-redirect-buf")))
        (unwind-protect
            (with-current-buffer orig-buf
              (insert "   ")
              (expect (macher-agent--transformer-detect-redirect t (point-min) '(terminal)) :to-equal 'terminal)
              (erase-buffer)
              (insert "do some work")
              (expect (macher-agent--transformer-detect-redirect t (point-min) '(terminal)) :to-be nil))
          (when (buffer-live-p orig-buf)
            (kill-buffer orig-buf))))))

  (describe "Transmission Pipeline Reducer & Directives Compilation"
    (it "compiles base prompt, task submission directives, boot directives, and thought queue"
      (let* ((init-buf (generate-new-buffer "test-pipe-init-buf"))
             (subseq-buf (generate-new-buffer "test-pipe-subseq-buf"))
             (submit-tool (gptel-make-tool :name "submit_task_result"
                                           :description "Submit final answer"
                                           :args nil))
             (state (make-macher-agent-transmission-state
                     :base-prompt "Base System Prompt"
                     :target-buffer init-buf
                     :tools (list submit-tool))))
        (unwind-protect
            (progn
              (with-current-buffer init-buf
                (setq-local macher-agent--boot-directive "Boot setup directive.")
                (macher-agent-add-pending-instruction "Queued user thought"))

              ;; Pipeline step applications
              (setq state (macher-agent-pipe--init-core-directives state init-buf nil nil nil))
              (setq state (macher-agent-pipe--append-boot-directive state init-buf nil nil nil))
              (setq state (macher-agent-pipe--drain-thought-queue state init-buf nil nil nil))
              (setq state (macher-agent-pipe--compile-directives state init-buf nil nil nil))

              (let ((compiled (macher-agent-transmission-state-compiled-prompt state)))
                (expect compiled :to-match "Base System Prompt")
                (expect compiled :to-match "CRITICAL DIRECTIVE: You MUST use the `submit_task_result' tool")
                (expect compiled :to-match "Boot setup directive.")
                (expect compiled :to-match "Queued user thought"))

              ;; Subsequent turn skips boot directive when response property is present
              (with-current-buffer subseq-buf
                (setq-local macher-agent--boot-directive "Boot setup directive.")
                (insert "Previous response")
                (put-text-property (point-min) (point-max) 'gptel 'response))
              (let ((state2 (make-macher-agent-transmission-state :target-buffer subseq-buf)))
                (setq state2 (macher-agent-pipe--append-boot-directive state2 subseq-buf nil nil nil))
                (expect (macher-agent-transmission-state-directives state2) :to-be nil)))
          (when (buffer-live-p init-buf) (kill-buffer init-buf))
          (when (buffer-live-p subseq-buf) (kill-buffer subseq-buf))))))

  (describe "Tool Scope Enforcement & Deduplication"
    (it "enforces tool scope against active FSM authorized tools"
      (let* ((allowed-tool (gptel-make-tool :name "read_file" :description "Read file" :args nil))
             (fsm (gptel-make-fsm :info (list :tools (list allowed-tool)))))
        (expect (macher-agent--enforce-tool-scope "read_file" fsm) :to-be nil)
        (let ((block-res (macher-agent--enforce-tool-scope "unauthorized_tool" fsm)))
          (expect (plist-get block-res :block) :to-match "ERROR: Tool 'unauthorized_tool' is not accessible"))))

    (it "deduplicates excessive identical tool invocations in buffer context"
      (with-temp-buffer
        (let ((macher-agent-max-duplicate-tools 1))
          (insert "```tool read_file\n{\"path\": \"foo.el\"}\n```\n")
          (insert "```tool read_file\n{\"path\": \"foo.el\"}\n```\n")
          (put-text-property (point-min) (point-max) 'gptel 'response)
          (macher-agent-transformer-deduplicate-tools nil nil)
          (expect (buffer-string) :to-match "{\"status\": \"omitted\", \"reason\": \"duplicate\"}")))))

  (describe "Media Injection Lifecycle"
    (it "rejects invalid media formats and clears media queue upon injection"
      (let* ((mock-ctx (macher-agent--make-context :project-root "/mock/proj/"))
             (fsm (gptel-make-fsm :info (list :macher-agent-context mock-ctx))))
        (setf (macher-agent-context-media-queue mock-ctx) 12345)
        (expect (macher-agent--perform-pending-media-injection fsm)
                :to-throw 'error)

        (setf (macher-agent-context-media-queue mock-ctx) "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==")
        (macher-agent--perform-pending-media-injection fsm)
        (expect (macher-agent-context-media-queue mock-ctx) :to-be nil))))

  (describe "Transmission Dispatch & Completion Flush"
    (it "transmits request via macher-agent-gptel-transmit with system message and gptel-send"
      (let* ((buf (generate-new-buffer "test-transmit-buf"))
             (task-ctx (make-macher-agent-task-context
                        :target-buffer buf
                        :system-message "Transmitted System Message")))
        (unwind-protect
            (progn
              (macher-agent-gptel-transmit task-ctx nil)
              (expect 'gptel-send :to-have-been-called)
              (with-current-buffer buf
                (expect gptel-system-prompt :to-equal "Transmitted System Message")))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "triggers flush hook and drains instructions queue on FSM completion"
      (let* ((buf (generate-new-buffer "test-flush-hook-buf"))
             (mock-ctx (macher-agent--make-context :project-root "/mock/proj/"))
             (fsm (gptel-make-fsm :info (list :buffer buf :macher-agent-context mock-ctx)
                                  :state 'DONE))
             (flush-called nil)
             (hook-fn (lambda (&rest _) (setq flush-called t))))
        (unwind-protect
            (with-current-buffer buf
              (setq-local macher-agent--persistent-context mock-ctx)
              (setq-local macher-agent--pending-instructions-queue '("queued-item"))
              (add-hook 'macher-agent-task-flush-hook hook-fn)
              (macher-agent-gptel--trigger-flush fsm)
              (expect flush-called :to-be t)
              (expect macher-agent--pending-instructions-queue :to-be nil))
          (remove-hook 'macher-agent-task-flush-hook hook-fn)
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

(provide 'macher-agent-gptel-test)
;;; macher-agent-gptel-test.el ends here
