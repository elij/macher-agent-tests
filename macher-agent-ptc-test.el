;;; macher-agent-ptc-test.el --- Programmatic Tool Calling (PTC) Tests -*- lexical-binding: t; -*-

(let* ((file (or load-file-name buffer-file-name))
       (test-dir (cond
                  (file (file-name-directory (expand-file-name file)))
                  ((file-exists-p (expand-file-name "macher-agent-test-setup.el" default-directory))
                   (expand-file-name default-directory))
                  ((file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" default-directory))
                   (expand-file-name "tests" default-directory))
                  (t (or (locate-dominating-file default-directory "tests") default-directory))))
       (root-dir (locate-dominating-file (or file default-directory) "macher-agent.el")))
  (when root-dir
    (add-to-list 'load-path (expand-file-name root-dir)))
  (add-to-list 'load-path (expand-file-name test-dir))
  (add-to-list 'load-path (expand-file-name "helpers" test-dir)))

(require 'macher-agent-test-setup)

(describe "Programmatic Tool Calling (PTC)"
  (macher-agent-test-setup-before-each)

  (it "executes a PTC script, yielding for subagents and resuming with results"
    (let* ((macher-agent--active-ptc-primitives '(spawn-subagent delegate-tasks-to-subagents read-file))
           (ctx (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/mock/ptc/") :contents nil))
           (script "(let* ((a1 (spawn-subagent \"agent-alpha\" nil))
                   (a2 (spawn-subagent \"agent-beta\" nil))
                   (tasks (list (list :buffer_name a1 :instructions \"task1\")
                                (list :buffer_name a2 :instructions \"task2\"))))
              (delegate-tasks-to-subagents tasks))")
           (spawn-calls nil)
           (spawned-bufs nil)
           (success-result nil)
           (error-result nil))
      (unwind-protect
          (progn
            (spy-on 'macher-agent-add-subagent :and-call-fake
                    (lambda (name &optional _presets _parent-buf _dir _context)
                      (push name spawn-calls)
                      (let ((buf (generate-new-buffer name)))
                        (push buf spawned-bufs)
                        buf)))
            (spy-on 'macher-agent-a2a-dispatch :and-call-fake
                    (lambda (_tasks callback &optional _parent-ctx)
                      (funcall callback
                               (vector
                                (list :status 'success :data "result-alpha" :buffer-name "agent-alpha")
                                (list :status 'success :data "result-beta" :buffer-name "agent-beta")))))
            (macher-agent-execute-ptc-script
             script
             ctx
             (lambda (val) (setq success-result val))
             (lambda (err) (setq error-result err)))
            (expect error-result :to-be nil)
            (expect (reverse spawn-calls) :to-equal '("agent-alpha" "agent-beta"))
            (expect success-result :to-match "All sub-agents completed:")
            (expect success-result :to-match "=== Response from sub-agent ===")
            (expect success-result :to-match "result-alpha")
            (expect success-result :to-match "result-beta"))
        (dolist (b spawned-bufs)
          (when (buffer-live-p b)
            (kill-buffer b))))))

  (it "executes multi-form scripts, handles VFS path resolution, and prevents #. injection"
    (let* ((ws (make-macher-agent-workspace :project-root "/mock/ptc/"))
           (ctx (macher-agent--make-vfs-context
                 :workspace ws
                 :contents (list (macher-agent-vfs-make-entry "/mock/ptc/file.txt" "orig" "vfs content"))))
           (multi-script "(setq x 10)\n(setq y 20)\n(+ x y)")
           (inject-script "#.(setq injected t)")
           (injected nil)
           (multi-res nil)
           (multi-err nil)
           (inject-res nil)
           (inject-err nil))
      ;; Multi-form script evaluation
      (macher-agent-execute-ptc-script
       multi-script ctx
       (lambda (val) (setq multi-res val))
       (lambda (err) (setq multi-err err))
       '(+))
      (expect multi-err :to-be nil)
      (expect multi-res :to-equal 30)

      ;; VFS content prioritizing and path traversal checks
      (expect (macher-agent--read-context-file ctx "/mock/ptc/file.txt") :to-equal "vfs content")
      (expect (macher-agent--read-context-file ctx "../../etc/passwd") :to-throw 'error)

      ;; Prevent #. code injection
      (macher-agent-execute-ptc-script
       inject-script ctx
       (lambda (val) (setq inject-res val))
       (lambda (err) (setq inject-err err)))
      (expect injected :to-be nil)
      (expect inject-err :not :to-be nil)))

  (it "evaluates AST special forms and expressions synchronously in sandbox"
    ;; Basic arithmetic without iter leaks
    (expect (macher-agent-sandbox-run '(+ 10 20) nil) :to-equal 30)
    ;; Apply
    (expect (macher-agent-sandbox-run '(apply #'+ '(1 2 3)) '(+)) :to-equal 6)
    ;; Condition-case caught error and normal path
    (expect (macher-agent-sandbox-run
             '(condition-case err (/ 1 0) (error (car err)))
             '(/))
            :to-equal 'arith-error)
    (expect (macher-agent-sandbox-run
             '(condition-case err (+ 10 5) (error -1))
             '(+))
            :to-equal 15)
    ;; Unwind-protect
    (expect (macher-agent-sandbox-run
             '(let ((cleanup nil))
                (unwind-protect (+ 20 30) (setq cleanup t)))
             '(+))
            :to-equal 50)
    ;; Catch and throw
    (expect (macher-agent-sandbox-run
             '(catch 'done (throw 'done 99) 100)
             nil)
            :to-equal 99))

  (it "handles generator yield and normalizes tool call interrupts"
    (let* ((macher-agent--active-ptc-primitives '(spawn-subagent)))
      ;; Direct eval-iter interrupt
      (let* ((iter (macher-agent-sandbox--eval-iter '(spawn-subagent "test" nil) nil))
             (yield-val (iter-next iter)))
        (expect (plist-get yield-val :interrupt) :to-equal 'tool-call)
        (expect (plist-get yield-val :name) :to-equal 'spawn-subagent))
      ;; Indirect funcall / apply invocation
      (let* ((iter (macher-agent-sandbox--funcall-iter 'spawn-subagent '("agent-alpha")))
             (yield (iter-next iter)))
        (expect (plist-get yield :interrupt) :to-equal 'tool-call)
        (expect (plist-get yield :name) :to-equal 'spawn-subagent)
        (expect (plist-get yield :args) :to-equal '("agent-alpha")))
      ;; Alist arguments converted to plist
      (let* ((iter1 (macher-agent-sandbox--funcall-iter 'spawn-subagent '((path . "/tmp") (name . "test"))))
             (yield1 (iter-next iter1))
             (iter2 (macher-agent-sandbox--funcall-iter 'spawn-subagent '(((path . "/tmp") (name . "test")))))
             (yield2 (iter-next iter2)))
        (expect (plist-get yield1 :args) :to-equal '(:path "/tmp" :name "test"))
        (expect (plist-get yield2 :args) :to-equal '(:path "/tmp" :name "test")))))

  (it "validates primitive matching, dispatch access control, and payload formatting"
    ;; Bidirectional matching and gptel-tools fallback
    (let ((macher-agent--active-ptc-primitives '(spawn-subagent)))
      (expect (macher-agent--ptc-primitive-p 'spawn_subagent) :to-be t)
      (expect (macher-agent--ptc-primitive-p 'spawn-subagent) :to-be t))
    (let ((macher-agent--active-ptc-primitives '("spawn_subagent")))
      (expect (macher-agent--ptc-primitive-p 'spawn-subagent) :to-be t))
    (let* ((macher-agent--active-ptc-primitives nil)
           (tool (gptel-make-tool :name "spawn_subagent" :description "test" :args nil))
           (gptel-tools (list tool)))
      (expect (macher-agent--ptc-primitive-p 'spawn-subagent) :to-be t)
      (expect (macher-agent--ptc-primitive-p 'spawn_subagent) :to-be t))

    ;; Rejection of unauthorized fbound symbols
    (let (cb-called err-called stop-called)
      (macher-agent--dispatch-ptc-primitive
       'delete-file '("/tmp/file.txt") nil "delete-file"
       (lambda (_res) (setq cb-called t))
       (lambda (err) (setq err-called err))
       (lambda (&optional _reason) (setq stop-called t)))
      (expect cb-called :to-be nil)
      (expect stop-called :to-be t)
      (expect err-called :to-equal '(:status error :error "ERROR: Tool 'delete-file' is not accessible.")))

    ;; Explicitly allowed primitive dispatch
    (let* ((macher-agent--active-ptc-primitives '(my-custom-primitive))
           (primitive-called nil) (cb-res nil) (err-res nil))
      (cl-letf (((symbol-function 'my-custom-primitive)
                 (lambda (arg) (setq primitive-called t) (format "result: %s" arg))))
        (macher-agent--dispatch-ptc-primitive
         'my-custom-primitive '("payload-data") nil "my-custom-primitive"
         (lambda (res) (setq cb-res res))
         (lambda (err) (setq err-res err))
         nil)
        (expect primitive-called :to-be t)
        (expect cb-res :to-equal "result: payload-data")
        (expect err-res :to-be nil)))

    ;; Payload extraction and format bypass under PTC execution
    (let* ((cb-raw-val nil) (cb-fmt-val nil)
           (cb-raw (macher-agent--build-success-callback
                    'test-tool nil nil nil
                    (lambda (res) (setq cb-raw-val (plist-get res :data)))))
           (cb-fmt (macher-agent--build-success-callback
                    'test-tool nil
                    (lambda (p) (format "Formatted: %s" p))
                    nil
                    (lambda (res) (setq cb-fmt-val (plist-get res :data))))))
      (funcall cb-raw "raw payload")
      (expect cb-raw-val :to-equal "raw payload")
      (funcall cb-fmt "raw payload")
      (expect cb-fmt-val :to-equal "Formatted: raw payload"))

    (let* ((mock-tool (macher-agent-make-tool mock-ptc-format-tool
                          "Test tool"
                        :category "test"
                        :args '((:name "val" :type string))
                        :command-fn (lambda (payload _context _root)
                                      `((status . "success") (value . ,(plist-get payload :val))))
                        :success-fn (lambda (res _payload)
                                      (format "Formatted output: %s" (cdr (assoc 'value res))))))
           (normal-res nil)
           (ptc-res nil))
      (funcall (gptel-tool-function mock-tool) (lambda (res) (setq normal-res res)) "hello")
      (expect normal-res :to-equal "Formatted output: hello")
      (let ((macher-agent--active-ptc-execution t))
        (funcall (gptel-tool-function mock-tool) (lambda (res) (setq ptc-res res)) "hello")
        (expect ptc-res :to-equal '((status . "success") (value . "hello"))))))

  (it "injects PTC instructions into prompts and transmission state"
    (let* ((tool (gptel-make-tool
                  :name "spawn-subagent"
                  :description "Spawn subagent"
                  :args '((:name "path" :type "string"))))
           (gptel-tools (list tool))
           (macher-agent--active-ptc-primitives '(spawn-subagent))
           (base-prompt "Initial system prompt.")
           (injected (macher-agent--inject-ptc-prompt base-prompt)))
      (expect injected :to-match "^Initial system prompt\\.")
      (expect injected :to-match "=== PROGRAMMATIC TOOL CALLING (PTC) ===")
      (expect injected :to-match "spawn-subagent -> (defun spawn-subagent (path)")
      (let ((macher-agent--active-ptc-primitives '(unmatched-primitive)))
        (expect (macher-agent--inject-ptc-prompt base-prompt) :to-equal base-prompt))

      (let* ((orig-buf (generate-new-buffer "test-ptc-append-directive-buf"))
             (state (make-macher-agent-transmission-state
                     :target-buffer orig-buf
                     :ptc-primitives '(spawn-subagent)
                     :tools (list tool))))
        (unwind-protect
            (progn
              (setq state (macher-agent-sandbox-append-ptc-directive state orig-buf nil nil nil))
              (expect (length (macher-agent-transmission-state-directives state)) :to-equal 1)
              (expect (car (macher-agent-transmission-state-directives state))
                      :to-match "=== PROGRAMMATIC TOOL CALLING (PTC) ==="))
          (kill-buffer orig-buf)))))

  (describe "Plugin State and Lifecycle"
    (it "manages sandbox state polymorphically across struct, plist, and alist contexts"
      ;; Struct context
      (let ((ctx (make-macher-agent-context :id "ctx-ptc-struct" :plugins '(:existing-key "value"))))
        (expect (macher-agent-sandbox-get-state ctx) :to-be nil)
        (macher-agent-sandbox-set-state ctx '(:active-primitives (spawn-subagent) :eval-mode safe))
        (expect (macher-agent-sandbox-get-state ctx) :to-equal '(:active-primitives (spawn-subagent) :eval-mode safe))
        (expect (plist-get (macher-agent-context-plugins ctx) :sandbox) :to-equal '(:active-primitives (spawn-subagent) :eval-mode safe))
        (expect (plist-get (macher-agent-context-plugins ctx) :existing-key) :to-equal "value"))
      ;; Plist contexts
      (let ((raw-ctx (list :id "raw-ptc-1" :sandbox '(:active-primitives (spawn-subagent))))
            (raw-nested (list :id "raw-nested-1" :plugins '(:sandbox (:active-primitives (spawn-subagent))))))
        (expect (macher-agent-sandbox-get-state raw-ctx) :to-equal '(:active-primitives (spawn-subagent)))
        (expect (macher-agent-sandbox-set-state raw-ctx '(:active-primitives (spawn-subagent read-file)))
                :to-equal '(:active-primitives (spawn-subagent read-file)))
        (expect (macher-agent-sandbox-get-state raw-nested) :to-equal '(:active-primitives (spawn-subagent)))
        (macher-agent-sandbox-set-state raw-nested '(:active-primitives (delegate-tasks-to-subagents)))
        (expect (macher-agent-sandbox-get-state raw-nested) :to-equal '(:active-primitives (delegate-tasks-to-subagents))))
      ;; Alist contexts
      (let ((raw-alist (list (cons :sandbox '(:active-primitives (spawn-subagent)))))
            (sym-alist (list (cons 'sandbox '(:active-primitives (spawn-subagent))))))
        (expect (macher-agent-sandbox-get-state raw-alist) :to-equal '(:active-primitives (spawn-subagent)))
        (macher-agent-sandbox-set-state raw-alist '(:active-primitives (read-file)))
        (expect (macher-agent-sandbox-get-state raw-alist) :to-equal '(:active-primitives (read-file)))
        (expect (macher-agent-sandbox-get-state sym-alist) :to-equal '(:active-primitives (spawn-subagent)))
        (macher-agent-sandbox-set-state sym-alist '(:active-primitives (read-file)))
        (expect (macher-agent-sandbox-get-state sym-alist) :to-equal '(:active-primitives (read-file)))))

    (it "registers and unregisters pipeline steps via install and uninstall"
      (let ((saved-registry (copy-hash-table macher-agent-pipeline-registry)))
        (unwind-protect
            (progn
              (clrhash macher-agent-pipeline-registry)
              (macher-agent-sandbox-install)
              (expect (member #'macher-agent-ptc--inject-tool
                              (macher-agent-get-pipeline-steps 'preset-composition))
                      :to-be-truthy)
              (expect (member #'macher-agent-sandbox-append-ptc-directive
                              (macher-agent-get-pipeline-steps 'transmission))
                      :to-be-truthy)
              (when (fboundp 'macher-agent-sandbox-uninstall)
                (macher-agent-sandbox-uninstall)
                (expect (member #'macher-agent-ptc--inject-tool
                                (macher-agent-get-pipeline-steps 'preset-composition))
                        :to-be nil)
                (expect (member #'macher-agent-sandbox-append-ptc-directive
                                (macher-agent-get-pipeline-steps 'transmission))
                        :to-be nil)))
          (setq macher-agent-pipeline-registry saved-registry))))))

(provide 'macher-agent-ptc-test)
;;; macher-agent-ptc-test.el ends here
