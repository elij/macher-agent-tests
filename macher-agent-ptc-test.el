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

          (it "safely evaluates synchronous expressions without leaking iter-end-of-sequence"
              (let ((val (macher-agent-sandbox-run '(+ 10 20) nil)))
                (expect val :to-equal 30)))

          (it "yields PTC tool interrupts when evaluated via eval-iter"
              (let* ((macher-agent--active-ptc-primitives '(spawn-subagent))
                     (iter (macher-agent-sandbox--eval-iter '(spawn-subagent "test" nil) nil))
                     (yield-val (iter-next iter)))
                (expect (plist-get yield-val :interrupt) :to-equal 'tool-call)
                (expect (plist-get yield-val :name) :to-equal 'spawn-subagent)))

          (it "injects PTC prompt instructions conditionally based on primitive matches"
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
                  (expect (macher-agent--inject-ptc-prompt base-prompt) :to-equal base-prompt))))

          (it "appends ptc directive to transmission state via macher-agent-sandbox-append-ptc-directive"
              (let* ((tool (gptel-make-tool
                            :name "spawn-subagent"
                            :description "Spawn subagent"
                            :args '((:name "path" :type "string"))))
                     (orig-buf (generate-new-buffer "test-ptc-append-directive-buf"))
                     (state (make-macher-agent-transmission-state
                             :target-buffer orig-buf
                             :ptc-primitives '(spawn-subagent)
                             :tools (list tool))))
                (setq state (macher-agent-sandbox-append-ptc-directive state orig-buf nil nil nil))
                (expect (length (macher-agent-transmission-state-directives state)) :to-equal 1)
                (expect (car (macher-agent-transmission-state-directives state))
                        :to-match "=== PROGRAMMATIC TOOL CALLING (PTC) ===")
                (kill-buffer orig-buf)))

          (it "extracts raw payload and applies success-fn formatting"
              (let* ((res "raw payload")
                     (cb-raw-val nil)
                     (cb-formatted-val nil)
                     (cb-raw (macher-agent--build-success-callback
                              'test-tool nil nil nil
                              (lambda (res) (setq cb-raw-val (plist-get res :data)))))
                     (cb-fmt (macher-agent--build-success-callback
                              'test-tool nil
                              (lambda (p) (format "Formatted: %s" p))
                              nil
                              (lambda (res) (setq cb-formatted-val (plist-get res :data))))))
                (funcall cb-raw res)
                (expect cb-raw-val :to-equal "raw payload")
                (funcall cb-fmt res)
                (expect cb-formatted-val :to-equal "Formatted: raw payload")))

          (it "allows indirect PTC primitive calls via funcall and apply inside sandbox"
              (let* ((macher-agent--active-ptc-primitives '(spawn-subagent))
                     (iter1 (macher-agent-sandbox--funcall-iter 'spawn-subagent '("agent-alpha")))
                     (yield1 (iter-next iter1)))
                (expect (plist-get yield1 :interrupt) :to-equal 'tool-call)
                (expect (plist-get yield1 :name) :to-equal 'spawn-subagent)
                (expect (plist-get yield1 :args) :to-equal '("agent-alpha"))))

          (it "converts alist arguments passed to PTC primitives to plists on tool call interrupts"
              (let* ((macher-agent--active-ptc-primitives '(spawn-subagent))
                     (iter1 (macher-agent-sandbox--funcall-iter 'spawn-subagent '((path . "/tmp") (name . "test"))))
                     (yield1 (iter-next iter1))
                     (iter2 (macher-agent-sandbox--funcall-iter 'spawn-subagent '(((path . "/tmp") (name . "test")))))
                     (yield2 (iter-next iter2)))
                (expect (plist-get yield1 :interrupt) :to-equal 'tool-call)
                (expect (plist-get yield1 :name) :to-equal 'spawn-subagent)
                (expect (plist-get yield1 :args) :to-equal '(:path "/tmp" :name "test"))
                (expect (plist-get yield2 :interrupt) :to-equal 'tool-call)
                (expect (plist-get yield2 :name) :to-equal 'spawn-subagent)
                (expect (plist-get yield2 :args) :to-equal '(:path "/tmp" :name "test"))))

          (it "matches PTC primitives bidirectionally and falls back to gptel-tools when active list is nil"
              (let ((macher-agent--active-ptc-primitives '(spawn-subagent)))
                (expect (macher-agent--ptc-primitive-p 'spawn_subagent) :to-be t)
                (expect (macher-agent--ptc-primitive-p 'spawn-subagent) :to-be t))
              (let ((macher-agent--active-ptc-primitives '("spawn_subagent")))
                (expect (macher-agent--ptc-primitive-p 'spawn-subagent) :to-be t))
              (let* ((macher-agent--active-ptc-primitives nil)
                     (tool (gptel-make-tool :name "spawn_subagent" :description "test" :args nil))
                     (gptel-tools (list tool)))
                (expect (macher-agent--ptc-primitive-p 'spawn-subagent) :to-be t)
                (expect (macher-agent--ptc-primitive-p 'spawn_subagent) :to-be t)))

          (it "bypasses success-fn formatting and preserves raw command-fn payload during PTC execution"
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
                ;; Standard execution (outside PTC)
                (funcall (gptel-tool-function mock-tool)
                         (lambda (res) (setq normal-res res))
                         "hello")
                (expect normal-res :to-equal "Formatted output: hello")
                ;; PTC execution
                (let ((macher-agent--active-ptc-execution t))
                  (funcall (gptel-tool-function mock-tool)
                           (lambda (res) (setq ptc-res res))
                           "hello")
                  (expect ptc-res :to-equal '((status . "success") (value . "hello"))))))

          (it "validates that macher-agent--dispatch-ptc-primitive rejects arbitrary fbound lisp symbols not in allowed primitives or tools"
              (let ((cb-called nil)
                    (err-called nil)
                    (stop-called nil))
                (macher-agent--dispatch-ptc-primitive
                 'delete-file
                 '("/tmp/some-file.txt")
                 nil
                 "delete-file"
                 (lambda (_res) (setq cb-called t))
                 (lambda (err) (setq err-called err))
                 (lambda (&optional _reason) (setq stop-called t)))
                (expect cb-called :to-be nil)
                (expect stop-called :to-be t)
                (expect err-called :to-equal '(:status error :error "ERROR: Tool 'delete-file' is not accessible."))))

          (it "allows explicitly allowed primitive symbols in macher-agent--dispatch-ptc-primitive"
              (let* ((macher-agent--active-ptc-primitives '(my-custom-primitive))
                     (primitive-called nil)
                     (cb-res nil)
                     (err-res nil))
                (cl-letf (((symbol-function 'my-custom-primitive)
                           (lambda (arg) (setq primitive-called t) (format "result: %s" arg))))
                  (macher-agent--dispatch-ptc-primitive
                   'my-custom-primitive
                   '("payload-data")
                   nil
                   "my-custom-primitive"
                   (lambda (res) (setq cb-res res))
                   (lambda (err) (setq err-res err))
                   nil)
                  (expect primitive-called :to-be t)
                  (expect cb-res :to-equal "result: payload-data")
                  (expect err-res :to-be nil))))

          (it "executes a PTC script containing multiple top-level forms"
              (let* ((ctx (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/mock/ptc/") :contents nil))
                     (script "(setq x 10)\n(setq y 20)\n(+ x y)")
                     (success-result nil)
                     (error-result nil))
                (macher-agent-execute-ptc-script
                 script
                 ctx
                 (lambda (val) (setq success-result val))
                 (lambda (err) (setq error-result err))
                 '(+))
                (expect error-result :to-be nil)
                (expect success-result :to-equal 30)))

          (it "validates and resolves paths with bounds checking in macher-agent--read-file-vfs-aware"
              (let* ((ws (make-macher-agent-workspace :project-root "/mock/ptc/"))
                     (ctx (macher-agent--make-vfs-context :workspace ws :contents (list (macher-agent-vfs-make-entry "/mock/ptc/file.txt" "orig" "vfs content")))))
                ;; 1. VFS content prioritized
                (expect (macher-agent--read-file-vfs-aware "/mock/ptc/file.txt" ctx) :to-equal "vfs content")
                ;; 2. Path traversal blocked
                (expect (macher-agent--read-file-vfs-aware "../../etc/passwd" ctx) :to-throw 'error)))

          (it "prevents #. code injection by binding read-eval to nil in macher-agent-execute-ptc-script"
              (let* ((ctx (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/mock/ptc/") :contents nil))
                     (injected nil)
                     (success-result nil)
                     (error-result nil)
                     (script "#.(setq injected t)"))
                (macher-agent-execute-ptc-script
                 script
                 ctx
                 (lambda (val) (setq success-result val))
                 (lambda (err) (setq error-result err)))
                (expect injected :to-be nil)
                (expect error-result :not :to-be nil)))

          (it "evaluates iterative apply, condition-case, unwind-protect, catch, and throw special forms"
              ;; Test apply
              (expect (macher-agent-sandbox-run '(apply #'+ '(1 2 3)) '(+)) :to-equal 6)
              ;; Test condition-case caught error
              (expect (macher-agent-sandbox-run
                       '(condition-case err
                            (/ 1 0)
                          (error (car err)))
                       '(/))
                      :to-equal 'arith-error)
              ;; Test condition-case normal path
              (expect (macher-agent-sandbox-run
                       '(condition-case err
                            (+ 10 5)
                          (error -1))
                       '(+))
                      :to-equal 15)
              ;; Test unwind-protect
              (expect (macher-agent-sandbox-run
                       '(let ((cleanup nil))
                          (unwind-protect
                              (+ 20 30)
                            (setq cleanup t)))
                       '(+))
                      :to-equal 50)
              ;; Test catch and throw
              (expect (macher-agent-sandbox-run
                       '(catch 'done
                          (throw 'done 99)
                          100)
                       nil)
                      :to-equal 99)))

(provide 'macher-agent-ptc-test)
;;; macher-agent-ptc-test.el ends here
