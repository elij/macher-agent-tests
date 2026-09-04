;;; macher-agent-sandbox-test.el --- Tests for Macher Agent Sandbox and PTC Context Transport -*- lexical-binding: t; -*-

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
(require 'macher-agent-core)
(require 'macher-agent-sandbox)
(require 'macher-agent-gptel)
(require 'macher-agent-tools)

(describe "Macher Agent Sandbox Direct Context Transport"
          (macher-agent-test-setup-before-each)

          (describe "PTC Evaluation Context Resolution"
                    (it "evaluates script with explicit context and target buffer without probing FSM"
                        (let* ((mock-ctx (macher-agent--make-context :id "ctx-ptc-direct" :project-root "/mock/direct-ptc/"))
                               (target-buf (generate-new-buffer " *test-ptc-buf*"))
                               (executed-ctx nil)
                               (success-called nil)
                               (error-called nil))
                          (unwind-protect
                              (progn
                                (macher-agent-execute-ptc-script
                                 "(+ 10 20)"
                                 mock-ctx
                                 target-buf
                                 (lambda (val)
                                   (setq success-called val)
                                   (setq executed-ctx (buffer-local-value 'macher-agent--persistent-context target-buf)))
                                 (lambda (err) (setq error-called err)))

                                (expect error-called :to-be nil)
                                (expect success-called :to-equal 30)
                                (expect executed-ctx :to-be mock-ctx))
                            (when (buffer-live-p target-buf)
                              (kill-buffer target-buf)))))

                    (it "delegates UI spoofing to macher-agent-gptel-spoof-tool-ui in display-ptc-tool-execution"
                        (let* ((mock-ctx (macher-agent--make-context :id "ctx-ui" :project-root "/mock/ui/"))
                               (target-buf (generate-new-buffer " *test-ui-buf*"))
                               (spoofed-buf nil)
                               (spoofed-tool nil))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent-gptel-spoof-tool-ui)
                                         (lambda (buf tool-name)
                                           (setq spoofed-buf buf)
                                           (setq spoofed-tool tool-name)))
                                        ((symbol-function 'macher-agent-resolve-tool)
                                         (lambda (_name &rest _) nil)))
                                (macher-agent--display-ptc-tool-execution 'custom-test-primitive '(:key "val") mock-ctx target-buf)
                                (expect spoofed-buf :to-equal target-buf)
                                (expect spoofed-tool :to-equal "custom_test_primitive"))
                            (when (buffer-live-p target-buf)
                              (kill-buffer target-buf)))))

                    (it "invokes on-error when script execution fails"
                        (let* ((mock-ctx (macher-agent--make-context :id "ctx-err" :project-root "/mock/err/"))
                               (target-buf (generate-new-buffer " *test-err-buf*"))
                               (err-result nil)
                               (success-result nil))
                          (unwind-protect
                              (progn
                                (macher-agent-execute-ptc-script
                                 "(error \"PTC execution failed\")"
                                 mock-ctx
                                 target-buf
                                 (lambda (val) (setq success-result val))
                                 (lambda (err) (setq err-result err)))
                                (expect success-result :to-be nil)
                                (expect (plist-get err-result :status) :to-equal 'error)
                                (expect (string-match-p "PTC execution failed" (plist-get err-result :error)) :to-be-truthy))
                            (when (buffer-live-p target-buf)
                              (kill-buffer target-buf)))))

                    (it "propagates explicit context and target buffer to sandbox-run without global lookups"
                        (let* ((mock-ctx (macher-agent--make-context :id "ctx-run-direct" :project-root "/mock/run/"))
                               (target-buf (generate-new-buffer " *test-run-buf*"))
                               (logged-ctx nil))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent-log-tool-intent)
                                         (lambda (context _type _target _args)
                                           (setq logged-ctx context))))
                                (let* ((macher-agent--active-ptc-primitives '(mock-intent-tool)))
                                  (macher-agent-sandbox-run '(mock-intent-tool :foo "bar") nil mock-ctx target-buf)
                                  (expect logged-ctx :to-be mock-ctx)))
                            (when (buffer-live-p target-buf)
                              (kill-buffer target-buf)))))

                    (it "recognizes ptc primitives from active primitives list without inspecting FSM"
                        (let ((macher-agent--active-ptc-primitives '(custom-prim-alpha)))
                          (expect (macher-agent--ptc-primitive-p 'custom-prim-alpha) :to-be t)
                          (expect (macher-agent--ptc-primitive-p 'custom_prim_alpha) :to-be t)
                          (expect (macher-agent--ptc-primitive-p 'unmatched-primitive) :to-be nil)))

                    (it "contains zero internal declare-function forms targeting macher-agent symbols"
                        (let* ((sandbox-file (or (locate-library "macher-agent-sandbox.el")
                                                 (expand-file-name "macher-agent-sandbox.el" default-directory)))
                               (forms (with-temp-buffer
                                        (insert-file-contents sandbox-file)
                                        (goto-char (point-min))
                                        (let (res)
                                          (condition-case nil
                                              (while t
                                                (let ((form (read (current-buffer))))
                                                  (when (and (consp form) (eq (car form) 'declare-function))
                                                    (push form res))))
                                            (end-of-file (nreverse res)))))))
                          (dolist (decl forms)
                            (let ((fn (cadr decl)))
                              (expect (string-prefix-p "macher-agent-" (symbol-name fn)) :to-be nil)))))

                    (it "requires only upstream modules and contains no downstream requires"
                        (let* ((sandbox-file (or (locate-library "macher-agent-sandbox.el")
                                                 (expand-file-name "macher-agent-sandbox.el" default-directory)))
                               (forms nil))
                          (with-temp-buffer
                            (insert-file-contents sandbox-file)
                            (goto-char (point-min))
                            (condition-case nil
                                (while t
                                  (push (read (current-buffer)) forms))
                              (end-of-file nil)))
                          (let ((requires
                                 (mapcar (lambda (form)
                                           (let ((feat (cadr form)))
                                             (if (and (consp feat) (eq (car feat) 'quote))
                                                 (cadr feat)
                                               feat)))
                                         (cl-remove-if-not
                                          (lambda (form)
                                            (and (consp form) (eq (car form) 'require)))
                                          forms))))
                            ;; Required upstream modules
                            (expect (or (member 'cl-lib requires) (member ''cl-lib requires)) :to-be-truthy)
                            (expect (or (member 'gptel requires) (member ''gptel requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-core requires) (member ''macher-agent-core requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-tools requires) (member ''macher-agent-tools requires)) :to-be-truthy)
                            ;; Downstream modules must not be required
                            (expect (member 'macher-agent-api requires) :to-be nil)
                            (expect (member ''macher-agent-api requires) :to-be nil)
                            (expect (member 'macher-agent-gptel requires) :to-be nil)
                            (expect (member ''macher-agent-gptel requires) :to-be nil)
                            (expect (member 'macher-agent-presets requires) :to-be nil)
                            (expect (member ''macher-agent-presets requires) :to-be nil)
                            (expect (member 'macher-agent-orchestration requires) :to-be nil)
                            (expect (member ''macher-agent-orchestration requires) :to-be nil)
                            (expect (member 'macher-agent-vfs requires) :to-be nil)
                            (expect (member ''macher-agent-vfs requires) :to-be nil)
                            (expect (member 'macher-agent-zero-mem requires) :to-be nil)
                            (expect (member ''macher-agent-zero-mem requires) :to-be nil)
                            (expect (member 'macher-agent-macher requires) :to-be nil)
                            (expect (member ''macher-agent-macher requires) :to-be nil)))))

          (describe "Sandbox Plugin State Accessors"
                    (it "reads and writes sandbox state via direct macher-agent-context-plugins manipulation"
                        (let* ((ctx (macher-agent--make-context :id "ctx-sandbox-state" :project-root "/mock/state/"))
                               (initial-plugins '(:custom-plugin "active" :prompt "Original Prompt"))
                               (sandbox-payload '(:active-primitives (spawn-subagent delegate-tasks) :eval-mode safe)))
                          (setf (macher-agent-context-plugins ctx) (copy-sequence initial-plugins))

                          ;; Initial state should be nil
                          (expect (macher-agent-sandbox-get-state ctx) :to-be nil)
                          (expect (plist-get (macher-agent-context-plugins ctx) :sandbox) :to-be nil)

                          ;; Set sandbox state directly
                          (let ((result (macher-agent-sandbox-set-state ctx sandbox-payload)))
                            (expect result :to-equal sandbox-payload)
                            (expect (macher-agent-sandbox-get-state ctx) :to-equal sandbox-payload)
                            (expect (plist-get (macher-agent-context-plugins ctx) :sandbox) :to-equal sandbox-payload)

                            ;; Verify other plugin keys remain intact
                            (expect (plist-get (macher-agent-context-plugins ctx) :custom-plugin) :to-equal "active")
                            (expect (plist-get (macher-agent-context-plugins ctx) :prompt) :to-equal "Original Prompt"))

                          ;; Update sandbox state to new value
                          (let ((updated-payload '(:active-primitives (read-file write-file) :eval-mode strict)))
                            (macher-agent-sandbox-set-state ctx updated-payload)
                            (expect (macher-agent-sandbox-get-state ctx) :to-equal updated-payload)
                            (expect (plist-get (macher-agent-context-plugins ctx) :sandbox) :to-equal updated-payload))))

                    (it "returns nil gracefully when get-state or set-state is called on non-context objects"
                        (expect (macher-agent-sandbox-get-state nil) :to-be nil)
                        (expect (macher-agent-sandbox-get-state '((:sandbox . :dummy))) :to-be nil)
                        (expect (macher-agent-sandbox-get-state "not-a-context") :to-be nil)
                        (expect (macher-agent-sandbox-set-state nil '(:active-primitives ())) :to-be nil)
                        (expect (macher-agent-sandbox-set-state '((:sandbox . :dummy)) '(:active-primitives ())) :to-be nil)
                        (expect (macher-agent-sandbox-set-state "not-a-context" '(:active-primitives ())) :to-be nil))))

(provide 'macher-agent-sandbox-test)
;;; macher-agent-sandbox-test.el ends here
