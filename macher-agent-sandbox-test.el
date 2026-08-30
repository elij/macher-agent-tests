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
    (it "receives context directly from execution payload or FSM info plist without global workspace searches"
      (let* ((mock-ctx (macher-agent--make-context :id "ctx-ptc-direct" :project-root "/mock/direct-ptc/"))
             (fsm (gptel-make-fsm :info (list :buffer (current-buffer) :macher-agent-context mock-ctx)))
             (macher-agent--active-fsm fsm)
             (executed-ctx nil)
             (success-called nil)
             (error-called nil))
        (clrhash macher-agent-active-workspaces)
        (setq-local macher-agent--persistent-context nil)

        (macher-agent-execute-ptc-script
         "(+ 10 20)"
         nil
         (lambda (val)
           (setq success-called val)
           (setq executed-ctx (bound-and-true-p macher-agent--persistent-context)))
         (lambda (err) (setq error-called err)))

        (expect error-called :to-be nil)
        (expect success-called :to-equal 30)
        (expect executed-ctx :to-be mock-ctx)))

    (it "resolves context directly from active FSM info plist during tool execution inside sandbox"
      (let* ((mock-ctx (macher-agent--make-context :id "ctx-fsm-tool" :project-root "/mock/fsm-tool/"))
             (fsm (gptel-make-fsm :info (list :buffer (current-buffer)
                                              :macher-agent-context mock-ctx
                                              :ptc-primitives '(test-ctx-tool))))
             (macher-agent--active-fsm fsm)
             (captured-ctx nil)
             (success-res nil)
             (err-res nil)
             (mock-tool (macher-agent-make-tool mock-sandbox-ctx-tool
                            "Test tool capturing context"
                          :category "test"
                          :args '((:name "param" :type string))
                          :command-fn (lambda (_payload context _root)
                                        (setq captured-ctx context)
                                        "tool-success"))))
        (clrhash macher-agent-active-workspaces)
        (setq-local macher-agent--persistent-context nil)

        (cl-letf (((symbol-function 'macher-agent-resolve-tool)
                   (lambda (_name &rest _args) mock-tool)))
          (macher-agent-execute-ptc-script
           "(test-ctx-tool \"value\")"
           nil
           (lambda (val) (setq success-res val))
           (lambda (err) (setq err-res err))
           '(test-ctx-tool)))

        (expect err-res :to-be nil)
        (expect success-res :to-equal "tool-success")
        (expect captured-ctx :to-be mock-ctx)))

    (it "propagates explicit context argument to sandbox-run without global lookups"
      (let* ((mock-ctx (macher-agent--make-context :id "ctx-run-direct" :project-root "/mock/run/"))
             (logged-ctx nil))
        (clrhash macher-agent-active-workspaces)
        (setq-local macher-agent--persistent-context nil)

        (cl-letf (((symbol-function 'macher-agent-log-tool-intent)
                   (lambda (context _type _target _args)
                     (setq logged-ctx context))))
          (let* ((macher-agent--active-ptc-primitives '(mock-intent-tool))
                 (result (macher-agent-sandbox-run '(mock-intent-tool :foo "bar") nil mock-ctx)))
            (expect logged-ctx :to-be mock-ctx)))))

    (it "extracts ptc primitives directly from FSM info plist"
      (let* ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)
                                              :ptc-primitives '(custom-prim-alpha))))
             (macher-agent--active-fsm fsm))
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
