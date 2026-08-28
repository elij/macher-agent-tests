;;; macher-agent-public-api-test.el --- Tests for public API -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'macher-agent-api)
(require 'macher-agent)
(require 'macher-agent-core)
(require 'macher-agent-presets)

(describe "Macher-Agent Public API Suite"

          (describe "API Contract"
                    (it "ensures all public API bridge functions are defined"
                        (expect (fboundp 'macher-agent-context-update) :to-be t)
                        (expect (fboundp 'macher-agent-scope-add-file) :to-be t)
                        (expect (fboundp 'macher-agent-a2a-dispatch) :to-be t)
                        (expect (fboundp 'macher-agent-sandbox-run) :to-be t)
                        (expect (fboundp 'macher-agent-api-register-skills-in-directory) :to-be t)
                        (expect (fboundp 'macher-agent-ui-show) :to-be t)))

          (describe "Sandboxed Evaluator Basic Features"
                    (it "evaluates basic expressions safely"
                        (expect (macher-agent-sandbox-run 42 nil) :to-equal 42)
                        (expect (macher-agent-sandbox-run "test" nil) :to-equal "test")
                        (expect (macher-agent-sandbox-run t nil) :to-equal t)
                        (expect (macher-agent-sandbox-run nil nil) :to-be nil)
                        (expect (macher-agent-sandbox-run '(quote (1 2 3)) nil) :to-equal '(1 2 3))
                        (expect (macher-agent-sandbox-run '(progn 1 2 3) nil) :to-equal 3)
                        (expect (macher-agent-sandbox-run '(if t 'yes 'no) nil) :to-equal 'yes)
                        (expect (macher-agent-sandbox-run '(if nil 'yes 'no) nil) :to-equal 'no)
                        (expect (macher-agent-sandbox-run '(let ((x 10) (y 20)) (progn (setq x 15) (+ x y))) '(+)) :to-equal 35)
                        (expect (macher-agent-sandbox-run '(funcall (lambda (x) (* x x)) 5) '(*)) :to-equal 25)))

          (describe "Sandboxed Evaluator Comprehensive Features"
                    (it "evaluates newly implemented sandboxed features safely"
                        ;; Keyword self-evaluation
                        (expect (macher-agent-sandbox-run ':foo nil) :to-equal :foo)
                        (expect (macher-agent-sandbox-run '(let ((x :bar)) x) nil) :to-equal :bar)

                        ;; Built-in and whitelisted functions
                        (expect (macher-agent-sandbox-run '(not nil) nil) :to-equal t)
                        (expect (macher-agent-sandbox-run '(not t) nil) :to-be nil)
                        (expect (macher-agent-sandbox-run '(reverse '(1 2 3)) nil) :to-equal '(3 2 1))
                        (expect (macher-agent-sandbox-run '(split-string "foo bar" " ") nil) :to-equal '("foo" "bar"))
                        (expect (macher-agent-sandbox-run '(plist-get '(:a 1 :b 2) :b) nil) :to-equal 2)

                        ;; Functional application
                        (expect (macher-agent-sandbox-run '(apply '+ 1 2 '(3 4)) '(+)) :to-equal 10)
                        (expect (macher-agent-sandbox-run '(apply '+ '(1 2 3)) '(+)) :to-equal 6)
                        (expect (macher-agent-sandbox-run '(mapcar (lambda (x) (* x 2)) '(1 2 3)) '(*)) :to-equal '(2 4 6))

                        ;; Control flow and Error handling
                        (expect (macher-agent-sandbox-run '(condition-case err (/ 1 0) (error 'caught)) '(/)) :to-equal 'caught)
                        (expect (macher-agent-sandbox-run '(unwind-protect 1 2) nil) :to-equal 1)
                        (expect (macher-agent-sandbox-run '(catch 'tag (throw 'tag 42)) nil) :to-equal 42)

                        ;; Introspection
                        (expect (macher-agent-sandbox-run '(fboundp 'car) nil) :to-equal t)
                        (expect (macher-agent-sandbox-run '(fboundp 'nonexistent) nil) :to-be nil)
                        (expect (macher-agent-sandbox-run '(let ((x 1)) (boundp 'x)) nil) :to-equal t)
                        (expect (macher-agent-sandbox-run '(boundp 'nonexistent) nil) :to-be nil)

                        ;; Macros
                        (expect (macher-agent-sandbox-run '(progn
                                                             (defalias 'my-when '(macro lambda (cond &rest body)
                                                                                        (list 'if cond (cons 'progn body))))
                                                             (my-when t 42))
                                                          nil)
                                :to-equal 42)))

          (describe "Context Resolution Regression"
                    (before-each
                     (setq macher-agent--persistent-context nil)
                     (clrhash macher-agent-active-workspaces))

                    (it "short-circuits waterfall pipeline when explicit context is passed"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/explicit-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))
                          (expect (macher-agent-resolve-context ctx) :to-be ctx)))

                    (it "extracts context from FSM payload when FSM object is supplied"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/fsm-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (fsm (gptel-make-fsm)))
                          (setf (gptel-fsm-info fsm) (list :macher-agent-context ctx))
                          (expect (macher-agent-resolve-context fsm) :to-be ctx)))

                    (it "resolves context cleanly via workspace identifiers and direct lookup"
                        (let* ((macher-agent-active-workspaces (make-hash-table :test 'equal))
                               (root-dir "/mock/ws-registry-proj/")
                               (sub-dir "/mock/ws-registry-proj/nested/dir/")
                               (ws (make-macher-agent-workspace :project-root root-dir))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (debug-lookups nil))
                          (puthash (expand-file-name root-dir) ctx macher-agent-active-workspaces)

                          (push (macher-agent-context-lookup root-dir) debug-lookups)
                          (push (macher-agent-context-lookup sub-dir) debug-lookups)
                          (push (macher-agent-context-lookup ws) debug-lookups)
                          (push (macher-agent-context-lookup (cons 'agent root-dir)) debug-lookups)
                          
                          (push (macher-agent-resolve-context root-dir) debug-lookups)
                          (push (macher-agent-resolve-context sub-dir) debug-lookups)
                          (push (macher-agent-resolve-context (list :workspace-id sub-dir)) debug-lookups)
                          (push (macher-agent-resolve-context (list :workspace ws)) debug-lookups)

                          ;; Assert all 8 resolution strategies successfully returned the context
                          (dolist (res debug-lookups)
                            (expect res :to-be ctx))))

                    (it "returns nil when context resolution fails across all steps"
                        (let ((macher-agent-active-workspaces (make-hash-table :test 'equal))
                              (macher-agent--persistent-context nil))
                          (spy-on 'macher-agent--resolve-context-lazy-init :and-return-value nil)
                          (expect (macher-agent-resolve-context) :to-be nil)))

                    (it "reads and updates files via context API functions"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/api-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (file "src/main.el"))
                          (macher-agent-context-update ctx file "(message \"hello\")")
                          (expect (macher-agent--read-context-file ctx file) :to-equal "(message \"hello\")")
                          (macher-agent-context-update ctx file "(message \"updated\")")
                          (expect (macher-agent--read-context-file ctx file) :to-equal "(message \"updated\")"))))

          (describe "Strict VFS Pipeline Regression"

                    (it "executes pipeline steps in strict sequential order"
                        (let* ((step-order nil)
                               (ws (make-macher-agent-workspace :project-root "/mock/vfs-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents '(("mock-file.txt" . "content")))))
                          (spy-on 'macher-agent--vfs-verify-clean-merge :and-call-fake (lambda (&rest _) (push 'verify step-order)))
                          (spy-on 'macher-agent--vfs-sync-baseline :and-call-fake (lambda (&rest _) (push 'sync step-order)))
                          (spy-on 'macher-agent-context-root :and-return-value "/mock/vfs-proj/")
                          (spy-on 'delete-directory :and-call-through)

                          (macher-agent-call-with-strict-vfs-pipeline ctx
                                                                      (lambda () 'done))

                          (expect (reverse step-order) :to-equal '(verify sync))))

                    (it "binds default-directory to isolated temporary sandbox directory during body execution"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/vfs-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (captured-dir nil))
                          (spy-on 'macher-agent--vfs-verify-clean-merge)
                          (spy-on 'macher-agent--vfs-sync-baseline)
                          (spy-on 'macher-agent-context-root :and-return-value "/mock/vfs-proj/")

                          (macher-agent-call-with-strict-vfs-pipeline ctx
                                                                      (lambda () (setq captured-dir default-directory)))

                          (expect captured-dir :not :to-equal "/mock/vfs-proj/")
                          (expect (string-match-p "macher-sandbox-" captured-dir) :to-be-truthy)))

                    (it "guarantees sandbox directory cleanup even when body signals an error"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/vfs-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (created-sandbox nil))
                          (spy-on 'macher-agent--vfs-verify-clean-merge)
                          (spy-on 'macher-agent--vfs-sync-baseline)
                          (spy-on 'macher-agent-context-root :and-return-value "/mock/vfs-proj/")

                          (expect
                           (macher-agent-call-with-strict-vfs-pipeline ctx
                                                                       (lambda ()
                                                                         (setq created-sandbox default-directory)
                                                                         (error "Forced pipeline failure")))
                           :to-throw 'error)

                          (expect created-sandbox :not :to-be nil)
                          (expect (file-exists-p created-sandbox) :to-be nil)))
                    )

          (describe "Workspace Root, Task Flush Hooks, and Module Invariants"

                    (it "resolves workspace root via core workspace-project-root without calling macher--workspace-root"
                        (let ((ws-proj (cons 'project "/mock/workspace/proj/"))
                              (ws-agent (cons 'agent "/mock/workspace/agent/"))
                              (ws-str "/mock/workspace/str/"))
                          (expect (macher-agent-workspace-root ws-proj) :to-equal (file-truename (expand-file-name "/mock/workspace/proj/")))
                          (expect (macher-agent-workspace-root ws-agent) :to-equal (file-truename (expand-file-name "/mock/workspace/agent/")))
                          (expect (macher-agent-workspace-root ws-str) :to-equal (file-truename (expand-file-name "/mock/workspace/str/")))
                          (expect (macher-agent-workspace-root nil) :to-be nil)))

                    (it "resolves context workspace root via macher-agent-context-workspace-root"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/context-ws/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))
                          (expect (macher-agent-context-workspace-root ctx) :to-equal (file-truename (expand-file-name "/mock/context-ws/")))))

                    (it "triggers task flush hook in force-review with explicit context"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/force-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (flushed-ctx nil))
                          (spy-on 'macher-agent-run-task-flush-hook :and-call-fake (lambda (c) (setq flushed-ctx c)))
                          (macher-agent-force-review ctx)
                          (expect 'macher-agent-run-task-flush-hook :to-have-been-called)
                          (expect flushed-ctx :to-be ctx)))

                    (it "triggers task flush hook in force-review resolving active context"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/force-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (flushed-ctx nil))
                          (spy-on 'macher-agent-resolve-context :and-return-value ctx)
                          (spy-on 'macher-agent-run-task-flush-hook :and-call-fake (lambda (c) (setq flushed-ctx c)))
                          (macher-agent-force-review)
                          (expect 'macher-agent-run-task-flush-hook :to-have-been-called)
                          (expect flushed-ctx :to-be ctx)))

                    (it "contains no internal declare-function forms in macher-agent-api.el and does not require optional plugins"
                        (let* ((api-file (or (locate-library "macher-agent-api.el")
                                             (expand-file-name "macher-agent-api.el" default-directory)))
                               (forms nil))
                          (with-temp-buffer
                            (insert-file-contents api-file)
                            (goto-char (point-min))
                            (condition-case nil
                                (while t
                                  (push (read (current-buffer)) forms))
                              (end-of-file nil)))
                          (let ((internal-declares
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form)
                                         (eq (car form) 'declare-function)
                                         (let ((fn (cadr form)))
                                           (when (and (consp fn) (eq (car fn) 'quote))
                                             (setq fn (cadr fn)))
                                           (and (symbolp fn)
                                                (string-prefix-p "macher-agent-" (symbol-name fn))))))
                                  forms))
                                (requires
                                 (mapcar (lambda (form)
                                           (let ((feat (cadr form)))
                                             (if (and (consp feat) (eq (car feat) 'quote))
                                                 (cadr feat)
                                               feat)))
                                         (cl-remove-if-not
                                          (lambda (form)
                                            (and (consp form)
                                                 (eq (car form) 'require)))
                                          forms))))
                            (expect internal-declares :to-equal nil)
                            (expect (or (member 'macher-agent-core requires) (member ''macher-agent-core requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-gptel requires) (member ''macher-agent-gptel requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-orchestration requires) (member ''macher-agent-orchestration requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-presets requires) (member ''macher-agent-presets requires)) :to-be-truthy)
                            (expect (member 'macher-agent-sandbox requires) :to-be nil)
                            (expect (member ''macher-agent-sandbox requires) :to-be nil)
                            (expect (member 'macher-agent-vfs requires) :to-be nil)
                            (expect (member ''macher-agent-vfs requires) :to-be nil)
                            (expect (member 'macher-agent-macher requires) :to-be nil)
                            (expect (member ''macher-agent-macher requires) :to-be nil))))

                    (it "contains no internal declare-function forms in macher-agent-gptel.el and requires internal modules directly"
                        (let* ((gptel-file (or (locate-library "macher-agent-gptel.el")
                                               (expand-file-name "macher-agent-gptel.el" default-directory)))
                               (forms nil))
                          (with-temp-buffer
                            (insert-file-contents gptel-file)
                            (goto-char (point-min))
                            (condition-case nil
                                (while t
                                  (push (read (current-buffer)) forms))
                              (end-of-file nil)))
                          (let ((internal-declares
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form)
                                         (eq (car form) 'declare-function)
                                         (let ((fn (cadr form)))
                                           (when (and (consp fn) (eq (car fn) 'quote))
                                             (setq fn (cadr fn)))
                                           (and (symbolp fn)
                                                (string-prefix-p "macher-agent-" (symbol-name fn))))))
                                  forms))
                                (requires
                                 (mapcar (lambda (form)
                                           (let ((feat (cadr form)))
                                             (if (and (consp feat) (eq (car feat) 'quote))
                                                 (cadr feat)
                                               feat)))
                                         (cl-remove-if-not
                                          (lambda (form)
                                            (and (consp form)
                                                 (eq (car form) 'require)))
                                          forms))))
                            (expect internal-declares :to-equal nil)
                            (expect (or (member 'macher-agent-core requires) (member ''macher-agent-core requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-presets requires) (member ''macher-agent-presets requires)) :to-be-truthy)
                            (expect (member 'macher-agent-zero-mem requires) :to-be nil)
                            (expect (member ''macher-agent-zero-mem requires) :to-be nil))))

                    (it "contains no internal declare-function forms in macher-agent-tools.el and requires internal modules directly"
                        (let* ((tools-file (or (locate-library "macher-agent-tools.el")
                                               (expand-file-name "macher-agent-tools.el" default-directory)))
                               (forms nil))
                          (with-temp-buffer
                            (insert-file-contents tools-file)
                            (goto-char (point-min))
                            (condition-case nil
                                (while t
                                  (push (read (current-buffer)) forms))
                              (end-of-file nil)))
                          (let ((internal-declares
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form)
                                         (eq (car form) 'declare-function)
                                         (let ((fn (cadr form)))
                                           (when (and (consp fn) (eq (car fn) 'quote))
                                             (setq fn (cadr fn)))
                                           (and (symbolp fn)
                                                (string-prefix-p "macher-agent-" (symbol-name fn))))))
                                  forms))
                                (requires
                                 (mapcar (lambda (form)
                                           (let ((feat (cadr form)))
                                             (if (and (consp feat) (eq (car feat) 'quote))
                                                 (cadr feat)
                                               feat)))
                                         (cl-remove-if-not
                                          (lambda (form)
                                            (and (consp form)
                                                 (eq (car form) 'require)))
                                          forms))))
                            (expect internal-declares :to-equal nil)
                            (expect (or (member 'macher-agent-core requires) (member ''macher-agent-core requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-gptel requires) (member ''macher-agent-gptel requires)) :to-be-truthy))))

                    (it "contains no internal declare-function forms in macher-agent-presets.el and requires internal modules directly"
                        (let* ((presets-file (or (locate-library "macher-agent-presets.el")
                                                 (expand-file-name "macher-agent-presets.el" default-directory)))
                               (forms nil))
                          (with-temp-buffer
                            (insert-file-contents presets-file)
                            (goto-char (point-min))
                            (condition-case nil
                                (while t
                                  (push (read (current-buffer)) forms))
                              (end-of-file nil)))
                          (let ((internal-declares
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form)
                                         (eq (car form) 'declare-function)
                                         (let ((fn (cadr form)))
                                           (when (and (consp fn) (eq (car fn) 'quote))
                                             (setq fn (cadr fn)))
                                           (and (symbolp fn)
                                                (string-prefix-p "macher-agent-" (symbol-name fn))))))
                                  forms))
                                (requires
                                 (mapcar (lambda (form)
                                           (let ((feat (cadr form)))
                                             (if (and (consp feat) (eq (car feat) 'quote))
                                                 (cadr feat)
                                               feat)))
                                         (cl-remove-if-not
                                          (lambda (form)
                                            (and (consp form)
                                                 (eq (car form) 'require)))
                                          forms))))
                            (expect internal-declares :to-equal nil)
                            (expect (or (member 'macher-agent-core requires) (member ''macher-agent-core requires)) :to-be-truthy))))

                    (it "contains no internal declare-function forms in macher-agent-zero-mem.el and requires internal modules directly"
                        (let* ((zm-file (or (locate-library "macher-agent-zero-mem.el")
                                            (expand-file-name "macher-agent-zero-mem.el" default-directory)))
                               (forms nil))
                          (with-temp-buffer
                            (insert-file-contents zm-file)
                            (goto-char (point-min))
                            (condition-case nil
                                (while t
                                  (push (read (current-buffer)) forms))
                              (end-of-file nil)))
                          (let ((internal-declares
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form)
                                         (eq (car form) 'declare-function)
                                         (let ((fn (cadr form)))
                                           (when (and (consp fn) (eq (car fn) 'quote))
                                             (setq fn (cadr fn)))
                                           (and (symbolp fn)
                                                (string-prefix-p "macher-agent-" (symbol-name fn))))))
                                  forms))
                                (requires
                                 (mapcar (lambda (form)
                                           (let ((feat (cadr form)))
                                             (if (and (consp feat) (eq (car feat) 'quote))
                                                 (cadr feat)
                                               feat)))
                                         (cl-remove-if-not
                                          (lambda (form)
                                            (and (consp form)
                                                 (eq (car form) 'require)))
                                          forms))))
                            (expect internal-declares :to-equal nil)
                            (expect (or (member 'macher-agent-core requires) (member ''macher-agent-core requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-gptel requires) (member ''macher-agent-gptel requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-tools requires) (member ''macher-agent-tools requires)) :to-be-truthy))))

                    (it "contains no internal declare-function forms in macher-agent-orchestration.el and requires internal modules directly"
                        (let* ((orch-file (or (locate-library "macher-agent-orchestration.el")
                                              (expand-file-name "macher-agent-orchestration.el" default-directory)))
                               (forms nil)
                               (file-str nil))
                          (with-temp-buffer
                            (insert-file-contents orch-file)
                            (setq file-str (buffer-string))
                            (goto-char (point-min))
                            (condition-case nil
                                (while t
                                  (push (read (current-buffer)) forms))
                              (end-of-file nil)))
                          (let ((internal-declares
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form)
                                         (eq (car form) 'declare-function)
                                         (let ((fn (cadr form)))
                                           (when (and (consp fn) (eq (car fn) 'quote))
                                             (setq fn (cadr fn)))
                                           (and (symbolp fn)
                                                (string-prefix-p "macher-agent-" (symbol-name fn))))))
                                  forms))
                                (requires
                                 (mapcar (lambda (form)
                                           (let ((feat (cadr form)))
                                             (if (and (consp feat) (eq (car feat) 'quote))
                                                 (cadr feat)
                                               feat)))
                                         (cl-remove-if-not
                                          (lambda (form)
                                            (and (consp form)
                                                 (eq (car form) 'require)))
                                          forms))))
                            (expect internal-declares :to-equal nil)
                            (expect (or (member 'macher-agent-core requires) (member ''macher-agent-core requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-gptel requires) (member ''macher-agent-gptel requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-tools requires) (member ''macher-agent-tools requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-presets requires) (member ''macher-agent-presets requires)) :to-be-truthy)
                            (expect (member 'macher-agent-sandbox requires) :to-be nil)
                            (expect (member ''macher-agent-sandbox requires) :to-be nil)
                            (expect (member 'macher-agent-vfs requires) :to-be nil)
                            (expect (member ''macher-agent-vfs requires) :to-be nil)
                            (expect (member 'macher-agent-macher requires) :to-be nil)
                            (expect (member ''macher-agent-macher requires) :to-be nil)
                            (expect (string-match-p "macher-agent-vfs" file-str) :to-be nil))))

                    (it "contains no internal declare-function forms in macher-agent-sandbox.el and requires internal modules directly"
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
                          (let ((internal-declares
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form)
                                         (eq (car form) 'declare-function)
                                         (let ((fn (cadr form)))
                                           (when (and (consp fn) (eq (car fn) 'quote))
                                             (setq fn (cadr fn)))
                                           (and (symbolp fn)
                                                (string-prefix-p "macher-agent-" (symbol-name fn))))))
                                  forms))
                                (requires
                                 (mapcar (lambda (form)
                                           (let ((feat (cadr form)))
                                             (if (and (consp feat) (eq (car feat) 'quote))
                                                 (cadr feat)
                                               feat)))
                                         (cl-remove-if-not
                                          (lambda (form)
                                            (and (consp form)
                                                 (eq (car form) 'require)))
                                          forms))))
                            (expect internal-declares :to-equal nil)
                            (expect (or (member 'macher-agent-core requires) (member ''macher-agent-core requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-tools requires) (member ''macher-agent-tools requires)) :to-be-truthy))))

                    (it "contains no internal declare-function forms in macher-agent.el and requires internal modules directly"
                        (let* ((main-file (or (locate-library "macher-agent.el")
                                              (expand-file-name "macher-agent.el" default-directory)))
                               (forms nil))
                          (with-temp-buffer
                            (insert-file-contents main-file)
                            (goto-char (point-min))
                            (condition-case nil
                                (while t
                                  (push (read (current-buffer)) forms))
                              (end-of-file nil)))
                          (let ((internal-declares
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form)
                                         (eq (car form) 'declare-function)
                                         (let ((fn (cadr form)))
                                           (when (and (consp fn) (eq (car fn) 'quote))
                                             (setq fn (cadr fn)))
                                           (and (symbolp fn)
                                                (string-prefix-p "macher-agent-" (symbol-name fn))))))
                                  forms))
                                (requires
                                 (mapcar (lambda (form)
                                           (let ((feat (cadr form)))
                                             (if (and (consp feat) (eq (car feat) 'quote))
                                                 (cadr feat)
                                               feat)))
                                         (cl-remove-if-not
                                          (lambda (form)
                                            (and (consp form)
                                                 (eq (car form) 'require)))
                                          forms))))
                            (expect internal-declares :to-equal nil)
                            (expect (or (member 'macher-agent-core requires) (member ''macher-agent-core requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-presets requires) (member ''macher-agent-presets requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-gptel requires) (member ''macher-agent-gptel requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-tools requires) (member ''macher-agent-tools requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-sandbox requires) (member ''macher-agent-sandbox requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-orchestration requires) (member ''macher-agent-orchestration requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-api requires) (member ''macher-agent-api requires)) :to-be-truthy)
                            (expect (member 'macher-agent-vfs requires) :to-be nil)
                            (expect (member ''macher-agent-vfs requires) :to-be nil)
                            (expect (member 'macher-agent-macher requires) :to-be nil)
                            (expect (member ''macher-agent-macher requires) :to-be nil))))

                    (it "contains no internal declare-function forms in macher-agent-vfs.el and requires internal modules directly"
                        (let* ((vfs-file (or (locate-library "macher-agent-vfs.el")
                                             (expand-file-name "macher-agent-vfs.el" default-directory)))
                               (forms nil))
                          (with-temp-buffer
                            (insert-file-contents vfs-file)
                            (goto-char (point-min))
                            (condition-case nil
                                (while t
                                  (push (read (current-buffer)) forms))
                              (end-of-file nil)))
                          (let ((internal-declares
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form)
                                         (eq (car form) 'declare-function)
                                         (let ((fn (cadr form)))
                                           (when (and (consp fn) (eq (car fn) 'quote))
                                             (setq fn (cadr fn)))
                                           (and (symbolp fn)
                                                (string-prefix-p "macher-agent-" (symbol-name fn))))))
                                  forms))
                                (all-declares
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form) (eq (car form) 'declare-function)))
                                  forms))
                                (requires
                                 (mapcar (lambda (form)
                                           (let ((feat (cadr form)))
                                             (if (and (consp feat) (eq (car feat) 'quote))
                                                 (cadr feat)
                                               feat)))
                                         (cl-remove-if-not
                                          (lambda (form)
                                            (and (consp form)
                                                 (eq (car form) 'require)))
                                          forms))))
                            (expect internal-declares :to-equal nil)
                            ;; Verify all declare-functions target external libraries only
                            (dolist (dec all-declares)
                              (let ((target-file (caddr dec)))
                                (when (and (consp target-file) (eq (car target-file) 'quote))
                                  (setq target-file (cadr target-file)))
                                (expect (member target-file '("gptel" "mailcap" "macher" gptel mailcap macher))
                                        :to-be-truthy)))
                            (expect (or (member 'macher-agent-core requires) (member ''macher-agent-core requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-macher requires) (member ''macher-agent-macher requires)) :to-be-truthy)))))

          (describe "Buffer Scope and Instruction Preparation"
                    (it "resolves buffer path via macher-agent-workspace-resolve-path"
                        (expect (macher-agent-workspace-resolve-path "foo/bar.el")
                                :to-equal (macher-agent--resolve-buffer-name "foo/bar.el")))

                    (it "adds buffer contents to scope in context via macher-agent-scope-add-file"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))
                          (with-temp-buffer
                            (rename-buffer "*mock-scope-buf*" t)
                            (insert "buffer sample content")
                            (macher-agent-scope-add-file (buffer-name) ctx)
                            (expect (macher-agent-context-read ctx (buffer-name))
                                    :to-equal "buffer sample content"))))

                    (it "prepares sub-agent buffer instructions and hooks via macher-agent-prepare-instructions"
                        (with-temp-buffer
                          (let ((buf (current-buffer)))
                            (macher-agent-prepare-instructions buf "You are a subagent." 'elisp-expert)
                            (expect (buffer-string) :to-equal "You are a subagent.")
                            (expect macher-agent-presets :to-equal '(elisp-expert)))))

                    (it "logs tool execution intents cleanly in context audit log"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/audit-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))
                          (macher-agent-log-tool-intent ctx "gptel-tool" "read_file" '(:path "foo.el"))
                          (let ((log (macher-agent--get-context-data ctx :audit-log)))
                            (expect (length log) :to-equal 1)
                            (expect (alist-get 'target (car log)) :to-equal "read_file")
                            (expect (alist-get 'type (car log)) :to-equal "gptel-tool")))))

          (describe "Decoupled Virtual Buffer Application"
                    (it "applies virtual edits from 3-tuple cons cells (path orig . curr) to live buffers"
                        (let ((buf (get-buffer-create "*mock-orch-target-1*")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf
                                  (erase-buffer)
                                  (insert "initial text"))
                                (let ((applied (macher-agent--apply-single-virtual-buffer '("*mock-orch-target-1*" "initial text" . "updated tuple text"))))
                                  (expect applied :to-be-truthy)
                                  (expect (with-current-buffer buf (buffer-string)) :to-equal "updated tuple text")))
                            (when (buffer-live-p buf) (kill-buffer buf)))))

                    (it "applies virtual edits from pair cons cells (path . curr) to live buffers"
                        (let ((buf (get-buffer-create "*mock-orch-target-2*")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf
                                  (erase-buffer)
                                  (insert "initial text"))
                                (let ((applied (macher-agent--apply-single-virtual-buffer '("*mock-orch-target-2*" . "updated pair text"))))
                                  (expect applied :to-be-truthy)
                                  (expect (with-current-buffer buf (buffer-string)) :to-equal "updated pair text")))
                            (when (buffer-live-p buf) (kill-buffer buf)))))

                    (it "applies virtual edits from plist structures (:path / :curr) to live buffers"
                        (let ((buf (get-buffer-create "*mock-orch-target-3*")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf
                                  (erase-buffer)
                                  (insert "initial text"))
                                (let ((applied (macher-agent--apply-single-virtual-buffer '(:path "*mock-orch-target-3*" :curr "updated plist text"))))
                                  (expect applied :to-be-truthy)
                                  (expect (with-current-buffer buf (buffer-string)) :to-equal "updated plist text")))
                            (when (buffer-live-p buf) (kill-buffer buf)))))

                    (it "applies virtual edits from plist structures (:file / :content) to live buffers"
                        (let ((buf (get-buffer-create "*mock-orch-target-4*")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf
                                  (erase-buffer)
                                  (insert "initial text"))
                                (let ((applied (macher-agent--apply-single-virtual-buffer '(:file "*mock-orch-target-4*" :content "updated file plist text"))))
                                  (expect applied :to-be-truthy)
                                  (expect (with-current-buffer buf (buffer-string)) :to-equal "updated file plist text")))
                            (when (buffer-live-p buf) (kill-buffer buf)))))

                    (it "applies virtual edits from 2-element list (path curr) to live buffers"
                        (let ((buf (get-buffer-create "*mock-orch-target-5*")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf
                                  (erase-buffer)
                                  (insert "initial text"))
                                (let ((applied (macher-agent--apply-single-virtual-buffer '("*mock-orch-target-5*" "updated 2-list text"))))
                                  (expect applied :to-be-truthy)
                                  (expect (with-current-buffer buf (buffer-string)) :to-equal "updated 2-list text")))
                            (when (buffer-live-p buf) (kill-buffer buf)))))

                    (it "applies virtual edits from plist structures (:path / :contents) to live buffers"
                        (let ((buf (get-buffer-create "*mock-orch-target-6*")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf
                                  (erase-buffer)
                                  (insert "initial text"))
                                (let ((applied (macher-agent--apply-single-virtual-buffer '(:path "*mock-orch-target-6*" :contents "updated contents plist text"))))
                                  (expect applied :to-be-truthy)
                                  (expect (with-current-buffer buf (buffer-string)) :to-equal "updated contents plist text")))
                            (when (buffer-live-p buf) (kill-buffer buf)))))

                    (it "returns nil gracefully for nil or invalid entries or dead buffers"
                        (expect (macher-agent--apply-single-virtual-buffer nil) :to-be nil)
                        (expect (macher-agent--apply-single-virtual-buffer '("*nonexistent-buf*" . "some content")) :to-be nil)
                        (expect (macher-agent--apply-single-virtual-buffer '(:path nil :curr "content")) :to-be nil)
                        (expect (macher-agent--apply-single-virtual-buffer '(:file 123 :curr "content")) :to-be nil))

                    (it "applies batch virtual buffer entries via macher-agent-apply-virtual-buffers"
                        (let ((buf1 (get-buffer-create "*mock-batch-1*"))
                              (buf2 (get-buffer-create "*mock-batch-2*"))
                              (buf3 (get-buffer-create "*mock-batch-3*")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf1 (erase-buffer) (insert "old 1"))
                                (with-current-buffer buf2 (erase-buffer) (insert "old 2"))
                                (with-current-buffer buf3 (erase-buffer) (insert "old 3"))
                                (let* ((ws (make-macher-agent-workspace :project-root "/mock/batch/"))
                                       (ctx (macher-agent--make-vfs-context
                                             :workspace ws
                                             :contents (list '("*mock-batch-1*" "old 1" . "new batch 1")
                                                             '(:path "*mock-batch-2*" :curr "new batch 2")
                                                             '(:file "*mock-batch-3*" :content "new batch 3")))))
                                  (macher-agent-apply-virtual-buffers ctx)
                                  (expect (with-current-buffer buf1 (buffer-string)) :to-equal "new batch 1")
                                  (expect (with-current-buffer buf2 (buffer-string)) :to-equal "new batch 2")
                                  (expect (with-current-buffer buf3 (buffer-string)) :to-equal "new batch 3")))
                            (when (buffer-live-p buf1) (kill-buffer buf1))
                            (when (buffer-live-p buf2) (kill-buffer buf2))
                            (when (buffer-live-p buf3) (kill-buffer buf3)))))

                    (it "normalises context entries to clean alist and trims content right in macher-agent-apply-virtual-buffers"
                        (let ((buf1 (get-buffer-create "*mock-norm-1*"))
                              (buf2 (get-buffer-create "*mock-norm-2*"))
                              (buf3 (get-buffer-create "*mock-norm-3*"))
                              (buf4 (get-buffer-create "*mock-norm-4*")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf1 (erase-buffer) (insert "init 1"))
                                (with-current-buffer buf2 (erase-buffer) (insert "init 2"))
                                (with-current-buffer buf3 (erase-buffer) (insert "init 3"))
                                (with-current-buffer buf4 (erase-buffer) (insert "init 4"))
                                (let* ((ws (make-macher-agent-workspace :project-root "/mock/norm/"))
                                       (ctx (macher-agent--make-vfs-context
                                             :workspace ws
                                             :contents (list '(:path "*mock-norm-1*" :curr "norm content 1\n\n  ")
                                                             '(:file "*mock-norm-2*" :content "norm content 2 \t\n")
                                                             '(:path "*mock-norm-3*" :contents "norm content 3\r\n")
                                                             '("*mock-norm-4*" . "norm content 4\n")))))
                                  (macher-agent-apply-virtual-buffers ctx)
                                  ;; Buffers receive trimmed content
                                  (expect (with-current-buffer buf1 (buffer-string)) :to-equal "norm content 1")
                                  (expect (with-current-buffer buf2 (buffer-string)) :to-equal "norm content 2")
                                  (expect (with-current-buffer buf3 (buffer-string)) :to-equal "norm content 3")
                                  (expect (with-current-buffer buf4 (buffer-string)) :to-equal "norm content 4")
                                  ;; Context contents is converted to cons pairs, never remaining plists
                                  (let ((updated-contents (macher-agent--get-context-contents ctx)))
                                    (expect updated-contents :to-equal '(("*mock-norm-1*" "norm content 1" . "norm content 1")
                                                                         ("*mock-norm-2*" "norm content 2" . "norm content 2")
                                                                         ("*mock-norm-3*" "norm content 3" . "norm content 3")
                                                                         ("*mock-norm-4*" "norm content 4" . "norm content 4")))
                                    (dolist (entry updated-contents)
                                      (expect (consp entry) :to-be-truthy)
                                      (expect (keywordp (car entry)) :to-be nil)
                                      (expect (stringp (car entry)) :to-be-truthy)
                                      (expect (stringp (macher-agent-vfs-entry-curr entry)) :to-be-truthy)))))
                            (when (buffer-live-p buf1) (kill-buffer buf1))
                            (when (buffer-live-p buf2) (kill-buffer buf2))
                            (when (buffer-live-p buf3) (kill-buffer buf3))
                            (when (buffer-live-p buf4) (kill-buffer buf4)))))))

(provide 'macher-agent-public-api-test)
;;; macher-agent-public-api-test.el ends here
