;;; macher-agent-public-api-test.el --- Tests for public API -*- lexical-binding: t; -*-

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

(require 'buttercup)
(require 'cl-lib)
(require 'macher-agent-test-setup)
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

          (describe "Context Resolution and Typed Contracts"
                    (before-each
                     (setq macher-agent--persistent-context nil)
                     (clrhash macher-agent-active-workspaces))

                    (it "extracts persistent context from live buffer objects and buffer names via macher-agent-context-from-buffer"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/buf-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (buf (generate-new-buffer "*mock-ctx-buf*")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf
                                  (setq-local macher-agent--persistent-context ctx))
                                (expect (macher-agent-context-from-buffer buf) :to-be ctx)
                                (expect (macher-agent-context-from-buffer (buffer-name buf)) :to-be ctx))
                            (when (buffer-live-p buf)
                              (kill-buffer buf)))))

                    (it "extracts context directly or from transit payload via macher-agent-context-from-payload"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/payload-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (payload (make-macher-agent-transit-payload :target-context ctx)))
                          (expect (macher-agent-context-from-payload ctx) :to-be ctx)
                          (expect (macher-agent-context-from-payload payload) :to-be ctx)))

                    (it "reads and updates files via context API functions"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/api-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (file "src/main.el"))
                          (macher-agent-context-update ctx file "(message \"hello\")")
                          (expect (macher-agent--read-context-file ctx file) :to-equal "(message \"hello\")")
                          (macher-agent-context-update ctx file "(message \"updated\")")
                          (expect (macher-agent--read-context-file ctx file) :to-equal "(message \"updated\")"))))

          (describe "Strict VFS Pipeline Regression"

                    (it "executes within strict VFS boundary and triggers flush and restore"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/vfs-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents (list (make-macher-agent-vfs-entry :path "mock-file.txt" :orig "content" :curr "content"))))
                               (flushed nil)
                               (restored nil)
                               (executed nil))
                          (cl-letf (((symbol-function 'macher-agent-vfs-flush)
                                     (lambda (c) (setq flushed c)))
                                    ((symbol-function 'macher-agent-vfs-restore)
                                     (lambda (c) (setq restored c))))
                            (macher-agent-with-strict-vfs ctx
                              (setq executed t))
                            (expect flushed :to-equal ctx)
                            (expect restored :to-equal ctx)
                            (expect executed :to-be t))))

                    (it "guarantees restore execution even when body signals an error"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/vfs-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (restored nil))
                          (cl-letf (((symbol-function 'macher-agent-vfs-restore)
                                     (lambda (c) (setq restored c))))
                            (expect
                             (macher-agent-with-strict-vfs ctx
                               (error "Forced pipeline failure"))
                             :to-throw 'error)
                            (expect restored :to-equal ctx))))

                    (it "bypasses flush and restore when context is inactive or nil"
                        (let ((flushed nil)
                              (restored nil)
                              (res nil))
                          (cl-letf (((symbol-function 'macher-agent-vfs-flush)
                                     (lambda (_) (setq flushed t)))
                                    ((symbol-function 'macher-agent-vfs-restore)
                                     (lambda (_) (setq restored t))))
                            (setq res (macher-agent-with-strict-vfs nil
                                        'non-vfs-result))
                            (expect res :to-equal 'non-vfs-result)
                            (expect flushed :to-be nil)
                            (expect restored :to-be nil)))))

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
                          (setq-local macher-agent--persistent-context ctx)
                          (spy-on 'macher-agent-run-task-flush-hook :and-call-fake (lambda (c) (setq flushed-ctx c)))
                          (macher-agent-force-review)
                          (expect 'macher-agent-run-task-flush-hook :to-have-been-called)
                          (expect flushed-ctx :to-be ctx)))

                    (it "contains no internal declare-function forms in macher-agent-api.el and requires upstream modules directly"
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
                            (expect (or (member 'macher-agent-presets requires) (member ''macher-agent-presets requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-tools requires) (member ''macher-agent-tools requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-sandbox requires) (member ''macher-agent-sandbox requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-vfs requires) (member ''macher-agent-vfs requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-zero-mem requires) (member ''macher-agent-zero-mem requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-orchestration requires) (member ''macher-agent-orchestration requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-macher requires) (member ''macher-agent-macher requires)) :to-be-truthy)
                            (expect (member 'macher-agent requires) :to-be nil)
                            (expect (member ''macher-agent requires) :to-be nil))))

                    (it "does not duplicate macher-agent--allow-gptel-restore in macher-agent-api.el"
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
                          (let ((defvars
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form)
                                         (memq (car form) '(defvar defcustom defconst))
                                         (eq (cadr form) 'macher-agent--allow-gptel-restore)))
                                  forms)))
                            (expect defvars :to-equal nil)
                            (expect (boundp 'macher-agent--allow-gptel-restore) :to-be t))))

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
                            (expect (string-match-p "macher-agent-vfs" file-str) :to-be nil)
                            (expect (string-match-p "defun macher-agent--push-context-to-parent" file-str) :to-be nil)
                            (expect (string-match-p "defun macher-agent--pop-routing" file-str) :to-be nil))))

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
                            (expect (or (member 'cl-lib requires) (member ''cl-lib requires)) :to-be-truthy)
                            (expect (or (member 'gptel requires) (member ''gptel requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-core requires) (member ''macher-agent-core requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-tools requires) (member ''macher-agent-tools requires)) :to-be-truthy)
                            (expect (member 'macher-agent-api requires) :to-be nil)
                            (expect (member ''macher-agent-api requires) :to-be nil)
                            (expect (member 'macher-agent-gptel requires) :to-be nil)
                            (expect (member ''macher-agent-gptel requires) :to-be nil))))

                    (it "contains no internal declare-function forms in macher-agent.el and requires macher-agent-api directly"
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
                            (expect (or (member 'macher-agent-api requires) (member ''macher-agent-api requires)) :to-be-truthy)
                            (expect (member 'macher-agent-core requires) :to-be nil)
                            (expect (member ''macher-agent-core requires) :to-be nil)
                            (expect (member 'macher-agent-presets requires) :to-be nil)
                            (expect (member ''macher-agent-presets requires) :to-be nil)
                            (expect (member 'macher-agent-gptel requires) :to-be nil)
                            (expect (member ''macher-agent-gptel requires) :to-be nil)
                            (expect (member 'macher-agent-tools requires) :to-be nil)
                            (expect (member ''macher-agent-tools requires) :to-be nil)
                            (expect (member 'macher-agent-sandbox requires) :to-be nil)
                            (expect (member ''macher-agent-sandbox requires) :to-be nil)
                            (expect (member 'macher-agent-orchestration requires) :to-be nil)
                            (expect (member ''macher-agent-orchestration requires) :to-be nil)
                            (expect (member 'macher-agent-vfs requires) :to-be nil)
                            (expect (member ''macher-agent-vfs requires) :to-be nil)
                            (expect (member 'macher-agent-macher requires) :to-be nil)
                            (expect (member ''macher-agent-macher requires) :to-be nil)
                            (expect (member 'macher-agent-zero-mem requires) :to-be nil)
                            (expect (member ''macher-agent-zero-mem requires) :to-be nil))))

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
                          (let ((log (plist-get (macher-agent-context-plugins ctx) :audit-log)))
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
                                (let ((applied (macher-agent--apply-single-virtual-buffer (make-macher-agent-vfs-entry :path "*mock-orch-target-1*" :orig "initial text" :curr "updated tuple text"))))
                                  (expect applied :to-be-truthy)
                                  (expect (with-current-buffer buf (buffer-string)) :to-equal "updated tuple text")))
                            (when (buffer-live-p buf) (kill-buffer buf)))))

                    (it "returns nil gracefully for nil or invalid entries or dead buffers in macher-agent--apply-single-virtual-buffer"
                        (expect (macher-agent--apply-single-virtual-buffer nil) :to-be nil)
                        (expect (macher-agent--apply-single-virtual-buffer (make-macher-agent-vfs-entry :path "*nonexistent-buf*" :orig "" :curr "some content")) :to-be nil)
                        (expect (macher-agent--apply-single-virtual-buffer (make-macher-agent-vfs-entry :path "" :orig "" :curr "content")) :to-be nil))

                    (it "applies batch virtual buffer entries via macher-agent-apply-virtual-buffers"
                        (let ((buf1 (get-buffer-create "*mock-batch-1*"))
                              (buf2 (get-buffer-create "*mock-batch-2*"))
                              (buf3 (get-buffer-create "*mock-batch-3*"))
                              (buf4 (get-buffer-create "*mock-batch-4*")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf1 (erase-buffer) (insert "old 1"))
                                (with-current-buffer buf2 (erase-buffer) (insert "old 2"))
                                (with-current-buffer buf3 (erase-buffer) (insert "old 3"))
                                (with-current-buffer buf4 (erase-buffer) (insert "old 4"))
                                (let* ((ws (make-macher-agent-workspace :project-root "/mock/batch/"))
                                       (ctx (macher-agent--make-vfs-context
                                             :workspace ws
                                             :contents (list (make-macher-agent-vfs-entry :path "*mock-batch-1*" :orig "old 1" :curr "new batch 1")
                                                             (make-macher-agent-vfs-entry :path "*mock-batch-2*" :orig nil :curr "new batch 2")
                                                             (make-macher-agent-vfs-entry :path "*mock-batch-3*" :orig nil :curr "new batch 3")
                                                             (make-macher-agent-vfs-entry :path "*mock-batch-4*" :orig nil :curr "new batch 4")))))
                                  (macher-agent-apply-virtual-buffers ctx)
                                  (expect (with-current-buffer buf1 (buffer-string)) :to-equal "new batch 1")
                                  (expect (with-current-buffer buf2 (buffer-string)) :to-equal "new batch 2")
                                  (expect (with-current-buffer buf3 (buffer-string)) :to-equal "new batch 3")
                                  (expect (with-current-buffer buf4 (buffer-string)) :to-equal "new batch 4")))
                            (when (buffer-live-p buf1) (kill-buffer buf1))
                            (when (buffer-live-p buf2) (kill-buffer buf2))
                            (when (buffer-live-p buf3) (kill-buffer buf3))
                            (when (buffer-live-p buf4) (kill-buffer buf4)))))

                    (it "normalises context entries and trims content right in macher-agent-apply-virtual-buffers"
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
                                             :contents (list (make-macher-agent-vfs-entry :path "*mock-norm-1*" :orig nil :curr "norm content 1\n\n  ")
                                                             (make-macher-agent-vfs-entry :path "*mock-norm-2*" :orig nil :curr "norm content 2 \t\n")
                                                             (make-macher-agent-vfs-entry :path "*mock-norm-3*" :orig nil :curr "norm content 3\r\n")
                                                             (make-macher-agent-vfs-entry :path "*mock-norm-4*" :orig nil :curr "norm content 4\n")))))
                                  (macher-agent-apply-virtual-buffers ctx)
                                  ;; Buffers receive trimmed content
                                  (expect (with-current-buffer buf1 (buffer-string)) :to-equal "norm content 1")
                                  (expect (with-current-buffer buf2 (buffer-string)) :to-equal "norm content 2")
                                  (expect (with-current-buffer buf3 (buffer-string)) :to-equal "norm content 3")
                                  (expect (with-current-buffer buf4 (buffer-string)) :to-equal "norm content 4")
                                  ;; Context contents is converted to strict vfs-entry structs
                                  (let ((updated-contents (macher-agent--get-context-contents ctx)))
                                    (expect (length updated-contents) :to-equal 4)
                                    (dolist (entry updated-contents)
                                      (expect (macher-agent-vfs-entry-p entry) :to-be t)
                                      (expect (stringp (macher-agent-vfs-entry-path entry)) :to-be t)
                                      (expect (stringp (macher-agent-vfs-entry-curr entry)) :to-be t)))))
                            (when (buffer-live-p buf1) (kill-buffer buf1))
                            (when (buffer-live-p buf2) (kill-buffer buf2))
                            (when (buffer-live-p buf3) (kill-buffer buf3))
                            (when (buffer-live-p buf4) (kill-buffer buf4)))))))

(provide 'macher-agent-public-api-test)
;;; macher-agent-public-api-test.el ends here
