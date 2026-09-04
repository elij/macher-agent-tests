;;; macher-agent-api-test.el --- Tests for macher-agent-api -*- lexical-binding: t; -*-

(let* ((file (or load-file-name buffer-file-name))
       (test-dir (cond
                  (file (file-name-directory (expand-file-name file)))
                  ((file-exists-p (expand-file-name "macher-agent-test-setup.el" default-directory))
                   default-directory)
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
(require 'macher-agent-core)
(require 'macher-agent-api)
(require 'macher-agent-gptel)
(require 'macher-agent-presets)
(require 'macher-agent-tools)
(require 'macher-agent-sandbox)
(require 'macher-agent-vfs)
(require 'macher-agent-zero-mem)
(require 'macher-agent-orchestration)

(describe "macher-agent-api module"

  (macher-agent-test-setup-before-each)

  (describe "macher-agent-log-tool-intent"

    (it "appends audit log entries directly onto the context structure plugins plist"
      (let* ((ctx (macher-agent--make-context :id "test-audit-ctx" :plugins '(:preserved "val")))
             (res (macher-agent-log-tool-intent ctx "gptel-tool" "read_file" '(:path "test.txt"))))
        (expect (listp res) :to-be t)
        (expect (length res) :to-equal 1)
        (let* ((plugins (macher-agent-context-plugins ctx))
               (audit-log (plist-get plugins :audit-log))
               (entry (car audit-log)))
          (expect (plist-get plugins :preserved) :to-equal "val")
          (expect (length audit-log) :to-equal 1)
          (expect (cdr (assoc 'type entry)) :to-equal "gptel-tool")
          (expect (cdr (assoc 'target entry)) :to-equal "read_file")
          (expect (cdr (assoc 'args entry)) :to-equal '(:path "test.txt"))
          (expect (cdr (assoc 'buffer entry)) :to-equal (buffer-name))
          (expect (cdr (assoc 'preset entry)) :to-equal "unknown")
          (expect (stringp (cdr (assoc 'timestamp entry))) :to-be t))))

    (it "captures the active skill preset symbol when bound"
      (let ((ctx (macher-agent--make-context :id "preset-audit-ctx"))
            (macher-agent--active-skill-sym 'expert-reviewer))
        (macher-agent-log-tool-intent ctx "ptc" 'spawn-subagent '(:name "worker-1"))
        (let* ((log (plist-get (macher-agent-context-plugins ctx) :audit-log))
               (entry (car log)))
          (expect (length log) :to-equal 1)
          (expect (cdr (assoc 'preset entry)) :to-equal "expert-reviewer")
          (expect (cdr (assoc 'type entry)) :to-equal "ptc")
          (expect (cdr (assoc 'target entry)) :to-equal 'spawn-subagent)
          (expect (cdr (assoc 'args entry)) :to-equal '(:name "worker-1")))))

    (it "appends multiple invocations sequentially and returns the updated log list"
      (let ((ctx (macher-agent--make-context :id "multi-audit-ctx")))
        (let ((log1 (macher-agent-log-tool-intent ctx "gptel-tool" "tool-1" '(:a 1))))
          (expect (length log1) :to-equal 1))
        (let ((log2 (macher-agent-log-tool-intent ctx "ptc" "tool-2" '(:b 2))))
          (expect (length log2) :to-equal 2)
          (expect (cdr (assoc 'target (nth 0 log2))) :to-equal "tool-1")
          (expect (cdr (assoc 'target (nth 1 log2))) :to-equal "tool-2"))
        (let ((log3 (macher-agent-log-tool-intent ctx "gptel-tool" "tool-3" '(:c 3))))
          (expect (length log3) :to-equal 3)
          (expect (cdr (assoc 'target (nth 2 log3))) :to-equal "tool-3"))))

    (it "returns nil safely when context is nil or not a valid context struct"
      (expect (macher-agent-log-tool-intent nil "gptel-tool" "tool" nil) :to-be nil)
      (expect (macher-agent-log-tool-intent "invalid-context-string" "gptel-tool" "tool" nil) :to-be nil)
      (expect (macher-agent-log-tool-intent '(:not :a :struct) "gptel-tool" "tool" nil) :to-be nil)
      (expect (macher-agent-log-tool-intent 12345 "gptel-tool" "tool" nil) :to-be nil)))

  (describe "Pure context API functions"

    (it "binds default-directory to project root via macher-agent-with-project-root"
      (let* ((mock-root (expand-file-name "/mock/test/project/root/"))
             (captured-dir nil))
        (cl-letf (((symbol-function 'macher-agent-root) (lambda (&rest _) mock-root)))
          (macher-agent-with-project-root
           (setq captured-dir default-directory))
          (expect captured-dir :to-equal (file-name-as-directory mock-root)))))

    (it "resolves buffer path via macher-agent-workspace-resolve-path"
      (expect (macher-agent-workspace-resolve-path "src/core.el")
              :to-equal (macher-agent--resolve-buffer-name "src/core.el")))

    (it "reads and updates context file contents via context API"
      (let* ((ws (make-macher-agent-workspace :project-root "/mock/api-test-proj/"))
             (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
             (test-file "lib/module.el"))
        (macher-agent-context-update ctx test-file "(defun test-api () t)")
        (expect (macher-agent-context-read ctx test-file) :to-equal "(defun test-api () t)")
        (macher-agent-context-update ctx test-file "(defun test-api () nil)")
        (expect (macher-agent-context-read ctx test-file) :to-equal "(defun test-api () nil)")))

    (it "adds buffer contents to context scope via macher-agent-scope-add-file"
      (let* ((ws (make-macher-agent-workspace :project-root "/mock/scope-proj/"))
             (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
             (buf (generate-new-buffer "*scope-test-buf*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (insert "(defvar *scoped-var* 99)"))
              (macher-agent-scope-add-file (buffer-name buf) ctx)
              (expect (macher-agent-context-read ctx (buffer-name buf)) :to-equal "(defvar *scoped-var* 99)"))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "adds buffer to scope interactively via macher-agent-add-buffer-to-scope"
      (let* ((ws (make-macher-agent-workspace :project-root "/mock/interactive-scope/"))
             (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
             (buf (generate-new-buffer "*interactive-scope-buf*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (insert "sample buffer text"))
              (setq-local macher-agent--persistent-context ctx)
              (macher-agent-add-buffer-to-scope buf)
              (expect (macher-agent-context-read ctx (buffer-name buf)) :to-equal "sample buffer text"))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "signals an error in macher-agent-add-buffer-to-scope when persistent context is absent"
      (let ((macher-agent--persistent-context nil))
        (expect (condition-case _ (macher-agent-add-buffer-to-scope "*some-buf*") (error 'trapped))
                :to-equal 'trapped)))

    (it "prepares sub-agent directives and presets via macher-agent-prepare-instructions"
      (with-temp-buffer
        (let ((buf (current-buffer)))
          (macher-agent-prepare-instructions buf "Refactor instructions for subagent." '(elisp-coder))
          (expect (buffer-string) :to-equal "Refactor instructions for subagent.")
          (expect macher-agent-presets :to-equal '(elisp-coder))
          (expect (memq #'macher-agent-sync-prompt-transformer gptel-prompt-transform-functions) :to-be-truthy)
          (expect (memq #'macher-agent--enforce-tool-scope gptel-pre-tool-call-functions) :to-be-truthy))))

    (it "resolves context workspace root via macher-agent-context-workspace-root and alias"
      (let* ((ws (make-macher-agent-workspace :project-root "/mock/ctx-ws/"))
             (ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))
        (expect (macher-agent-context-workspace-root ctx)
                :to-equal (file-truename (expand-file-name "/mock/ctx-ws/")))
        (expect (macher-context-workspace-root ctx)
                :to-equal (file-truename (expand-file-name "/mock/ctx-ws/")))))

    (it "invokes task flush hook in macher-agent-force-review with explicit context"
      (let* ((ctx (macher-agent--make-context :id "force-rev-ctx"))
             (flushed-ctx nil))
        (cl-letf (((symbol-function 'macher-agent-run-task-flush-hook)
                   (lambda (c) (setq flushed-ctx c))))
          (macher-agent-force-review ctx)
          (expect flushed-ctx :to-be ctx))))

    (it "invokes task flush hook in macher-agent-force-review using buffer-local persistent context"
      (let* ((ctx (macher-agent--make-context :id "local-rev-ctx"))
             (flushed-ctx nil))
        (setq-local macher-agent--persistent-context ctx)
        (cl-letf (((symbol-function 'macher-agent-run-task-flush-hook)
                   (lambda (c) (setq flushed-ctx c))))
          (macher-agent-force-review)
          (expect flushed-ctx :to-be ctx)))))

  (describe "Detached PTC Execution Macro"

    (it "executes script via macher-agent-execute-detached-ptc and passes extra-primitives directly"
      (let* ((executed-res nil)
             (executed-err nil)
             (passed-prims nil))
        (cl-letf (((symbol-function 'macher-agent-execute-ptc-script)
                   (lambda (script-str ctx root-buf success-cb error-cb extra-prims)
                     (expect (stringp script-str) :to-be t)
                     (expect (macher-agent-context-p ctx) :to-be t)
                     (expect (buffer-live-p root-buf) :to-be t)
                     (setq passed-prims (buffer-local-value 'macher-agent--active-ptc-primitives root-buf))
                     (funcall success-cb 42))))
          (macher-agent-execute-detached-ptc
           (:buffer-name "*test-detached-ptc*"
            :reap t
            :primitives '(+ - my-custom-prim)
            :on-success (lambda (res) (setq executed-res res))
            :on-error (lambda (err) (setq executed-err err)))
           (+ 20 22))
          (expect executed-res :to-equal 42)
          (expect executed-err :to-be nil)
          (expect passed-prims :to-equal '(+ - my-custom-prim)))))

    (it "does not cl-letf rebind macher-agent--ptc-primitive-p in macher-agent-execute-detached-ptc"
      (let ((macro-expansion (macroexpand-1
                              '(macher-agent-execute-detached-ptc
                                (:primitives '(foo bar))
                                (foo 1 2)))))
        (expect (string-match-p "macher-agent--ptc-primitive-p" (format "%S" macro-expansion)) :to-be nil)))))

(describe "macher-agent-api architectural decoupling"

  (it "ensures macher-agent--log-gptel-pre-tool is hosted in macher-agent-gptel.el and not macher-agent-api.el"
    (let* ((api-file (or (locate-library "macher-agent-api.el")
                         (expand-file-name "macher-agent-api.el" default-directory)))
           (gptel-file (or (locate-library "macher-agent-gptel.el")
                           (expand-file-name "macher-agent-gptel.el" default-directory)))
           (api-content (with-temp-buffer (insert-file-contents api-file) (buffer-string)))
           (gptel-content (with-temp-buffer (insert-file-contents gptel-file) (buffer-string))))
      ;; Defined in gptel module
      (expect (string-match-p "(defun macher-agent--log-gptel-pre-tool" gptel-content) :to-be-truthy)
      ;; Absent from API module
      (expect (string-match-p "(defun macher-agent--log-gptel-pre-tool" api-content) :to-be nil)))

  (it "contains zero references to macher-agent-resolve-context or FSM probing in macher-agent-api.el"
    (let* ((api-file (or (locate-library "macher-agent-api.el")
                         (expand-file-name "macher-agent-api.el" default-directory)))
           (content (with-temp-buffer (insert-file-contents api-file) (buffer-string))))
      (expect (string-match-p "macher-agent-resolve-context" content) :to-be nil)
      (expect (string-match-p "macher-agent-get-active-fsm" content) :to-be nil)
      (expect (string-match-p "gptel-request" content) :to-be nil))))

(provide 'macher-agent-api-test)
;;; macher-agent-api-test.el ends here
