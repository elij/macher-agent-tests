;;; tests/macher-agent-test.el --- Tests for Macher Agent Main Module -*- lexical-binding: t; -*-

(let* ((file (or load-file-name buffer-file-name))
       (test-dir (cond
                  ((and file (file-exists-p (expand-file-name "macher-agent-test-setup.el" (file-name-directory (expand-file-name file)))))
                   (file-name-directory (expand-file-name file)))
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
(require 'macher-agent)
(require 'macher-agent-core)
(require 'macher-agent-gptel)
(require 'macher-agent-tools)
(require 'macher-agent-sandbox)
(require 'macher-agent-zero-mem)
(require 'macher-agent-vfs)
(require 'macher-agent-macher)

(describe "Macher Agent Main Module"
  (macher-agent-test-setup-before-each)

  (describe "macher-agent-inject-thought"
    (it "queues instruction formatted as user override into pending instructions"
      (let ((added-instruction nil))
        (cl-letf (((symbol-function 'macher-agent-add-pending-instruction)
                   (lambda (inst) (setq added-instruction inst))))
          (macher-agent-inject-thought "focus on performance")
          (expect added-instruction :to-equal "USER OVERRIDE: focus on performance"))))

    (it "works seamlessly with context using direct slot access"
      (with-temp-buffer
        (let ((ctx (make-macher-agent-context :id "thought-ctx" :project-root "/tmp/test")))
          (setq-local macher-agent--persistent-context ctx)
          (expect (macher-agent-context-id macher-agent--persistent-context) :to-equal "thought-ctx")
          (expect (macher-agent-context-project-root macher-agent--persistent-context) :to-equal "/tmp/test")
          (macher-agent-inject-thought "optimize loop")
          (expect (macher-agent-context-p macher-agent--persistent-context) :to-be t)))))

  (describe "macher-agent-mode"
    (it "sets up gptel mode and tools when enabled"
      (with-temp-buffer
        (let ((mode-setup-called nil)
              (buf-setup-called nil)
              (wrap-tools-called nil))
          (cl-letf (((symbol-function 'macher-agent-gptel-mode-setup)
                     (lambda () (setq mode-setup-called t)))
                    ((symbol-function 'macher-agent-setup-gptel-buffer)
                     (lambda () (setq buf-setup-called t)))
                    ((symbol-function 'macher-agent--wrap-macher-tools)
                     (lambda () (setq wrap-tools-called t))))
            (macher-agent-mode 1)
            (expect mode-setup-called :to-be t)
            (expect buf-setup-called :to-be t)
            (expect wrap-tools-called :to-be t)))))

    (it "cleans up hooks when disabled"
      (with-temp-buffer
        (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)
        (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t)
        (setq-local macher-agent-mode t)
        (macher-agent-mode -1)
        (expect (memq #'macher-agent-sync-prompt-transformer gptel-prompt-transform-functions) :to-be nil)
        (expect (memq #'macher-agent--enforce-tool-scope gptel-pre-tool-call-functions) :to-be nil))))

  (describe "macher-agent-install and setup alias"
    (it "invokes module installation functions and registers global hooks"
      (let ((ctx-res-called nil)
            (sandbox-called nil)
            (zero-mem-called nil)
            (vfs-called nil)
            (trans-called nil))
        (cl-letf (((symbol-function 'macher-agent-context-resolution-install)
                   (lambda () (setq ctx-res-called t)))
                  ((symbol-function 'macher-agent-sandbox-install)
                   (lambda () (setq sandbox-called t)))
                  ((symbol-function 'macher-agent-zero-mem-install)
                   (lambda () (setq zero-mem-called t)))
                  ((symbol-function 'macher-agent-vfs-install)
                   (lambda () (setq vfs-called t)))
                  ((symbol-function 'macher-agent-transmission-install)
                   (lambda () (setq trans-called t))))
          (macher-agent-install)
          (expect ctx-res-called :to-be t)
          (expect sandbox-called :to-be t)
          (expect zero-mem-called :to-be t)
          (expect vfs-called :to-be t)
          (expect trans-called :to-be t)
          (expect (memq #'macher-agent--fsm-hijack-transform gptel-prompt-transform-functions) :to-be-truthy)
          (expect (memq #'macher-agent--restore-local-state hack-local-variables-hook) :to-be-truthy)
          (expect (memq #'macher-agent--log-gptel-pre-tool gptel-pre-tool-call-functions) :to-be-truthy)
          (expect (memq #'macher-agent--mutation-dispatcher macher-agent-context-mutated-hook) :to-be-truthy))))

    (it "aliases macher-agent-setup to macher-agent-install"
      (expect (symbol-function 'macher-agent-setup) :to-equal #'macher-agent-install)))

  (describe "Obsolete Context Helpers Verification"
    (it "contains no references to obsolete context helpers in macher-agent.el"
      (let ((file-content
             (with-temp-buffer
               (insert-file-contents
                (expand-file-name "macher-agent.el"
                                  (locate-dominating-file default-directory "macher-agent.el")))
               (buffer-string))))
        (expect (string-match-p "macher-agent--get-context-data" file-content) :to-be nil)
        (expect (string-match-p "macher-agent--set-context-data" file-content) :to-be nil)
        (expect (string-match-p "macher-agent-context-zero-mem" file-content) :to-be nil)
        (expect (string-match-p "macher-agent--get-context-workspace" file-content) :to-be nil)
        (expect (string-match-p "macher-agent--get-context-shadow-buffers" file-content) :to-be nil)))

    (it "operates strictly on struct slots and specialised accessors"
      (let ((ctx (make-macher-agent-context
                  :id "canonical-01"
                  :project-root "/mock/root"
                  :tools '(tool-1)
                  :skills '(skill-1)
                  :plugins '(:key "val"))))
        ;; Direct slot access
        (expect (macher-agent-context-id ctx) :to-equal "canonical-01")
        (expect (macher-agent-context-project-root ctx) :to-equal "/mock/root")
        (expect (macher-agent-context-tools ctx) :to-equal '(tool-1))
        (expect (macher-agent-context-skills ctx) :to-equal '(skill-1))
        (expect (plist-get (macher-agent-context-plugins ctx) :key) :to-equal "val")
        ;; Specialised accessors
        (setf (macher-agent-context-prompt ctx) "test prompt")
        (expect (macher-agent-context-prompt ctx) :to-equal "test prompt")
        (expect (macher-agent-context-workspace ctx) :to-equal (cons 'project (directory-file-name (expand-file-name "/mock/root"))))
        (expect (macher-agent-context-root ctx) :to-equal (file-truename (expand-file-name "/mock/root")))))))

(provide 'tests/macher-agent-test)
(provide 'macher-agent-test)
;;; macher-agent-test.el ends here
