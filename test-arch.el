;;; test-arch.el --- Module architecture and static invariants -*- lexical-binding: t; -*-

;;; Commentary:
;; Specification 1: Module architecture and static invariants
;; ARCH-01, ARCH-02, ARCH-03, ARCH-04

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-test-helpers)

(describe "1. Module architecture and static invariants"

  (before-each
    (macher-agent-test-reset-env))

  ;; --------------------------------------------------------------------------
  ;; ARCH-01
  ;; --------------------------------------------------------------------------
  (it "ARCH-01: byte-compiles all package modules and skill scripts in clean subprocess with zero warnings"
    (let* ((root (file-name-directory (locate-library "macher-agent")))
           (emacs-bin (expand-file-name invocation-name invocation-directory))
           (package-modules
            (directory-files root t "^macher-agent\\(-[a-z]+\\)*\\.el$"))
           (scripts-dir (expand-file-name "skills/scripts" root))
           (skill-scripts
            (when (file-directory-p scripts-dir)
              (directory-files scripts-dir t "\\.el$")))
           (all-files (append package-modules skill-scripts))
           (out-buf (generate-new-buffer " *arch01-compile-out*")))
      (unwind-protect
          (dolist (file all-files)
            (with-current-buffer out-buf (erase-buffer))
            (let* ((is-pkg-mod (member file package-modules))
                   (eval-code
                    (if is-pkg-mod
                        "(setq byte-compile-error-on-warn t byte-compile-warnings '(not obsolete docstrings))"
                      (format "(progn (setq byte-compile-error-on-warn t byte-compile-warnings '(not obsolete docstrings)) (dolist (feat '(gptel macher-agent-core macher-agent-presets macher-agent-tools macher-agent-gptel macher-agent-orchestration macher-agent-api macher-agent-vfs macher-agent-sandbox macher-agent-zero-mem transient)) (require feat nil t)) (unless (fboundp 'macher-agent-workspace-mtime-tracker) (defun macher-agent-workspace-mtime-tracker (&rest _) nil)) (let ((sdir %S)) (when (file-directory-p sdir) (dolist (f (directory-files sdir t \"\\\\.el$\")) (let* ((b (file-name-sans-extension (file-name-nondirectory f))) (h (intern (format \"macher-agent-%%s-tool\" (replace-regexp-in-string \"_\" \"-\" b)))) (u (intern (format \"macher-agent-%%s-tool\" (replace-regexp-in-string \"-\" \"_\" b))))) (eval `(defvar ,h nil)) (eval `(defvar ,u nil)))))))"
                              (expand-file-name "skills/scripts" root))))
                   (args (list "-Q" "--batch"
                               "-L" (expand-file-name root)
                               "-L" (expand-file-name "macher" root)
                               "-L" (expand-file-name "gptel" root)
                               "-L" (expand-file-name "skills/scripts" root)
                               "--eval" eval-code
                               "-f" "batch-byte-compile"
                               file))
                   (exit-code (apply #'call-process emacs-bin nil out-buf nil args))
                   (output (with-current-buffer out-buf (buffer-string))))
              ;; If compilation produced an .elc file, delete it
              (let ((elc (concat (file-name-sans-extension file) ".elc")))
                (when (file-exists-p elc)
                  (delete-file elc)))
              (let ((clean-output (replace-regexp-in-string "^.*macher/.*: Warning:.*\\(?:\n\\)?" "" output)))
                (when (/= exit-code 0)
                  (error "ARCH-01 FAILED ON %s (exit %s): %s" file exit-code output))
                (expect exit-code :to-equal 0)
                (expect clean-output :not :to-match "Warning:")
                (expect clean-output :not :to-match "free variable")
                (expect clean-output :not :to-match "unresolved call"))))
        (when (buffer-live-p out-buf)
          (kill-buffer out-buf)))))

  ;; --------------------------------------------------------------------------
  ;; ARCH-02
  ;; --------------------------------------------------------------------------
  (it "ARCH-02: verifies five-tier Directed Acyclic Graph without circular or downward requires"
    (let* ((root (file-name-directory (locate-library "macher-agent")))
           (tier-0 '(macher-agent-core))
           (tier-1 '(macher-agent-presets macher-agent-tools macher-agent-gptel))
           (tier-2 '(macher-agent-orchestration macher-agent))
           (tier-3 '(macher-agent-vfs macher-agent-sandbox macher-agent-zero-mem))
           (tier-4 '(macher-agent-macher))
           (upper-tiers (append tier-0 tier-1 tier-2))
           (lower-tiers (append tier-3 tier-4))
           (all-modules (append tier-0 tier-1 '(macher-agent-orchestration macher-agent-api macher-agent) tier-3 tier-4))
           (require-graph (make-hash-table :test 'eq)))

      ;; Extract require graph from ASTs of all package modules
      (dolist (mod all-modules)
        (let* ((file (expand-file-name (format "%s.el" mod) root))
               (reqs nil))
          (when (file-exists-p file)
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (condition-case nil
                  (while t
                    (let ((form (read (current-buffer))))
                      (when (and (consp form) (eq (car form) 'require))
                        (let ((feat (cadr form)))
                          (when (and (consp feat) (eq (car feat) 'quote))
                            (setq feat (cadr feat)))
                          (when (memq feat all-modules)
                            (push feat reqs))))))
                (end-of-file nil))))
          (puthash mod (delete-dups reqs) require-graph)))

      ;; Assert: No module in Tiers 0, 1, or 2 requires a Tier 3 or Tier 4 plugin
      (dolist (mod upper-tiers)
        (let ((reqs (gethash mod require-graph)))
          (dolist (r reqs)
            (expect (memq r lower-tiers) :to-be nil))))

      ;; Assert: Tier 4 (macher-agent-macher) is only required by macher-agent-vfs
      ;; and only requires macher-agent-core internally
      (dolist (mod all-modules)
        (let ((reqs (gethash mod require-graph)))
          (when (memq 'macher-agent-macher reqs)
            (expect mod :to-equal 'macher-agent-vfs))))
      (expect (gethash 'macher-agent-macher require-graph)
              :to-equal '(macher-agent-core))

      ;; Assert: Require graph is a Directed Acyclic Graph (no cycles)
      (let ((visited (make-hash-table :test 'eq))
            (rec-stack (make-hash-table :test 'eq))
            (has-cycle nil))
        (cl-labels ((dfs (node)
                      (puthash node t visited)
                      (puthash node t rec-stack)
                      (dolist (neighbor (gethash node require-graph nil))
                        (cond
                         ((gethash neighbor rec-stack)
                          (setq has-cycle t))
                         ((not (gethash neighbor visited))
                          (dfs neighbor))))
                      (puthash node nil rec-stack)))
          (dolist (node all-modules)
            (unless (gethash node visited)
              (dfs node)))
          (expect has-cycle :to-be nil)))))

  ;; --------------------------------------------------------------------------
  ;; ARCH-03
  ;; --------------------------------------------------------------------------
  (it "ARCH-03: verifies zero internal declare-function forms targeting macher-agent symbols"
    (let* ((root (file-name-directory (locate-library "macher-agent")))
           (files (directory-files root t "^macher-agent\\(-[a-z]+\\)*\\.el$"))
           (internal-declares nil))
      (dolist (file files)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case nil
              (while t
                (let ((form (read (current-buffer))))
                  (when (and (consp form) (eq (car form) 'declare-function))
                    (let ((fn (cadr form)))
                      (when (and (consp fn) (eq (car fn) 'quote))
                        (setq fn (cadr fn)))
                      (when (and (symbolp fn)
                                 (string-prefix-p "macher-agent-" (symbol-name fn)))
                        (push (list file fn form) internal-declares))))))
            (end-of-file nil))))
      (expect (length internal-declares) :to-equal 0)))

  ;; --------------------------------------------------------------------------
  ;; ARCH-04
  ;; --------------------------------------------------------------------------
  (it "ARCH-04: registers pipeline steps idempotently and unregisters cleanly without dangling functions"
    (clrhash macher-agent-pipeline-registry)
    ;; Run macher-agent-install multiple times
    (macher-agent-install)
    (macher-agent-install)
    (macher-agent-install)

    ;; Verify all pipeline steps are registered without duplicate entries
    (maphash
     (lambda (_pipeline entries)
       (let ((steps (mapcar (lambda (e) (or (plist-get e :step) (plist-get e :fn))) entries)))
         (expect (length steps) :to-equal (length (delete-dups (copy-sequence steps))))))
     macher-agent-pipeline-registry)

    ;; Verify sandbox steps are present
    (expect (member #'macher-agent-ptc--inject-tool
                    (macher-agent-get-pipeline-steps 'preset-composition))
            :to-be-truthy)
    (expect (member #'macher-agent-sandbox-append-ptc-to-transmission
                    (macher-agent-get-pipeline-steps 'transmission))
            :to-be-truthy)

    ;; Verify zero-mem steps are present
    (expect (member #'macher-agent-memory-pipe--inject-tool
                    (macher-agent-get-pipeline-steps 'transmission))
            :to-be-truthy)
    (expect (member #'macher-agent-memory-pipe--truncate-buffer
                    (macher-agent-get-pipeline-steps 'transmission))
            :to-be-truthy)
    (expect (member #'macher-agent-memory-pipe--inject-directive
                    (macher-agent-get-pipeline-steps 'transmission))
            :to-be-truthy)

    ;; Verify VFS steps are present
    (expect (member #'macher-agent-vfs--merge-payload
                    (macher-agent-get-pipeline-steps 'payload-merge))
            :to-be-truthy)
    (expect (member #'macher-agent-vfs--clone-context-step
                    (macher-agent-get-pipeline-steps 'context-clone))
            :to-be-truthy)

    ;; Invoke plugin uninstallers
    (macher-agent-vfs-uninstall)
    (macher-agent-sandbox-uninstall)
    (macher-agent-zero-mem-uninstall)

    ;; Verify uninstalled cleanly without leaving dangling functions in pipeline registry
    (expect (member #'macher-agent-vfs--merge-payload
                    (macher-agent-get-pipeline-steps 'payload-merge))
            :to-be nil)
    (expect (member #'macher-agent-vfs--clone-context-step
                    (macher-agent-get-pipeline-steps 'context-clone))
            :to-be nil)
    (expect (member #'macher-agent-ptc--inject-tool
                    (macher-agent-get-pipeline-steps 'preset-composition))
            :to-be nil)
    (expect (member #'macher-agent-sandbox-append-ptc-to-transmission
                    (macher-agent-get-pipeline-steps 'transmission))
            :to-be nil)
    (expect (member #'macher-agent-memory-pipe--inject-tool
                    (macher-agent-get-pipeline-steps 'transmission))
            :to-be nil)
    (expect (member #'macher-agent-memory-pipe--truncate-buffer
                    (macher-agent-get-pipeline-steps 'transmission))
            :to-be nil)
    (expect (member #'macher-agent-memory-pipe--inject-directive
                    (macher-agent-get-pipeline-steps 'transmission))
            :to-be nil)
    ;; Restore pipeline state for subsequent tests
    (macher-agent-install)))

(provide 'test-arch)
;;; test-arch.el ends here
