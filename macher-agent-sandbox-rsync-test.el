;;; macher-agent-sandbox-rsync-test.el --- Sandbox and Rsync Tests -*- lexical-binding: t; -*-

(let* ((file (or load-file-name buffer-file-name))
       (test-dir (cond
                  (file (file-name-directory file))
                  ((file-exists-p (expand-file-name "macher-agent-test-setup.el" default-directory))
                   default-directory)
                  ((file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" default-directory))
                   (expand-file-name "tests" default-directory))
                  (t default-directory))))
  (add-to-list 'load-path test-dir)
  (add-to-list 'load-path (expand-file-name "helpers" test-dir)))

(require 'macher-agent-test-setup)

(describe "Sandbox and Physical Disk Operations"
          (macher-agent-test-setup-before-each)

          (it "constructs rsync shell command inside git repository and errors when outside"
              (spy-on 'call-process :and-return-value 0)
              (let* ((src "/my/project/")
                     (dest "/tmp/sandbox/")
                     (cmd (macher-agent--build-rsync-cmd src dest)))
                (expect (stringp cmd) :to-be t)
                (expect (string-match-p "git .*ls-files -z -c --recurse-submodules" cmd) :to-be-truthy)
                (expect (string-match-p "git .*ls-files -z -o --exclude-standard" cmd) :to-be-truthy)
                (expect (string-match-p "rsync -aC --delete --from0 --files-from=-" cmd) :to-be-truthy)
                (expect 'call-process :to-have-been-called-with "git" nil nil nil "rev-parse" "--is-inside-work-tree"))
              (spy-on 'call-process :and-return-value 1)
              (expect (macher-agent--build-rsync-cmd "/my/project/" "/tmp/sandbox/") :to-throw 'error))

          (it "hydrates deletions and physically removes deleted files from sandbox"
              (let* ((entry (cons "test.txt" nil))
                     (hydrated (macher-agent--hydrate-vfs-entry entry "/mock/root"))
                     (sandbox-dir (make-temp-file "macher-test-sandbox-" t))
                     (target-file (expand-file-name "test.txt" sandbox-dir)))
                (expect (macher-agent-vfs-entry-curr hydrated) :to-be nil)
                (with-temp-file target-file (insert "existing"))
                (expect (file-exists-p target-file) :to-be t)
                (macher-agent--vfs-process-entries '("test.txt") sandbox-dir (lambda (e) e) (lambda (_) nil))
                (expect (file-exists-p target-file) :to-be nil)
                (delete-directory sandbox-dir t)))

          (it "reroutes virtual edits to ephemeral sandbox and no-ops on clean memory"
              (let* ((workspace-root "/my/project/")
                     (sandbox-dir "/tmp/sandbox-12345/")
                     (mock-ws (make-macher-agent-workspace :project-root workspace-root))
                     (dirty-ctx (macher--make-context :dirty-p t
                                                      :workspace mock-ws
                                                      :contents (list (macher-agent-vfs-make-entry "/my/project/src/main.rs" "orig" "new content"))))
                     (clean-ctx (macher--make-context :dirty-p nil :contents nil))
                     (write-region-called-with nil))
                (spy-on 'file-in-directory-p :and-return-value t)
                (spy-on 'macher--workspace-root :and-return-value workspace-root)
                (spy-on 'write-region :and-call-fake
                        (lambda (start _end filename &rest _)
                          (push (list start filename) write-region-called-with)))
                ;; Dirty memory applies overlay
                (macher-agent--vfs-apply-overlay-stateless (macher-context-contents dirty-ctx) workspace-root sandbox-dir)
                (expect 'write-region :to-have-been-called)
                (expect (caar write-region-called-with) :to-equal "new content")
                (expect (file-truename (cadar write-region-called-with)) :to-equal (file-truename "/tmp/sandbox-12345/src/main.rs"))
                ;; Clean memory does not write
                (spy-on 'write-region)
                (macher-agent--vfs-apply-overlay-stateless (macher-context-contents clean-ctx) workspace-root sandbox-dir)
                (expect 'write-region :not :to-have-been-called)))

          (it "always executes the 3-step composition in exact order without mangling paths"
              (let ((call-order nil))
                (spy-on 'macher-agent--vfs-verify-clean-merge :and-call-fake (lambda (&rest _) (push 'merge call-order)))
                (spy-on 'macher-agent--vfs-sync-baseline :and-call-fake (lambda (&rest _) (push 'sync call-order)))
                (spy-on 'macher-agent--vfs-apply-overlay-stateless :and-call-fake (lambda (&rest _) (push 'overlay call-order)))
                (spy-on 'macher-agent--build-rsync-cmd :and-return-value "echo dummy")
                (spy-on 'macher-agent-context-root :and-return-value "/my/project/")
                (spy-on 'make-temp-file :and-return-value "/tmp/sandbox-12345/")
                (spy-on 'delete-directory)
                (spy-on 'shell-command-to-string :and-return-value "running")
                (let ((mock-context (macher--make-context :workspace nil :contents (list 'dummy))))
                  (macher-agent-with-strict-vfs-pipeline mock-context
                                                         (shell-command-to-string "echo 'running'")))
                (expect 'macher-agent--vfs-verify-clean-merge :to-have-been-called)
                (expect 'macher-agent--vfs-sync-baseline :to-have-been-called)
                (expect 'macher-agent--vfs-apply-overlay-stateless :to-have-been-called)
                (expect (reverse call-order) :to-equal '(merge sync overlay)))))

(provide 'macher-agent-sandbox-rsync-test)
;;; macher-agent-sandbox-rsync-test.el ends here
