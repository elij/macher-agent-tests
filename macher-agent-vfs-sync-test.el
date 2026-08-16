;;; macher-agent-test-vfs-sync.el --- VFS Synchronisation Tests -*- lexical-binding: t; -*-

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

(describe "Virtual File System (VFS) Mutators"
          (macher-agent-test-setup-before-each)

          (it "throws a security error if accessing a path outside of the allowed context"
              (let ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "allowed.txt" "old" "new")))))
                (expect (macher-agent--ensure-access ctx "forbidden.txt") :to-throw 'error)))

          (it "successfully records a virtual edit to an existing scoped buffer"
              (let* ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "test.txt" "orig" "orig")))))
                (macher-agent--update-context-file ctx "test.txt" "modified")
                (expect (macher-context-dirty-p ctx) :to-be t)
                (expect (macher-agent-vfs-entry-curr (cl-find "test.txt" (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)) :to-equal "modified")))

          (it "invalidates the local cache if both local and remote diverged"
              (let* ((test-dir (make-temp-file "macher-test-dir" t))
                     (test-file (expand-file-name "test.txt" test-dir))
                     (ctx (macher--make-context :dirty-p t)))
                (setf (macher-context-contents ctx)
                      (list (macher-agent-vfs-make-entry test-file "v1" "v2-local")))
                (with-temp-file test-file (insert "v2-remote"))
                (macher-agent--auto-sync-context ctx)
                (let ((entry (cl-find test-file (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                  (expect (macher-agent-vfs-entry-orig entry) :to-equal "v2-remote")
                  (expect (macher-agent-vfs-entry-curr entry) :to-equal "v2-remote"))
                (delete-directory test-dir t)))

          (it "preserves unapplied virtual edits across tool calls if the physical state has not mutated"
              (let* ((entry (macher-agent-vfs-make-entry "test-file.el" "original state" "proposed ghost state")))
                (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "original state")
                (macher-agent--sync-context-entry entry)
                (expect (macher-agent-vfs-entry-curr entry) :to-equal "proposed ghost state")))

          (it "invalidates edits and prevents ghost diffs if the underlying buffer or file is destroyed"
              (let* ((entry (macher-agent-vfs-make-entry "test-file.el" "original state" "proposed ghost state")))
                (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value nil)
                (macher-agent--sync-context-entry entry)
                (expect (macher-agent-vfs-entry-orig entry) :to-be nil)
                (expect (macher-agent-vfs-entry-curr entry) :to-be nil)))

          (it "splits pure buffers from physical files for independent diff generation"
              (let* ((ctx (macher--make-context))
                     (file-path (expand-file-name "dummy-file.txt" temporary-file-directory))
                     (pure-name "*macher-dummy-buf*")
                     (file-buf (find-file-noselect file-path))
                     (pure-buf (get-buffer-create pure-name)))
                (push (macher-agent-vfs-make-entry file-path "a" "b") (macher-context-contents ctx))
                (push (macher-agent-vfs-make-entry pure-name "x" "y") (macher-context-contents ctx))
                (expect (length (macher-context-contents ctx)) :to-equal 2)

                (let* ((split (macher-agent--split-context ctx))
                       (file-ctx (car split))
                       (buf-ctx (cdr split)))
                  (expect (length (macher-context-contents file-ctx)) :to-equal 1)
                  (expect (cl-find file-path (macher-context-contents file-ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :to-be-truthy)
                  (expect (cl-find pure-name (macher-context-contents file-ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :to-be nil)

                  (expect (cl-find pure-name (macher-context-contents buf-ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :to-be-truthy)
                  (expect (cl-find file-path (macher-context-contents buf-ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :to-be nil))

                (kill-buffer file-buf)
                (kill-buffer pure-buf)))

          (it "fast-forwards a clean virtual memory if the physical disk mutates naturally"
              (let* ((entry (macher-agent-vfs-make-entry "test.el" "original state" "original state")))
                (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "new physical state")
                (let ((mutated (macher-agent--sync-context-entry entry)))
                  (expect mutated :to-be t)
                  (expect (macher-agent-vfs-entry-orig entry) :to-equal "new physical state")
                  (expect (macher-agent-vfs-entry-curr entry) :to-equal "new physical state"))))

          (it "OPTIMISTIC CONCURRENCY: invalidates virtual edits if a hostile physical mutation occurs"
              (let* ((entry (macher-agent-vfs-make-entry "test.el" "original state" "agent edit")))
                (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "user physical edit")
                (let ((mutated (macher-agent--sync-context-entry entry)))
                  (expect mutated :to-be t)
                  (expect (macher-agent-vfs-entry-orig entry) :to-equal "user physical edit")
                  (expect (macher-agent-vfs-entry-curr entry) :to-equal "user physical edit"))))

          (it "fast-forwards virtual memory if the physical mutation perfectly matches the virtual delta (patch applied)"
              (let* ((entry (macher-agent-vfs-make-entry "test.el" "original state" "agent edit")))
                (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "agent edit")
                (let ((mutated (macher-agent--sync-context-entry entry)))
                  (expect mutated :to-be t)
                  (expect (macher-agent-vfs-entry-orig entry) :to-equal "agent edit")
                  (expect (macher-agent-vfs-entry-curr entry) :to-equal "agent edit"))))

          (it "bypasses stale live buffer and reads directly from physical disk if disk mtime is newer than stored mtime"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher--make-context :workspace workspace :contents nil))
                     (file-path "/mock/proj/test.el")
                     (entry (macher-agent-vfs-make-entry file-path "original state" "original state"))
                     (old-mtime '(25000 10000))
                     (new-mtime '(25000 20000)))
                (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                (let ((tracker (macher-agent-workspace-mtime-tracker workspace)))
                  (puthash file-path old-mtime tracker)
                  (spy-on 'file-attributes :and-call-fake
                          (lambda (path)
                            (if (equal path file-path)
                                `(t 1 1 1 ,new-mtime ,new-mtime ,new-mtime 100 "mode" t 1 1)
                              nil)))
                  (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "fresh disk state")
                  (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "stale buffer state")
                  (let ((mutated (macher-agent--sync-context-entry entry (macher-agent-workspace-mtime-tracker workspace))))
                    (expect mutated :to-be t)
                    (expect (macher-agent-vfs-entry-orig entry) :to-equal "fresh disk state")
                    (expect (macher-agent-vfs-entry-curr entry) :to-equal "fresh disk state")
                    (expect (gethash file-path tracker) :to-equal new-mtime)))))

          (it "passes workspace argument from macher-agent--sync-and-check-dirty-entries to macher-agent--sync-context-entry"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (entry (macher-agent-vfs-make-entry "/mock/proj/test.el" "a" "b"))
                     (contents (list entry))
                     (passed-ws nil))
                (spy-on 'macher-agent--sync-context-entry :and-call-fake
                        (lambda (e &optional ws)
                          (setq passed-ws ws)
                          nil))
                (macher-agent--sync-and-check-dirty-entries contents workspace)
                (expect passed-ws :to-equal workspace)))

          (it "resolves attrs for virtual paths via file-attributes without requiring file-exists-p"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher--make-context :workspace workspace :contents nil))
                     (file-path "/mock/proj/virtual-file.el")
                     (entry (macher-agent-vfs-make-entry file-path "original state" "original state"))
                     (old-mtime '(25000 10000))
                     (new-mtime '(25000 20000)))
                (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                (let ((tracker (macher-agent-workspace-mtime-tracker workspace))
                      (attrs-called nil))
                  (puthash file-path old-mtime tracker)
                  (spy-on 'file-attributes :and-call-fake
                          (lambda (path)
                            (when (equal path file-path)
                              (setq attrs-called t)
                              `(t 1 1 1 ,new-mtime ,new-mtime ,new-mtime 100 "mode" t 1 1))))
                  (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "mock disk state")
                  (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "mock disk state")
                  (let ((mutated (macher-agent--sync-context-entry entry (macher-agent-workspace-mtime-tracker workspace))))
                    (expect attrs-called :to-be t)
                    (expect mutated :to-be t)
                    (expect (macher-agent-vfs-entry-orig entry) :to-equal "mock disk state")))))

          (it "invalidates and bypasses stale live buffers when an externally modified disk file has a newer mtime"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher--make-context :workspace workspace :contents nil))
                     (file-path "/mock/proj/desynced-buffer.el")
                     (entry (macher-agent-vfs-make-entry file-path "original state" "original state"))
                     (old-mtime '(25000 10000))
                     (new-mtime '(25000 20000))
                     (buf (get-buffer-create file-path)))
                (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                (let ((tracker (macher-agent-workspace-mtime-tracker workspace)))
                  (unwind-protect
                      (progn
                        (with-current-buffer buf
                          (erase-buffer)
                          (insert "desynced live buffer content"))
                        (puthash file-path old-mtime tracker)
                        (spy-on 'file-attributes :and-call-fake
                                (lambda (path)
                                  (if (equal path file-path)
                                      `(t 1 1 1 ,new-mtime ,new-mtime ,new-mtime 100 "mode" t 1 1)
                                    nil)))
                        (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "new disk content")
                        (let ((mutated (macher-agent--sync-context-entry entry (macher-agent-workspace-mtime-tracker workspace))))
                          (expect mutated :to-be t)
                          (expect (macher-agent-vfs-entry-orig entry) :to-equal "new disk content")
                          (expect (macher-agent-vfs-entry-curr entry) :to-equal "new disk content")))
                    (when (buffer-live-p buf)
                      (kill-buffer buf))))))

          (it "treats desynced live buffer content as current-state when physical disk is NOT newer"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher--make-context :workspace workspace :contents nil))
                     (file-path "/mock/proj/desynced-buffer.el")
                     (entry (macher-agent-vfs-make-entry file-path "original state" "original state"))
                     (same-mtime '(25000 10000))
                     (buf (get-buffer-create file-path)))
                (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                (let ((tracker (macher-agent-workspace-mtime-tracker workspace)))
                  (unwind-protect
                      (progn
                        (with-current-buffer buf
                          (erase-buffer)
                          (insert "desynced live buffer content"))
                        (puthash file-path same-mtime tracker)
                        (spy-on 'file-attributes :and-call-fake
                                (lambda (path)
                                  (if (equal path file-path)
                                      `(t 1 1 1 ,same-mtime ,same-mtime ,same-mtime 100 "mode" t 1 1)
                                    nil)))
                        (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "disk content")
                        (let ((mutated (macher-agent--sync-context-entry entry (macher-agent-workspace-mtime-tracker workspace))))
                          (expect mutated :to-be t)
                          (expect (macher-agent-vfs-entry-orig entry) :to-equal "desynced live buffer content")
                          (expect (macher-agent-vfs-entry-curr entry) :to-equal "desynced live buffer content")))
                    (when (buffer-live-p buf)
                      (kill-buffer buf))))))

          (it "gives precedence to desynced live buffer when its content matches orig even if disk is newer"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher--make-context :workspace workspace :contents nil))
                     (file-path "/mock/proj/dirty-buffer.el")
                     (entry (macher-agent-vfs-make-entry file-path "original state" "original state"))
                     (old-mtime '(25000 10000))
                     (new-mtime '(25000 20000))
                     (buf (get-buffer-create file-path)))
                (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                (let ((tracker (macher-agent-workspace-mtime-tracker workspace)))
                  (unwind-protect
                      (progn
                        (with-current-buffer buf
                          (erase-buffer)
                          (insert "original state"))
                        (puthash file-path old-mtime tracker)
                        (spy-on 'file-attributes :and-call-fake
                                (lambda (path)
                                  (if (equal path file-path)
                                      `(t 1 1 1 ,new-mtime ,new-mtime ,new-mtime 100 "mode" t 1 1)
                                    nil)))
                        (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "new disk content")
                        (let ((mutated (macher-agent--sync-context-entry entry (macher-agent-workspace-mtime-tracker workspace))))
                          (expect mutated :to-be nil)
                          (expect (macher-agent-vfs-entry-orig entry) :to-equal "original state")
                          (expect (macher-agent-vfs-entry-curr entry) :to-equal "original state")))
                    (when (buffer-live-p buf)
                      (kill-buffer buf)))))))

(provide 'macher-agent-test-vfs-sync)
;;; macher-agent-test-vfs-sync.el ends here


