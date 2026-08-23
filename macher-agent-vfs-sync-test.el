;;; macher-agent-vfs-sync-test.el --- VFS Synchronisation Tests -*- lexical-binding: t; -*-

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
                (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "original state")
                (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "original state")
                (macher-agent--sync-context-entry entry)
                (expect (macher-agent-vfs-entry-curr entry) :to-equal "proposed ghost state")))

          (it "invalidates edits and prevents ghost diffs if the underlying buffer or file is destroyed"
              (let* ((entry (macher-agent-vfs-make-entry "test-file.el" "original state" "original state")))
                (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value nil)
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
                (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "new physical state")
                (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "new physical state")
                (let ((mutated (macher-agent--sync-context-entry entry)))
                  (expect mutated :to-be t)
                  (expect (macher-agent-vfs-entry-orig entry) :to-equal "new physical state")
                  (expect (macher-agent-vfs-entry-curr entry) :to-equal "new physical state"))))

          (it "invalidates uncommitted agent edits when physical disk state changes externally"
              (let* ((entry (macher-agent-vfs-make-entry "test.el" "original state" "agent edit")))
                (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "user physical edit")
                (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "user physical edit")
                (let ((mutated (macher-agent--sync-context-entry entry)))
                  (expect mutated :to-be t)
                  (expect (macher-agent-vfs-entry-orig entry) :to-equal "user physical edit")
                  (expect (macher-agent-vfs-entry-curr entry) :to-equal "user physical edit"))))

          (it "fast-forwards virtual memory if the physical mutation perfectly matches the virtual delta (patch applied)"
              (let* ((entry (macher-agent-vfs-make-entry "test.el" "original state" "agent edit")))
                (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "agent edit")
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
                        (lambda (e &optional ws &rest _r)
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
                      (kill-buffer buf))))))

          (it "enforces optimistic concurrency control in macher-agent--update-context-file against external disk modifications"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher--make-context :workspace workspace :contents nil))
                     (file-path "/mock/proj/test.el")
                     (original-mtime '(25000 10000))
                     (drifted-mtime '(25000 20000)))
                (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                (puthash file-path original-mtime (macher-agent-workspace-mtime-tracker workspace))
                (spy-on 'file-attributes :and-call-fake
                        (lambda (path)
                          (if (equal path file-path)
                              `(t 1 1 1 ,drifted-mtime ,drifted-mtime ,drifted-mtime 100 "mode" t 1 1)
                            nil)))
                (expect (macher-agent--update-context-file ctx file-path "New content")
                        :to-throw 'error)))

          (it "distinguishes between distinct files sharing base filenames without collision during merge and update"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (parent-ctx (macher--make-context :workspace workspace
                                                       :contents (list (macher-agent-vfs-make-entry "/mock/proj/src/main.c" "src orig" "src orig")
                                                                       (macher-agent-vfs-make-entry "/mock/proj/tests/main.c" "tests orig" "tests orig"))))
                     (child-ctx (macher--make-context :workspace workspace
                                                      :contents (list (macher-agent-vfs-make-entry "/mock/proj/src/main.c" "src orig" "src modified")))))
                (macher-agent--merge-contexts parent-ctx child-ctx)
                (let ((src-entry (cl-find "/mock/proj/src/main.c" (macher-context-contents parent-ctx) :key #'macher-agent-vfs-entry-path :test #'equal))
                      (tests-entry (cl-find "/mock/proj/tests/main.c" (macher-context-contents parent-ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                  (expect (macher-agent-vfs-entry-curr src-entry) :to-equal "src modified")
                  (expect (macher-agent-vfs-entry-curr tests-entry) :to-equal "tests orig"))))

          (it "tracks modification times in mtime tracker when reading context files from disk"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher--make-context :workspace workspace :contents nil))
                     (file-path "/mock/proj/read-track.el")
                     (mtime '(25000 30000)))
                (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                (spy-on 'file-exists-p :and-return-value t)
                (spy-on 'file-attributes :and-call-fake
                        (lambda (path)
                          (if (equal path file-path)
                              `(t 1 1 1 ,mtime ,mtime ,mtime 100 "mode" t 1 1)
                            nil)))
                (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "content on disk")
                (let ((content (macher-agent--read-context-file ctx file-path)))
                  (expect content :to-equal "content on disk")
                  (expect (gethash file-path (macher-agent-workspace-mtime-tracker workspace)) :to-equal mtime))))

          (it "supports re-entrant lock acquisition when held by the same task-id"
              (clrhash macher-agent--vfs-lock-table)
              (clrhash macher-agent--vfs-lock-queues)
              (clrhash macher-agent--pending-callbacks)
              (let* ((resource "src/reentrant-res.el")
                     (cb1-result nil)
                     (cb2-result nil))
                ;; Initial lock acquisition
                (puthash resource (lambda (res) (setq cb1-result res)) macher-agent--pending-callbacks)
                (macher-agent--vfs-a2a-callback
                 `(:type ACQUIRE_LOCK
                         :task-id "task-reentrant-1"
                         :metadata (:resource_path ,resource)))
                (expect (gethash resource macher-agent--vfs-lock-table) :to-equal (cons "task-reentrant-1" 1))
                (expect cb1-result :to-equal "Resource lock acquired.")
                ;; Re-entrant lock acquisition with the same task-id
                (puthash resource (lambda (res) (setq cb2-result res)) macher-agent--pending-callbacks)
                (macher-agent--vfs-a2a-callback
                 `(:type ACQUIRE_LOCK
                         :task-id "task-reentrant-1"
                         :metadata (:resource_path ,resource)))
                (expect (gethash resource macher-agent--vfs-lock-table) :to-equal (cons "task-reentrant-1" 2))
                (expect cb2-result :to-equal "Resource lock acquired.")
                (expect (gethash resource macher-agent--vfs-lock-queues) :to-be nil)
                ;; Release once decrements ref-count
                (expect (macher-agent-vfs-release-lock resource "task-reentrant-1") :to-be t)
                (expect (gethash resource macher-agent--vfs-lock-table) :to-equal (cons "task-reentrant-1" 1))
                ;; Release second time frees the lock completely
                (expect (macher-agent-vfs-release-lock resource "task-reentrant-1") :to-be t)
                (expect (gethash resource macher-agent--vfs-lock-table) :to-be nil)))

          (it "rejects lock release when task-id does not match the current owner"
              (clrhash macher-agent--vfs-lock-table)
              (clrhash macher-agent--vfs-lock-queues)
              (clrhash macher-agent--pending-callbacks)
              (let ((resource "src/owner-mismatch.el"))
                (puthash resource (cons "task-owner-1" 1) macher-agent--vfs-lock-table)
                ;; Attempting to release with wrong task-id returns nil
                (expect (macher-agent-vfs-release-lock resource "task-owner-2") :to-be nil)
                (expect (gethash resource macher-agent--vfs-lock-table) :to-equal (cons "task-owner-1" 1))
                ;; Attempting to release with nil task-id returns nil when owner is not nil
                (expect (macher-agent-vfs-release-lock resource nil) :to-be nil)
                (expect (gethash resource macher-agent--vfs-lock-table) :to-equal (cons "task-owner-1" 1))
                ;; Releasing with matching task-id succeeds
                (expect (macher-agent-vfs-release-lock resource "task-owner-1") :to-be t)
                (expect (gethash resource macher-agent--vfs-lock-table) :to-be nil)))

          (it "releases lock and transfers to queued requester on release"
              (clrhash macher-agent--vfs-lock-table)
              (clrhash macher-agent--vfs-lock-queues)
              (clrhash macher-agent--pending-callbacks)
              (let* ((resource "src/wildcard-lock.el")
                     (cb-result nil))
                (puthash resource (cons "task-1" 1) macher-agent--vfs-lock-table)
                (puthash resource (list (cons "task-2" (lambda (res) (setq cb-result res)))) macher-agent--vfs-lock-queues)
                (expect (macher-agent-vfs-release-lock resource "task-1") :to-be t)
                (expect (gethash resource macher-agent--vfs-lock-table) :to-equal (cons "task-2" 1))
                (expect cb-result :to-equal "Resource lock acquired.")))

          (it "classifies live buffers and asterisk buffers before slash-based file matching"
              (let* ((asterisk-slash-buf "*worker/subagent*")
                     (live-buf (generate-new-buffer "live-no-file-buffer"))
                     (live-buf-slash (generate-new-buffer "live/buffer/slash")))
                (unwind-protect
                    (progn
                      (expect (macher-agent-context-classify-entry asterisk-slash-buf "/mock/root") :to-equal 'buffer)
                      (expect (macher-agent-context-classify-entry live-buf "/mock/root") :to-equal 'buffer)
                      (expect (macher-agent-context-classify-entry live-buf-slash "/mock/root") :to-equal 'buffer)
                      (expect (macher-agent-context-classify-entry "*scratch*" "/mock/root") :to-equal 'buffer)
                      (expect (macher-agent-context-classify-entry "src/main.rs" "/mock/root") :to-equal 'file))
                  (kill-buffer live-buf)
                  (kill-buffer live-buf-slash))))

          (it "computes safe workspace hash inspecting unwrapped project cons cells"
              (let ((ws-cons '(agent project . "/mock/project/path"))
                    (direct-project '(project . "/mock/project/path")))
                (expect (macher-agent--safe-workspace-hash ws-cons)
                        :to-equal (md5 "/mock/project/path"))
                (expect (macher-agent--safe-workspace-hash direct-project)
                        :to-equal (md5 "/mock/project/path"))))

          (it "merges contexts respecting buffer-local macher-agent--is-subagent"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (parent-ctx (macher--make-context :workspace workspace
                                                       :contents (list (macher-agent-vfs-make-entry "/mock/proj/file.el" "v1" "v1"))))
                     (child-ctx (macher--make-context :workspace workspace
                                                      :contents (list (macher-agent-vfs-make-entry "/mock/proj/file.el" "v1" "v2")))))
                (let ((macher-agent--is-subagent nil))
                  (macher-agent--merge-contexts parent-ctx child-ctx)
                  (let ((entry (cl-find "/mock/proj/file.el" (macher-context-contents parent-ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                    (expect (macher-agent-vfs-entry-curr entry) :to-equal "v2")))))

          (it "extracts context from alist using 'context symbol key in macher-agent-storage--extract-context"
              (let* ((ws (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                     (payload-symbol `((context . ,ctx)))
                     (payload-keyword `((:context . ,ctx)))
                     (payload-target-sym `((target-context . ,ctx)))
                     (payload-target-kw `((:target-context . ,ctx))))
                (expect (macher-agent-storage--extract-context payload-symbol) :to-be ctx)
                (expect (macher-agent-storage--extract-context payload-keyword) :to-be ctx)
                (expect (macher-agent-storage--extract-context payload-target-sym) :to-be ctx)
                (expect (macher-agent-storage--extract-context payload-target-kw) :to-be ctx)
                (expect (macher-agent-storage--extract-context (list :target-context ctx)) :to-be ctx)
                (expect (macher-context-p (macher-agent-storage--extract-context '(project . "/mock/proj/"))) :to-be t)
                (expect (macher-agent-storage--extract-context nil) :to-be nil)
                (expect (macher-context-p (macher-agent-storage--extract-context "invalid-string")) :to-be t)
                (expect (macher-agent-storage--extract-context '(:non-context-key "foo")) :to-be nil)))

          (it "ensures trailing newline before separator in macher-agent--write-vfs-entries-to-buffer"
              (with-temp-buffer
                (let ((entries (list (macher-agent-vfs-make-entry "file-no-newline.txt" "orig" "content without newline")
                                     (macher-agent-vfs-make-entry "file-with-newline.txt" "orig" "content with newline\n"))))
                  (macher-agent--write-vfs-entries-to-buffer entries)
                  (let ((buf-str (buffer-string)))
                    (expect buf-str :to-equal
                            (concat "=== VFS ENTRY: file-no-newline.txt ===\n"
                                    "content without newline\n"
                                    "=======================\n\n"
                                    "=== VFS ENTRY: file-with-newline.txt ===\n"
                                    "content with newline\n"
                                    "=======================\n\n")))))))

(provide 'macher-agent-vfs-sync-test)
;;; macher-agent-vfs-sync-test.el ends here


