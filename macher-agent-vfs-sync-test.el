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
(require 'macher-agent-vfs)
(require 'macher-agent-macher)

(describe "Virtual File System (VFS) Mutators"
          (macher-agent-test-setup-before-each)

          (it "throws a security error if accessing a path outside of the allowed context"
              (let ((ctx (macher-agent--make-context
                          :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "allowed.txt" "old" "new")))))))
                (expect (macher-agent--ensure-access ctx "forbidden.txt") :to-throw 'error)))

          (it "successfully records a virtual edit to an existing scoped buffer"
              (let* ((ctx (macher-agent--make-context
                           :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "test.txt" "orig" "orig")))))))
                (macher-agent--update-context-file ctx "test.txt" "modified")
                (expect (macher-agent--context-dirty-p ctx) :to-be t)
                (expect (macher-agent-vfs-entry-curr (cl-find "test.txt" (macher-agent--get-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)) :to-equal "modified")))

          (it "invalidates the local cache if both local and remote diverged"
              (let* ((test-dir (make-temp-file "macher-test-dir" t))
                     (test-file (expand-file-name "test.txt" test-dir))
                     (ctx (macher-agent--make-context
                           :project-root test-dir
                           :plugins (list :vfs (list :dirty-p t
                                                     :contents (list (macher-agent-vfs-make-entry test-file "v1" "v2-local")))))))
                (with-temp-file test-file (insert "v2-remote"))
                (macher-agent--auto-sync-context ctx)
                (let ((entry (cl-find test-file (macher-agent--get-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                  (expect (macher-agent-vfs-entry-orig entry) :to-equal "v2-remote")
                  (expect (macher-agent-vfs-entry-curr entry) :to-equal "v2-remote"))
                (delete-directory test-dir t)))

          (it "safely guards macher-agent--read-content-from-disk-or-buffer against non-string and invalid inputs"
              (expect (macher-agent--read-content-from-disk-or-buffer nil) :to-be nil)
              (expect (macher-agent--read-content-from-disk-or-buffer :invalid-keyword) :to-be nil)
              (expect (macher-agent--read-content-from-disk-or-buffer 'some-symbol) :to-be nil)
              (expect (macher-agent--read-content-from-disk-or-buffer 12345) :to-be nil)
              (expect (macher-agent--read-content-from-disk-or-buffer '(:path "test")) :to-be nil)
              (expect (macher-agent--read-content-from-disk-or-buffer "") :to-be nil))

          (it "preserves unapplied virtual edits across tool calls if the physical state has not mutated"
              (let* ((entry (macher-agent-vfs-make-entry "test-file.el" "original state" "proposed ghost state")))
                (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "original state")
                (macher-agent--sync-context-entry entry)
                (expect (macher-agent-vfs-entry-curr entry) :to-equal "proposed ghost state")))

          (it "invalidates edits and prevents ghost diffs if the underlying buffer or file is destroyed"
              (let* ((entry (macher-agent-vfs-make-entry "test-file.el" "original state" "original state")))
                (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value nil)
                (macher-agent--sync-context-entry entry)
                (expect (macher-agent-vfs-entry-orig entry) :to-be nil)
                (expect (macher-agent-vfs-entry-curr entry) :to-be nil)))

          (it "splits pure buffers from physical files for independent diff generation"
              (let* ((ctx (macher-agent--make-context))
                     (file-path (expand-file-name "dummy-file.txt" temporary-file-directory))
                     (pure-name "*macher-dummy-buf*")
                     (file-buf (find-file-noselect file-path))
                     (pure-buf (get-buffer-create pure-name)))
                (macher-agent--set-context-contents ctx (list (macher-agent-vfs-make-entry file-path "a" "b")
                                                              (macher-agent-vfs-make-entry pure-name "x" "y")))
                (expect (length (macher-agent--get-context-contents ctx)) :to-equal 2)

                (let* ((split (macher-agent--split-context ctx))
                       (file-ctx (car split))
                       (buf-ctx (cdr split)))
                  (expect (length (macher-agent--get-context-contents file-ctx)) :to-equal 1)
                  (expect (cl-find file-path (macher-agent--get-context-contents file-ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :to-be-truthy)
                  (expect (cl-find pure-name (macher-agent--get-context-contents file-ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :to-be nil)

                  (expect (cl-find pure-name (macher-agent--get-context-contents buf-ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :to-be-truthy)
                  (expect (cl-find file-path (macher-agent--get-context-contents buf-ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :to-be nil))

                (kill-buffer file-buf)
                (kill-buffer pure-buf)))

          (it "fast-forwards a clean virtual memory if the physical disk mutates naturally"
              (let* ((entry (macher-agent-vfs-make-entry "test.el" "original state" "original state")))
                (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "new physical state")
                (let ((mutated (macher-agent--sync-context-entry entry)))
                  (expect mutated :to-be t)
                  (expect (macher-agent-vfs-entry-orig entry) :to-equal "new physical state")
                  (expect (macher-agent-vfs-entry-curr entry) :to-equal "new physical state"))))

          (it "invalidates uncommitted agent edits when physical disk state changes externally"
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
                     (ctx (macher-agent--make-context :project-root "/mock/proj/"))
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
                  (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "fresh disk state")
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
                     (ctx (macher-agent--make-context :project-root "/mock/proj/"))
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
                  (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "mock disk state")
                  (let ((mutated (macher-agent--sync-context-entry entry (macher-agent-workspace-mtime-tracker workspace))))
                    (expect attrs-called :to-be t)
                    (expect mutated :to-be t)
                    (expect (macher-agent-vfs-entry-orig entry) :to-equal "mock disk state")))))

          (it "invalidates and bypasses stale live buffers when an externally modified disk file has a newer mtime"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher-agent--make-context :project-root "/mock/proj/"))
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
                        (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "new disk content")
                        (let ((mutated (macher-agent--sync-context-entry entry (macher-agent-workspace-mtime-tracker workspace))))
                          (expect mutated :to-be t)
                          (expect (macher-agent-vfs-entry-orig entry) :to-equal "new disk content")
                          (expect (macher-agent-vfs-entry-curr entry) :to-equal "new disk content")))
                    (when (buffer-live-p buf)
                      (kill-buffer buf))))))

          (it "treats desynced live buffer content as current-state when physical disk is NOT newer"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher-agent--make-context :project-root "/mock/proj/"))
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
                        (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "desynced live buffer content")
                        (let ((mutated (macher-agent--sync-context-entry entry (macher-agent-workspace-mtime-tracker workspace))))
                          (expect mutated :to-be t)
                          (expect (macher-agent-vfs-entry-orig entry) :to-equal "desynced live buffer content")
                          (expect (macher-agent-vfs-entry-curr entry) :to-equal "desynced live buffer content")))
                    (when (buffer-live-p buf)
                      (kill-buffer buf))))))

          (it "gives precedence to desynced live buffer when its content matches orig even if disk is newer"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher-agent--make-context :project-root "/mock/proj/"))
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
                        (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "original state")
                        (let ((mutated (macher-agent--sync-context-entry entry (macher-agent-workspace-mtime-tracker workspace))))
                          (expect mutated :to-be nil)
                          (expect (macher-agent-vfs-entry-orig entry) :to-equal "original state")
                          (expect (macher-agent-vfs-entry-curr entry) :to-equal "original state")))
                    (when (buffer-live-p buf)
                      (kill-buffer buf))))))

          (it "enforces optimistic concurrency control in macher-agent--update-context-file against external disk modifications"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher-agent--make-context :project-root "/mock/proj/"))
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
              (let* ((parent-ctx (macher-agent--make-context
                                  :project-root "/mock/proj/"
                                  :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/proj/src/main.c" "src orig" "src orig")
                                                                            (macher-agent-vfs-make-entry "/mock/proj/tests/main.c" "tests orig" "tests orig"))))))
                     (child-ctx (macher-agent--make-context
                                 :project-root "/mock/proj/"
                                 :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/proj/src/main.c" "src orig" "src modified")))))))
                (macher-agent--merge-contexts parent-ctx child-ctx)
                (let ((src-entry (cl-find "/mock/proj/src/main.c" (macher-agent--get-context-contents parent-ctx) :key #'macher-agent-vfs-entry-path :test #'equal))
                      (tests-entry (cl-find "/mock/proj/tests/main.c" (macher-agent--get-context-contents parent-ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                  (expect (macher-agent-vfs-entry-curr src-entry) :to-equal "src modified")
                  (expect (macher-agent-vfs-entry-curr tests-entry) :to-equal "tests orig"))))

          (it "tracks modification times in mtime tracker when reading context files from disk"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher-agent--make-context :project-root "/mock/proj/"))
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

          (it "classifies live buffers purely on runtime buffer state without asterisk heuristics"
              (let* ((pure-buf (generate-new-buffer "worker-subagent"))
                     (live-buf (generate-new-buffer "live-no-file-buffer"))
                     (live-buf-slash (generate-new-buffer "live/buffer/slash")))
                (unwind-protect
                    (progn
                      (expect (macher-agent--classify-file-path pure-buf "/mock/root") :to-equal 'buffer)
                      (expect (macher-agent--classify-file-path "worker-subagent" "/mock/root") :to-equal 'buffer)
                      (expect (macher-agent--classify-file-path live-buf "/mock/root") :to-equal 'buffer)
                      (expect (macher-agent--classify-file-path live-buf-slash "/mock/root") :to-equal 'buffer)
                      (expect (macher-agent--classify-file-path "src/main.rs" "/mock/root") :to-equal 'file)
                      (expect (macher-agent--classify-file-path "/mock/external/file.txt" "/mock/root") :to-equal 'external))
                  (kill-buffer pure-buf)
                  (kill-buffer live-buf)
                  (kill-buffer live-buf-slash))))

          (it "initialises untouched scoped buffer baseline symmetrically with zero diff output"
              (let* ((live-buf (generate-new-buffer "pure-scoped-state"))
                     (ctx (macher-agent--make-context :project-root "/mock/root/")))
                (with-current-buffer live-buf
                  (insert "Hello live buffer content"))
                (unwind-protect
                    (progn
                      (macher-agent--update-context-file ctx "pure-scoped-state" "Hello live buffer content")
                      (let ((entry (cl-find "pure-scoped-state" (macher-agent--get-context-contents ctx) :key #'car :test #'equal)))
                        (expect entry :not :to-be nil)
                        (expect (macher-agent-vfs-entry-orig entry) :to-equal "Hello live buffer content")
                        (expect (macher-agent-vfs-entry-curr entry) :to-equal "Hello live buffer content")
                        (expect (macher-agent-vfs-entry-modified-p entry) :to-be nil)
                        (let ((payload (list :context ctx :child-context ctx)))
                          (expect (plist-get (macher-agent-prepare-upstream-payloads payload) :diff) :to-be nil))))
                  (kill-buffer live-buf))))

          (it "computes safe workspace hash inspecting unwrapped project cons cells"
              (let ((ws-cons '(agent project . "/mock/project/path"))
                    (direct-project '(project . "/mock/project/path")))
                (expect (macher-agent--safe-workspace-hash ws-cons)
                        :to-equal (md5 "/mock/project/path"))
                (expect (macher-agent--safe-workspace-hash direct-project)
                        :to-equal (md5 "/mock/project/path"))))

          (it "merges contexts directly in macher-agent--merge-contexts"
              (let* ((parent-ctx (macher-agent--make-context
                                  :project-root "/mock/proj/"
                                  :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/proj/file.el" "v1" "v1"))))))
                     (child-ctx (macher-agent--make-context
                                 :project-root "/mock/proj/"
                                 :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/proj/file.el" "v1" "v2")))))))
                (macher-agent--merge-contexts parent-ctx child-ctx)
                (let ((entry (cl-find "/mock/proj/file.el" (macher-agent--get-context-contents parent-ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                  (expect (macher-agent-vfs-entry-curr entry) :to-equal "v2"))))

          (it "extracts context from alist using 'context symbol key in macher-agent-storage--extract-context"
              (let* ((ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (payload-symbol `((context . ,ctx)))
                     (payload-keyword `((:context . ,ctx)))
                     (payload-target-sym `((target-context . ,ctx)))
                     (payload-target-kw `((:target-context . ,ctx))))
                (expect (macher-agent-storage--extract-context payload-symbol) :to-be ctx)
                (expect (macher-agent-storage--extract-context payload-keyword) :to-be ctx)
                (expect (macher-agent-storage--extract-context payload-target-sym) :to-be ctx)
                (expect (macher-agent-storage--extract-context payload-target-kw) :to-be ctx)
                (expect (macher-agent-storage--extract-context (list :target-context ctx)) :to-be ctx)
                (expect (macher-agent-storage--extract-context '(project . "/mock/proj/")) :to-be nil)
                (expect (macher-agent-storage--extract-context '((project . "/mock/proj/"))) :to-be nil)
                (expect (macher-agent-storage--extract-context '(:project "/mock/proj/")) :to-be nil)
                (expect (macher-agent-storage--extract-context nil) :to-be nil)
                (expect (macher-agent-storage--extract-context "invalid-string") :to-be nil)
                (expect (macher-agent-storage--extract-context '(:non-context-key "foo")) :to-be nil)))

          (it "ensures trailing newline before separator in macher-agent--persist-vfs-to-hidden-buffer"
              (let* ((entries (list (macher-agent-vfs-make-entry "file-no-newline.txt" "orig" "content without newline")
                                    (macher-agent-vfs-make-entry "file-with-newline.txt" "orig" "content with newline\n")))
                     (ctx (macher-agent--make-context
                           :project-root "/mock/persist-test/"
                           :plugins (list :vfs (list :contents entries)))))
                (macher-agent--persist-vfs-to-hidden-buffer ctx)
                (let* ((buf-name (format " *macher-agent-vfs-state-%s*" (md5 (expand-file-name "/mock/persist-test/"))))
                       (vfs-buf (get-buffer buf-name)))
                  (expect vfs-buf :not :to-be nil)
                  (with-current-buffer vfs-buf
                    (expect (buffer-string) :to-match "content without newline\n=======================")
                    (expect (buffer-string) :to-match "content with newline\n======================="))
                  (when (buffer-live-p vfs-buf)
                    (kill-buffer vfs-buf)))))

          (it "computes deterministic expressive patch buffer names"
              (let* ((ws (cons 'project "/mock/my-project/"))
                     (hash (macher-agent--safe-workspace-hash ws 4))
                     (expr-phys (macher-agent--expressive-patch-buffer-name "physical" ws "agent-france"))
                     (expr-virt (macher-agent--expressive-patch-buffer-name "virtual" ws "agent-france"))
                     (expr-fallback (macher-agent--expressive-patch-buffer-name "physical" ws nil)))
                (expect expr-phys :to-equal (format "*macher-physical-patch:project@my-project<%s>[agent-france]*" hash))
                (expect expr-virt :to-equal (format "*macher-virtual-patch:project@my-project<%s>[agent-france]*" hash))
                (expect expr-fallback :to-equal (format "*macher-physical-patch:project@my-project<%s>*" hash))))

          (it "reuses existing live physical patch buffer across multiple flush operations"
              (let* ((ws (cons 'project "/mock/reuse-proj/"))
                     (orig-buf (generate-new-buffer "agent-reuse-test"))
                     (fsm (gptel-make-fsm :info (list :buffer orig-buf)))
                     (ctx1 (macher-agent--make-context
                            :project-root "/mock/reuse-proj/"
                            :origin-buffer orig-buf
                            :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/reuse-proj/file.el" "line1" "line1\nline2"))))))
                     (ctx2 (macher-agent--make-context
                            :project-root "/mock/reuse-proj/"
                            :origin-buffer orig-buf
                            :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/reuse-proj/file.el" "line1" "line1\nline2\nline3")))))))
                (unwind-protect
                    (let* ((buf1 (macher-agent--build-and-rename-patch ctx1 fsm "physical"))
                           (expected-name (macher-agent--expressive-patch-buffer-name "physical" ws orig-buf)))
                      (expect (buffer-live-p buf1) :to-be-truthy)
                      (expect (buffer-name buf1) :to-equal expected-name)
                      (expect (with-current-buffer buf1 (buffer-string)) :to-match "\\+line2")
                      ;; Subsequent flush should update patch buffer in place
                      (let ((buf2 (macher-agent--build-and-rename-patch ctx2 fsm "physical")))
                        (expect (buffer-live-p buf2) :to-be-truthy)
                        (expect (buffer-name buf2) :to-equal expected-name)
                        (expect (with-current-buffer buf2 (buffer-string)) :to-match "\\+line3")))
                  (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                  (when-let* ((b (get-buffer (macher-agent--expressive-patch-buffer-name "physical" ws "agent-reuse-test"))))
                    (kill-buffer b)))))

          (it "reuses existing live virtual patch buffer across multiple flush operations"
              (let* ((ws (cons 'project "/mock/virt-reuse-proj/"))
                     (orig-buf (generate-new-buffer "agent-virt-test"))
                     (fsm (gptel-make-fsm :info (list :buffer orig-buf)))
                     (ctx1 (macher-agent--make-context
                            :project-root "/mock/virt-reuse-proj/"
                            :origin-buffer orig-buf
                            :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "*virtual-doc*" "v1" "v2"))))))
                     (ctx2 (macher-agent--make-context
                            :project-root "/mock/virt-reuse-proj/"
                            :origin-buffer orig-buf
                            :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "*virtual-doc*" "v1" "v3")))))))
                (unwind-protect
                    (let* ((buf1 (macher-agent--build-and-rename-patch ctx1 fsm "virtual"))
                           (expected-name (macher-agent--expressive-patch-buffer-name "virtual" ws orig-buf)))
                      (expect (buffer-live-p buf1) :to-be-truthy)
                      (expect (buffer-name buf1) :to-equal expected-name)
                      (expect (with-current-buffer buf1 (buffer-string)) :to-match "\\+v2")
                      ;; Subsequent flush updates patch buffer in place
                      (let ((buf2 (macher-agent--build-and-rename-patch ctx2 fsm "virtual")))
                        (expect (buffer-live-p buf2) :to-be-truthy)
                        (expect (buffer-name buf2) :to-equal expected-name)
                        (expect (with-current-buffer buf2 (buffer-string)) :to-match "\\+v3")))
                  (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                  (when-let* ((b (get-buffer (macher-agent--expressive-patch-buffer-name "virtual" ws "agent-virt-test"))))
                    (kill-buffer b)))))

          (it "guarantees deterministic buffer uniqueness across distinct agent buffers in the same workspace"
              (when (get-buffer "agent-alpha") (kill-buffer (get-buffer "agent-alpha")))
              (when (get-buffer "agent-beta") (kill-buffer (get-buffer "agent-beta")))
              (let* ((ws (cons 'project "/mock/multi-agent-proj/"))
                     (agent1-buf (generate-new-buffer "agent-alpha"))
                     (agent2-buf (generate-new-buffer "agent-beta"))
                     (fsm1 (gptel-make-fsm :info (list :buffer agent1-buf)))
                     (fsm2 (gptel-make-fsm :info (list :buffer agent2-buf)))
                     (ctx1 (macher-agent--make-context
                            :project-root "/mock/multi-agent-proj/"
                            :origin-buffer agent1-buf
                            :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/multi-agent-proj/a.el" "old1" "new1"))))))
                     (ctx2 (macher-agent--make-context
                            :project-root "/mock/multi-agent-proj/"
                            :origin-buffer agent2-buf
                            :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/multi-agent-proj/b.el" "old2" "new2")))))))
                (unwind-protect
                    (let* ((pbuf1 (macher-agent--build-and-rename-patch ctx1 fsm1 "physical"))
                           (pbuf2 (macher-agent--build-and-rename-patch ctx2 fsm2 "physical"))
                           (vbuf1 (macher-agent--build-and-rename-patch ctx1 fsm1 "virtual"))
                           (vbuf2 (macher-agent--build-and-rename-patch ctx2 fsm2 "virtual")))
                      (expect pbuf1 :not :to-be pbuf2)
                      (expect (buffer-name pbuf1) :to-match "\\[agent-alpha\\]\\*$")
                      (expect (buffer-name pbuf2) :to-match "\\[agent-beta\\]\\*$")
                      (expect (buffer-name vbuf1) :to-match "\\[agent-alpha\\]\\*$")
                      (expect (buffer-name vbuf2) :to-match "\\[agent-beta\\]\\*$")
                      ;; Re-flushing agent1 updates agent1's buffer without affecting agent2
                      (let ((pbuf1-reused (macher-agent--build-and-rename-patch ctx1 fsm1 "physical")))
                        (expect (buffer-live-p pbuf1-reused) :to-be-truthy)
                        (expect (buffer-name pbuf1-reused) :to-match "\\[agent-alpha\\]\\*$")
                        (expect (buffer-live-p pbuf2) :to-be-truthy)))
                  (when (buffer-live-p agent1-buf) (kill-buffer agent1-buf))
                  (when (buffer-live-p agent2-buf) (kill-buffer agent2-buf))
                  (when-let* ((b (get-buffer (macher-agent--expressive-patch-buffer-name "physical" ws "agent-alpha"))))
                    (kill-buffer b))
                  (when-let* ((b (get-buffer (macher-agent--expressive-patch-buffer-name "physical" ws "agent-beta"))))
                    (kill-buffer b))
                  (when-let* ((b (get-buffer (macher-agent--expressive-patch-buffer-name "virtual" ws "agent-alpha"))))
                    (kill-buffer b))
                  (when-let* ((b (get-buffer (macher-agent--expressive-patch-buffer-name "virtual" ws "agent-beta"))))
                    (kill-buffer b)))))

          (it "restores expressive buffer name safely if macher-agent-macher-build-patch signals an error"
              (let* ((ws (cons 'project "/mock/error-proj/"))
                     (orig-buf (generate-new-buffer "agent-error-test"))
                     (fsm (gptel-make-fsm :info (list :buffer orig-buf)))
                     (ctx (macher-agent--make-context
                           :project-root "/mock/error-proj/"
                           :origin-buffer orig-buf
                           :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/error-proj/f.el" "1" "2"))))))
                     (expected-name (macher-agent--expressive-patch-buffer-name "physical" ws orig-buf)))
                (unwind-protect
                    (let ((buf (macher-agent--build-and-rename-patch ctx fsm "physical")))
                      (expect (buffer-live-p buf) :to-be-truthy)
                      (expect (buffer-name buf) :to-equal expected-name)
                      ;; Now simulate an error during next flush
                      (cl-letf (((symbol-function 'macher-agent-macher-build-patch)
                                 (lambda (&rest _) (error "Simulated patch build failure"))))
                        (expect (macher-agent--build-and-rename-patch ctx fsm "physical") :to-throw 'error))
                      ;; Buffer should retain expressive name
                      (expect (buffer-name buf) :to-equal expected-name))
                  (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                  (when-let* ((b (get-buffer expected-name))) (kill-buffer b)))))

          (it "executes multi-turn split patch flush reusing both physical and virtual buffers"
              (let* ((agent-buf (generate-new-buffer "agent-split-turn"))
                     (target-virt-buf (get-buffer-create "*app-scratch*"))
                     (fsm (gptel-make-fsm :info (list :buffer agent-buf :prompt "Turn 1")))
                     (gptel--fsm fsm)
                     (macher-agent--active-fsm fsm)
                     (ctx (macher-agent--make-context
                           :project-root "/mock/split-reuse/"
                           :origin-buffer agent-buf
                           :plugins (list :prompt "Turn 1"
                                          :vfs (list :dirty-p t
                                                     :contents (list (macher-agent-vfs-make-entry "/mock/split-reuse/main.rs" "fn a(){}" "fn a(){ 1 }")
                                                                     (macher-agent-vfs-make-entry "*app-scratch*" "init" "init modified")))))))
                (unwind-protect
                    (progn
                      (setq gptel--fsm fsm)
                      (setq macher-agent--active-fsm fsm)
                      (setf (gptel-fsm-info fsm) (list :buffer agent-buf :prompt "Turn 1" :macher-agent-context ctx))
                      ;; Turn 1 flush
                      (macher-agent-vfs-build-patch-from-hook ctx)
                      (let* ((ws (cons 'project "/mock/split-reuse/"))
                             (p-name (macher-agent--expressive-patch-buffer-name "physical" ws agent-buf))
                             (v-name (macher-agent--expressive-patch-buffer-name "virtual" ws agent-buf))
                             (p-buf1 (get-buffer p-name))
                             (v-buf1 (get-buffer v-name)))
                        (expect (buffer-live-p p-buf1) :to-be-truthy)
                        (expect (buffer-live-p v-buf1) :to-be-truthy)
                        ;; Turn 2 flush with further modifications
                        (macher-agent--update-context-file ctx "/mock/split-reuse/main.rs" "fn a(){ 2 }")
                        (macher-agent--update-context-file ctx "*app-scratch*" "init modified again")
                        (macher-agent-vfs-build-patch-from-hook ctx)
                        (let ((p-buf2 (get-buffer p-name))
                              (v-buf2 (get-buffer v-name)))
                          (expect (buffer-live-p p-buf2) :to-be-truthy)
                          (expect (buffer-live-p v-buf2) :to-be-truthy))))
                  (setq gptel--fsm nil)
                  (setq macher-agent--active-fsm nil)
                  (when (buffer-live-p agent-buf) (kill-buffer agent-buf))
                  (when (buffer-live-p target-virt-buf) (kill-buffer target-virt-buf))
                  (let* ((ws (cons 'project "/mock/split-reuse/"))
                         (p-name (macher-agent--expressive-patch-buffer-name "physical" ws "agent-split-turn"))
                         (v-name (macher-agent--expressive-patch-buffer-name "virtual" ws "agent-split-turn")))
                    (when-let* ((b (get-buffer p-name))) (kill-buffer b))
                    (when-let* ((b (get-buffer v-name))) (kill-buffer b)))))))

(provide 'macher-agent-vfs-sync-test)
;;; macher-agent-vfs-sync-test.el ends here


