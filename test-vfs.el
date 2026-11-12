;;; test-vfs.el --- Virtual file system (VFS) and optimistic concurrency -*- lexical-binding: t; -*-

;;; Commentary:
;; Specification 3: Virtual file system (VFS) and optimistic concurrency
;; VFS-01, VFS-02, VFS-03, VFS-04, VFS-05, VFS-06, VFS-07, VFS-08, VFS-09

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-test-helpers)

(describe "3. Virtual file system (VFS) and optimistic concurrency"

  (before-each
    (macher-agent-test-reset-env))

  ;; --------------------------------------------------------------------------
  ;; VFS-01
  ;; --------------------------------------------------------------------------
  (it "VFS-01: modifies tracked file and creates new file in VFS without touching physical disk"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("README.md" . "# Original README\n")))
      (let ((initial-readme (with-temp-buffer
                              (insert-file-contents (expand-file-name "README.md" root))
                              (buffer-string))))
        ;; Modify tracked file in VFS
        (macher-agent--update-context-file ctx "README.md" "# Modified in VFS\n")
        ;; Create new file in VFS
        (macher-agent--update-context-file ctx "new-doc.md" "# Brand New Document\n")

        ;; Subsequent reads return updated VFS content
        (expect (macher-agent--read-context-file ctx "README.md")
                :to-equal "# Modified in VFS\n")
        (expect (macher-agent--read-context-file ctx "new-doc.md")
                :to-equal "# Brand New Document\n")

        ;; Dirty flag is set
        (expect (macher-agent--get-context-dirty-p ctx) :to-be t)

        ;; Physical files on disk remain untouched
        (expect (with-temp-buffer
                  (insert-file-contents (expand-file-name "README.md" root))
                  (buffer-string))
                :to-equal initial-readme)
        (expect (file-exists-p (expand-file-name "new-doc.md" root)) :to-be nil))))

  ;; --------------------------------------------------------------------------
  ;; VFS-02
  ;; --------------------------------------------------------------------------
  (it "VFS-02: creates and edits in-memory buffer in VFS and applies only on macher-agent-apply-virtual-buffers"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (let ((vbuf (get-buffer-create "*in-memory-scratch*")))
        (unwind-protect
            (progn
              (with-current-buffer vbuf
                (erase-buffer)
                (insert "initial buffer text"))
              ;; Edit via workspace buffer tool
              (funcall (get 'macher-agent-write-buffer-in-workspace-tool 'ptc-function)
                       "*in-memory-scratch*" "staged virtual content" ctx)

              ;; Stored as virtual buffer entry in :vfs-buffers (not :contents)
              (let ((entry (gethash "*in-memory-scratch*" (macher-agent-workspace-vfs-buffers ctx))))
                (expect (macher-agent-vfs-entry-p entry) :to-be t)
                (expect (macher-agent-vfs-entry-curr entry) :to-equal "staged virtual content"))
              (expect (cl-find "*in-memory-scratch*" (macher-agent--get-context-contents ctx)
                               :key #'macher-agent-vfs-entry-path :test #'equal)
                      :to-be nil)

              ;; Live buffer has NOT mutated yet
              (expect (with-current-buffer vbuf (buffer-string))
                      :to-equal "initial buffer text")

              ;; Render into live buffer on macher-agent-apply-virtual-buffers
              (macher-agent-apply-virtual-buffers ctx)
              (expect (with-current-buffer vbuf (buffer-string))
                      :to-equal "staged virtual content"))
          (when (buffer-live-p vbuf)
            (kill-buffer vbuf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-03
  ;; --------------------------------------------------------------------------
  (it "VFS-03: deletes tracked VFS entries with nil content, purging virtual buffers and marking physical deletions"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("tracked.el" . "(defvar x 1)")))
      (let ((vbuf (get-buffer-create "*temp-vbuf*")))
        (unwind-protect
            (progn
              ;; Stage virtual buffer
              (funcall (get 'macher-agent-write-buffer-in-workspace-tool 'ptc-function)
                       "*temp-vbuf*" "vbuf content" ctx)
              ;; Stage physical file
              (macher-agent--update-context-file ctx "tracked.el" "(defvar x 2)")

              ;; 1. Delete virtual buffer entry by passing nil content
              (macher-agent--update-context-file ctx "*temp-vbuf*" nil)
              (expect (gethash "*temp-vbuf*" (macher-agent-workspace-vfs-buffers ctx)) :to-be nil)

              ;; 2. Delete physical file entry by passing nil content
              (macher-agent--update-context-file ctx "tracked.el" nil)
              (let ((p-entry (cl-find "tracked.el" (macher-agent--get-context-contents ctx)
                                      :key #'macher-agent-vfs-entry-path :test #'equal)))
                (expect p-entry :not :to-be nil)
                (expect (macher-agent-vfs-entry-path p-entry) :to-equal "tracked.el")
                ;; curr content is set to nil so diff represents file deletion
                (expect (macher-agent-vfs-entry-curr p-entry) :to-be nil)
                (expect (macher-agent-vfs-entry-modified-p p-entry) :to-be t)))
          (when (buffer-live-p vbuf)
            (kill-buffer vbuf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-04
  ;; --------------------------------------------------------------------------
  (it "VFS-04: detects disk modifications, synchronises baseline orig and curr, and clears dirty flag"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("config.json" . "{\"version\": 1}")))
      (let ((file-path (expand-file-name "config.json" root))
            (message-captured nil))
        ;; Stage agent edit in VFS
        (macher-agent--update-context-file ctx "config.json" "{\"version\": 2, \"agent\": true}")
        (expect (macher-agent--get-context-dirty-p ctx) :to-be t)

        ;; Modify physical file on disk outside agent
        (with-temp-file file-path
          (insert "{\"version\": 3, \"external\": true}"))

        ;; Trigger auto-sync and capture replacement message
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-captured (apply #'format fmt args)))))
          (macher-agent--auto-sync-context ctx))

        ;; VFS replaces both orig and curr with new disk content
        (let ((entry (cl-find "config.json" (macher-agent--get-context-contents ctx)
                              :key #'macher-agent-vfs-entry-path :test #'equal)))
          (expect entry :not :to-be nil)
          (expect (macher-agent-vfs-entry-path entry) :to-equal "config.json")
          (expect (macher-agent-vfs-entry-orig entry) :to-equal "{\"version\": 3, \"external\": true}")
          (expect (macher-agent-vfs-entry-curr entry) :to-equal "{\"version\": 3, \"external\": true}")
          (expect (macher-agent-vfs-entry-modified-p entry) :to-be nil))

        ;; Dirty flag is cleared when no other files modified
        (expect (macher-agent--get-context-dirty-p ctx) :to-be nil)
        ;; Replacement message emitted
        (expect message-captured :to-match "Replaced entry 'config.json' due to external edits"))))

  ;; --------------------------------------------------------------------------
  ;; VFS-05
  ;; --------------------------------------------------------------------------
  (it "VFS-05: prefers unsaved live Emacs buffer content over older on-disk file content"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("module.el" . ";; Disk Version\n")))
      (let* ((file-path (expand-file-name "module.el" root))
             (live-buf (find-file-noselect file-path)))
        (unwind-protect
            (progn
              ;; Make unsaved edit in live buffer
              (with-current-buffer live-buf
                (goto-char (point-max))
                (insert "(defun live-unsaved-fn () t)\n")
                (expect (buffer-modified-p) :to-be t))
              ;; Read through VFS
              (let ((vfs-read (macher-agent--read-context-file ctx "module.el")))
                ;; Expects the live modified buffer content
                (expect vfs-read :to-match "live-unsaved-fn")
                (expect (string-match-p "live-unsaved-fn"
                                        (with-temp-buffer
                                          (insert-file-contents file-path)
                                          (buffer-string)))
                        :to-be nil)))
          (when (buffer-live-p live-buf)
            (with-current-buffer live-buf
              (set-buffer-modified-p nil))
            (kill-buffer live-buf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-06
  ;; --------------------------------------------------------------------------
  (it "VFS-06: blocks path traversal via relative segments or symlinks outside workspace root"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      ;; Directory climbing via ../
      (expect (macher-agent--resolve-safe-path "../outside.txt" root)
              :to-throw 'error)
      (expect (macher-agent--resolve-safe-path "../../../../etc/passwd" root)
              :to-throw 'error)
      ;; Symlink resolving outside root
      (let* ((outside-dir (make-temp-file "macher-outside-" t))
             (outside-target (expand-file-name "secret.txt" outside-dir))
             (symlink-path (expand-file-name "escape_link" root)))
        (unwind-protect
            (progn
              (with-temp-file outside-target (insert "secret data"))
              (make-symbolic-link outside-dir symlink-path t)
              (expect (macher-agent--resolve-safe-path "escape_link/secret.txt" root)
                      :to-throw 'error))
          (ignore-errors (delete-file symlink-path))
          (ignore-errors (delete-directory outside-dir t))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-07
  ;; --------------------------------------------------------------------------
  (it "VFS-07: merges sub-agent VFS entries into parent context with zero sub-agent patch buffers"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
      (let* ((child-buf (macher-agent-add-subagent "*test-vfs-07-child*" '("default") parent-buf nil parent-ctx))
             (child-ctx (with-current-buffer child-buf macher-agent--persistent-context)))
        (unwind-protect
            (progn
              (macher-agent-register-task-callback
               "task-vfs-07"
               (lambda (msg)
                 (when (macher-agent-transit-payload-p msg)
                   (setf (macher-agent-transit-payload-target-context msg) parent-ctx))
                 (when (fboundp 'macher-agent-run-pipeline)
                   (macher-agent-run-pipeline 'payload-merge msg))))
              ;; Sub-agent operates with suppressed patches
              (with-current-buffer child-buf
                (setq-local macher-agent--suppress-patch t)
                (macher-agent--push-routing "task-vfs-07" (buffer-name parent-buf) t)
                ;; Modify two files
                (macher-agent--update-context-file child-ctx "a.txt" "content A")
                (macher-agent--update-context-file child-ctx "b.txt" "content B")
                ;; Submit task result
                (let ((submit-fn (get 'macher-agent-submit-task-result-tool 'ptc-function)))
                  (funcall submit-fn "Completed subtask" nil child-ctx))
                ;; Simulate FSM reaching DONE state
                (let ((child-fsm (gptel-make-fsm :info (list :buffer child-buf :macher-agent-context child-ctx)
                                                 :state 'DONE)))
                  (macher-agent-gptel--trigger-flush child-fsm)))

              ;; Sub-agent's VFS entries merge into parent orchestrator's context
              (expect (macher-agent--read-context-file parent-ctx "a.txt") :to-equal "content A")
              (expect (macher-agent--read-context-file parent-ctx "b.txt") :to-equal "content B")

              ;; Zero patch buffers created or displayed for child
              (let ((patch-bufs (cl-remove-if-not
                                 (lambda (b)
                                   (string-match-p "\\*macher-.*-patch:.*test-vfs-07-child.*\\*" (buffer-name b)))
                                 (buffer-list))))
                (expect (length patch-bufs) :to-equal 0)))
          (when (buffer-live-p child-buf)
            (kill-buffer child-buf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-08
  ;; --------------------------------------------------------------------------
  (it "VFS-08: generates exactly one diff patch and one buffer patch in split windows for root turn"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("app.el" . "(defvar app 1)")))
      (let ((vbuf (get-buffer-create "*vfs-08-buf*")))
        (unwind-protect
            (with-current-buffer owner-buf
              (setq-local macher-agent--suppress-patch nil)
              ;; Modify physical file
              (macher-agent--update-context-file ctx "app.el" "(defvar app 2)")
              ;; Modify virtual buffer
              (funcall (get 'macher-agent-write-buffer-in-workspace-tool 'ptc-function)
                       "*vfs-08-buf*" "new virtual app" ctx)

              ;; Stub upstream patch builder to produce distinct mock patch buffers
              (cl-letf (((symbol-function 'macher-agent-macher-build-patch)
                         (lambda (_ctx _prompt files)
                           (let* ((is-virtual (cl-some (lambda (e) (string-prefix-p "*" (macher-agent-vfs-entry-path e))) files))
                                  (raw-buf (generate-new-buffer (if is-virtual "*macher-patch-buf*" "*macher-patch*"))))
                             raw-buf))))
                (macher-agent-vfs-build-patch-from-hook ctx))

              ;; Exactly one diff patch and one buffer patch buffer exist
              (let ((diff-patches (cl-remove-if-not
                                   (lambda (b) (string-prefix-p "*macher-diff-patch:" (buffer-name b)))
                                   (buffer-list)))
                    (buf-patches (cl-remove-if-not
                                  (lambda (b) (string-prefix-p "*macher-buffer-patch:" (buffer-name b)))
                                  (buffer-list))))
                (expect (length diff-patches) :to-equal 1)
                (expect (length buf-patches) :to-equal 1)
                ;; Raw upstream *macher-patch* buffers renamed in place, not duplicated
                (expect (get-buffer "*macher-patch*") :to-be nil)
                ;; Windows are split
                (expect (> (length (window-list)) 1) :to-be t)))
          (when (buffer-live-p vbuf) (kill-buffer vbuf))
          (dolist (b (buffer-list))
            (when (string-match-p "\\*macher-.*-patch:" (buffer-name b))
              (kill-buffer b)))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-09
  ;; --------------------------------------------------------------------------
  (it "VFS-09: applies generated diff from patch buffer to physical files in git workspace and cleans output buffer"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("file.txt" . "alpha\nbeta\n")))
      (let* ((patch-content (format "--- a/file.txt\n+++ b/file.txt\n@@ -1,2 +1,2 @@\n alpha\n-beta\n+BETA_MODIFIED\n"))
             (patch-buf (generate-new-buffer "*macher-diff-patch:test*")))
        (unwind-protect
            (with-current-buffer patch-buf
              (diff-mode)
              (setq-local default-directory root)
              (setq-local macher-agent--persistent-context ctx)
              (insert patch-content)
              ;; Apply patch
              (macher-agent-apply-patch)
              ;; Physical file on disk is modified
              (let ((file-content (with-temp-buffer
                                    (insert-file-contents (expand-file-name "file.txt" root))
                                    (buffer-string))))
                (expect file-content :to-equal "alpha\nBETA_MODIFIED\n"))
              ;; Output buffer *macher-patch-out* is cleaned up on success
              (expect (get-buffer "*macher-patch-out*") :to-be nil))
          (when (buffer-live-p patch-buf)
            (kill-buffer patch-buf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-10
  ;; --------------------------------------------------------------------------
  (it "VFS-10: prevents sibling baseline entries from clobbering concurrent worker edits"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf '(("shared.txt" . "disk-base\n")))
      ;; Stage shared.txt in parent context
      (macher-agent--update-context-file parent-ctx "shared.txt" "base\n")
      ;; Child 1 and Child 2 cloned from parent context
      (let* ((child1-ctx (macher-agent--clone-context parent-ctx))
             (child2-ctx (macher-agent--clone-context parent-ctx)))
        ;; Initially both child entries are baseline (pending-patch is nil)
        (let ((c1-entry (cl-find "shared.txt"
                                 (macher-agent--get-context-contents child1-ctx)
                                 :key #'macher-agent-vfs-entry-path :test #'equal))
              (c2-entry (cl-find "shared.txt"
                                 (macher-agent--get-context-contents child2-ctx)
                                 :key #'macher-agent-vfs-entry-path :test #'equal)))
          (expect (macher-agent-vfs-entry-pending-patch c1-entry) :to-be nil)
          (expect (macher-agent-vfs-entry-pending-patch c2-entry) :to-be nil))

        ;; Child 1 modifies shared.txt (pending-patch becomes t)
        (macher-agent--update-context-file child1-ctx "shared.txt" "worker1-content\n")

        ;; Child 2 modifies a different file (other.txt), leaving shared.txt untouched
        (macher-agent--update-context-file child2-ctx "other.txt" "worker2-content\n")

        ;; 1. Merge Child 1 into parent
        (macher-agent--merge-contexts parent-ctx child1-ctx)
        (expect (macher-agent--read-context-file parent-ctx "shared.txt")
                :to-equal "worker1-content\n")

        ;; 2. Merge Child 2 into parent: Child 2's untouched shared.txt baseline MUST NOT
        ;; overwrite Child 1's "worker1-content", while other.txt is cleanly merged.
        (macher-agent--merge-contexts parent-ctx child2-ctx)
        (expect (macher-agent--read-context-file parent-ctx "shared.txt")
                :to-equal "worker1-content\n")
        (expect (macher-agent--read-context-file parent-ctx "other.txt")
                :to-equal "worker2-content\n"))))

  ;; --------------------------------------------------------------------------
  ;; VFS-11
  ;; --------------------------------------------------------------------------
  (it "VFS-11: ignores zero-diff updates, suppresses redundant hooks, and restores dirty state on revert"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("sample.txt" . "original content\n")))
      (let ((hook-calls 0))
        (add-hook 'macher-agent-context-mutated-hook
                  (lambda (&rest _) (setq hook-calls (1+ hook-calls))))
        (unwind-protect
            (progn
              ;; 1. Writing identical content to untracked clean file is zero-diff no-op
              (macher-agent--update-context-file ctx "sample.txt" "original content\n")
              (expect hook-calls :to-equal 0)
              (expect (macher-agent--get-context-dirty-p ctx) :to-be nil)

              ;; 2. Mutating content fires hook and marks dirty
              (macher-agent--update-context-file ctx "sample.txt" "modified content\n")
              (expect hook-calls :to-equal 1)
              (expect (macher-agent--get-context-dirty-p ctx) :to-be t)

              ;; 3. Writing the same mutated content again is zero-diff: hook not fired
              (macher-agent--update-context-file ctx "sample.txt" "modified content\n")
              (expect hook-calls :to-equal 1)

              ;; 4. Reverting back to original content removes zero-diff entry and clears dirty flag
              (macher-agent--update-context-file ctx "sample.txt" "original content\n")
              (expect hook-calls :to-equal 2)
              (expect (macher-agent--get-context-dirty-p ctx) :to-be nil)
              (expect (macher-agent--read-context-file ctx "sample.txt")
                      :to-equal "original content\n"))
          (setq macher-agent-context-mutated-hook nil)))))

  ;; --------------------------------------------------------------------------
  ;; VFS-12
  ;; --------------------------------------------------------------------------
  (it "VFS-12: releases actor locks and transfers to queued waiters via macher-agent-vfs-release-actor-locks"
    (let ((f1 "src/f1.rs")
          (f2 "src/f2.rs")
          (f3 "src/f3.rs")
          (t2-called nil)
          (t4-called nil))
      (unwind-protect
          (progn
            ;; Actor 1 holds locks on f1 and f2
            (macher-agent--vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-1" :resource_path f1 :callback #'ignore))
            (macher-agent--vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-1" :resource_path f2 :callback #'ignore))

            ;; Actor 2 and Actor 3 wait for f1
            (macher-agent--vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-2" :resource_path f1
                   :callback (lambda (_m) (setq t2-called t))))
            (macher-agent--vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-3" :resource_path f1 :callback #'ignore))

            ;; Actor 4 waits for f2
            (macher-agent--vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-4" :resource_path f2
                   :callback (lambda (_m) (setq t4-called t))))

            ;; Actor 5 holds f3, Actor 1 is queued on f3
            (macher-agent--vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-5" :resource_path f3 :callback #'ignore))
            (macher-agent--vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-1" :resource_path f3 :callback #'ignore))

            ;; Release locks for actor-1
            (macher-agent-vfs-release-actor-locks '("actor-1"))

            ;; f1 transferred to actor-2
            (expect t2-called :to-be t)
            (expect (car (gethash f1 macher-agent--vfs-lock-table)) :to-equal "actor-2")
            ;; actor-3 still in queue for f1
            (expect (length (gethash f1 macher-agent--vfs-lock-queues)) :to-equal 1)

            ;; f2 transferred to actor-4
            (expect t4-called :to-be t)
            (expect (car (gethash f2 macher-agent--vfs-lock-table)) :to-equal "actor-4")

            ;; actor-1 removed from f3 waiting queue; actor-5 still holds f3
            (expect (car (gethash f3 macher-agent--vfs-lock-table)) :to-equal "actor-5")
            (expect (gethash f3 macher-agent--vfs-lock-queues) :to-be nil))

        ;; Cleanup
        (macher-agent-vfs-release-actor-locks '("actor-2" "actor-3" "actor-4" "actor-5")))))

  ;; --------------------------------------------------------------------------
  ;; VFS-13
  ;; --------------------------------------------------------------------------
  (it "VFS-13: enforces workspace-relative VFS keys across direct updates, standalone macher tools, and secondary buffers"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("README.md" . "# Initial\n")))
      ;; 1. Updating with "./" prefix or absolute workspace path normalises to relative key
      (macher-agent--update-context-file ctx "./src/rel.el" "(provide 'rel)\n")
      (macher-agent--update-context-file ctx (expand-file-name "nested/abs.el" root) "(provide 'abs)\n")
      (let ((listed (macher-agent-vfs--workspace-list-files ctx)))
        (expect (member "src/rel.el" listed) :to-be-truthy)
        (expect (member "nested/abs.el" listed) :to-be-truthy)
        (expect (cl-every (lambda (p) (not (file-name-absolute-p p))) listed) :to-be t))

      ;; 2. Wrapped upstream macher tool called standalone (without orphaned macher-context)
      ;; stores relative path and drops read-only zero-diff entries
      (let* ((write-tool (gptel-make-tool
                          :name "test_macher_write"
                          :description "Write file"
                          :category "macher"
                          :async t
                          :function (lambda (m-ctx cb rel-path text)
                                      (macher-context--set-new-content-for-file
                                       (expand-file-name rel-path root) text m-ctx)
                                      (funcall cb "wrote"))))
             (read-tool (gptel-make-tool
                         :name "test_macher_read"
                         :description "Read file"
                         :category "macher"
                         :async t
                         :function (lambda (m-ctx cb rel-path)
                                     (let ((pair (macher-context--contents-for-file
                                                  (expand-file-name rel-path root) m-ctx)))
                                       (funcall cb (cdr pair))))))
             (cb-out nil))
        (macher-agent--wrap-single-tool write-tool)
        (macher-agent--wrap-single-tool read-tool)
        (with-current-buffer owner-buf
          ;; Invoke WITHOUT prepending an upstream macher-context struct
          (funcall (gptel-tool-function read-tool)
                   (lambda (r) (setq cb-out r)) "README.md")
          (expect cb-out :to-equal "# Initial\n")
          ;; Read-only access must NOT add unmodified README.md into VFS :contents
          (expect (cl-find "README.md" (macher-agent--get-context-contents ctx)
                           :key #'macher-agent-vfs-entry-path :test #'equal)
                  :to-be nil)
          ;; Write via wrapped macher tool stores canonical relative key
          (funcall (gptel-tool-function write-tool)
                   (lambda (r) (setq cb-out r)) "via-proxy/new.el" "(setq ok t)\n")
          (let ((proxy-entry (cl-find "via-proxy/new.el" (macher-agent--get-context-contents ctx)
                                      :key #'macher-agent-vfs-entry-path :test #'equal)))
            (expect proxy-entry :not :to-be nil)
            (expect (macher-agent-vfs-entry-curr proxy-entry) :to-equal "(setq ok t)\n"))))

      ;; 3. Secondary buffer enabling macher-agent-mode must NOT overwrite primary owner-buf in active-workspaces
      (let ((sec-buf (generate-new-buffer "*vfs-13-secondary*")))
        (unwind-protect
            (with-current-buffer sec-buf
              (setq-local default-directory root)
              (macher-agent-mode 1)
              (expect (gethash root macher-agent-active-workspaces) :to-be ctx))
          (when (buffer-live-p sec-buf)
            (kill-buffer sec-buf)))))))

(provide 'test-vfs)
;;; test-vfs.el ends here
