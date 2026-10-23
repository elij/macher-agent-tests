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

              ;; Stored as virtual buffer entry in :vfs-buffers and :contents
              (expect (gethash "*in-memory-scratch*" (macher-agent-workspace-vfs-buffers ctx))
                      :to-equal "staged virtual content")
              (let ((entry (cl-find "*in-memory-scratch*" (macher-agent--get-context-contents ctx)
                                    :key #'macher-agent-vfs-entry-path :test #'equal)))
                (expect entry :not :to-be nil)
                (expect (macher-agent-vfs-entry-curr entry) :to-equal "staged virtual content"))

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
              (expect (cl-find "*temp-vbuf*" (macher-agent--get-context-contents ctx)
                               :key #'macher-agent-vfs-entry-path :test #'equal)
                      :to-be nil)

              ;; 2. Delete physical file entry by passing nil content
              (macher-agent--update-context-file ctx "tracked.el" nil)
              (let ((p-entry (cl-find (expand-file-name "tracked.el" root) (macher-agent--get-context-contents ctx)
                                      :key #'macher-agent-vfs-entry-path :test #'file-equal-p)))
                (expect p-entry :not :to-be nil)
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
        (let ((entry (cl-find (expand-file-name "config.json" root) (macher-agent--get-context-contents ctx)
                              :key #'macher-agent-vfs-entry-path :test #'file-equal-p)))
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
            (kill-buffer patch-buf)))))))

(provide 'test-vfs)
;;; test-vfs.el ends here
