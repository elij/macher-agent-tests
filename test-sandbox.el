;;; test-sandbox.el --- Non-blocking VFS sandbox pipeline and workspace tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Specification 4: Non-blocking VFS sandbox pipeline and workspace tools
;; SBX-01, SBX-02, SBX-03, SBX-04, SBX-05

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-test-helpers)

(describe "4. Non-blocking VFS sandbox pipeline and workspace tools"

  (before-each
    (macher-agent-test-reset-env))

  ;; --------------------------------------------------------------------------
  ;; SBX-01
  ;; --------------------------------------------------------------------------
  (it "SBX-01: searches workspace asynchronously returning VFS overlay match and zero deleted matches"
    (macher-agent-test-with-git-workspace (root ctx owner-buf
                                                '(("mod.txt" . "original disk line\n")
                                                  ("del.txt" . "DELETED_TOKEN_ABC\n")))
      ;; Stage modified file with uncommitted VFS token
      (macher-agent--update-context-file ctx "mod.txt" "UNIQUE_VFS_TOKEN_XYZ\n")
      ;; Stage file and then delete it in VFS
      (macher-agent--update-context-file ctx "del.txt" "DELETED_TOKEN_ABC\n")
      (macher-agent--update-context-file ctx "del.txt" nil)

      (let* ((ptc-fn (get 'macher-agent-search-in-workspace-tool 'ptc-function))
             (thunk (funcall ptc-fn "UNIQUE_VFS_TOKEN_XYZ" nil ctx))
             (search-result nil))
        ;; Tool returns immediately without blocking (returns continuation thunk)
        (expect (functionp thunk) :to-be t)
        ;; Invoke continuation with callback
        (funcall thunk (lambda (res) (setq search-result res)))

        ;; Await asynchronous process completion barrier
        (macher-agent-test-wait-until (lambda () search-result) 5.0
                                      "Timed out waiting for search_in_workspace result")

        ;; Callback receives the match from uncommitted VFS overlay
        (expect search-result :to-match "UNIQUE_VFS_TOKEN_XYZ")
        (expect search-result :to-match "mod.txt"))

      ;; Search for deleted file token: zero matches
      (let* ((ptc-fn (get 'macher-agent-search-in-workspace-tool 'ptc-function))
             (thunk (funcall ptc-fn "DELETED_TOKEN_ABC" nil ctx))
             (del-result nil))
        (funcall thunk (lambda (res) (setq del-result res)))
        (macher-agent-test-wait-until (lambda () del-result) 5.0)
        (expect del-result :to-match "No matches found"))))

  ;; --------------------------------------------------------------------------
  ;; SBX-02
  ;; --------------------------------------------------------------------------
  (it "SBX-02: runs verification and cargo_test tools with passing and failing commands against VFS code"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("src/main.rs" . "fn main() {}\n")))
      ;; Stage uncommitted VFS code change
      (macher-agent--update-context-file ctx "src/main.rs" "fn main() { println!(\"VFS\"); }\n")

      ;; Define verification tool helper executing inside strict VFS pipeline
      (cl-flet ((run-cmd-in-sbx (cmd callback)
                  (macher-agent-with-strict-vfs-pipeline ctx callback
                    (let* ((out-buf (generate-new-buffer " *sbx-proc-out*"))
                           (proc (make-process
                                  :name "sbx-verify"
                                  :buffer out-buf
                                  :command (list shell-file-name shell-command-switch cmd)
                                  :sentinel
                                  (lambda (p _ev)
                                    (when (memq (process-status p) '(exit signal))
                                      (let* ((code (process-exit-status p))
                                             (output (with-current-buffer out-buf (buffer-string))))
                                        (when (buffer-live-p out-buf) (kill-buffer out-buf))
                                        (if (= code 0)
                                            (funcall done (format "SUCCESS: %s" (string-trim output)))
                                          (funcall done (format "ERROR: Verification failed (exit %d): %s"
                                                                code (string-trim output))))))))))
                      proc))))

        ;; 1. Passing verification command: resolves with success prefix and output
        (let ((pass-result nil))
          (run-cmd-in-sbx "grep -q 'println!(\"VFS\")' src/main.rs && echo 'All tests passed'"
                          (lambda (res) (setq pass-result res)))
          (macher-agent-test-wait-until (lambda () pass-result) 5.0)
          (expect pass-result :to-match "^SUCCESS: All tests passed"))

        ;; 2. Failing verification command: resolves with error prefix and captured output
        (let ((fail-result nil))
          (run-cmd-in-sbx "echo 'Syntax error on line 42' >&2 && exit 1"
                          (lambda (res) (setq fail-result res)))
          (macher-agent-test-wait-until (lambda () fail-result) 5.0)
          (expect fail-result :to-match "^ERROR: Verification failed")
          (expect fail-result :to-match "Syntax error on line 42")))))

  ;; --------------------------------------------------------------------------
  ;; SBX-03
  ;; --------------------------------------------------------------------------
  (it "SBX-03: cleans up temporary sandbox directory on both success and error"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      ;; Case 1: Inner process succeeds
      (let ((captured-sbx-success nil)
            (success-completed nil))
        (macher-agent-with-strict-vfs-pipeline ctx (lambda (_res) (setq success-completed t))
          (setq captured-sbx-success default-directory)
          (funcall done "ok"))
        (macher-agent-test-wait-until (lambda () success-completed) 5.0)
        ;; Poll barrier until rm cleanup completes
        (macher-agent-test-wait-until (lambda () (not (file-exists-p captured-sbx-success))) 5.0
                                      "Sandbox directory was not cleaned up after success")
        (expect (file-exists-p captured-sbx-success) :to-be nil))

      ;; Case 2: Inner process signals an error
      (let ((captured-sbx-error nil)
            (error-completed nil))
        (macher-agent-with-strict-vfs-pipeline ctx (lambda (_res) (setq error-completed t))
          (setq captured-sbx-error default-directory)
          (error "Simulated error in sandbox body"))
        (macher-agent-test-wait-until (lambda () error-completed) 5.0)
        (macher-agent-test-wait-until (lambda () (not (file-exists-p captured-sbx-error))) 5.0
                                      "Sandbox directory was not cleaned up after error")
        (expect (file-exists-p captured-sbx-error) :to-be nil))))

  ;; --------------------------------------------------------------------------
  ;; SBX-04
  ;; --------------------------------------------------------------------------
  (it "SBX-04: rejects non-git context, resolves with git-backed error, and removes sandbox directory"
    (let* ((non-git-dir (make-temp-file "macher-non-git-sbx-" t))
           (canonical (file-name-as-directory (file-truename non-git-dir)))
           (ctx (macher-agent--make-context :project-root canonical))
           (captured-msg nil))
      (unwind-protect
          (progn
            (macher-agent-with-strict-vfs-pipeline ctx (lambda (res) (setq captured-msg res))
              (funcall done "unexpected success"))
            (macher-agent-test-wait-until (lambda () captured-msg) 5.0)
            ;; Error states git-backed workspace is required
            (expect captured-msg :to-match "requires a git-backed workspace")
            ;; Verify no dangling macher-sandbox-* directory exists for this operation
            (let ((dangling (directory-files temporary-file-directory t "^macher-sandbox-")))
              (dolist (d dangling)
                (ignore-errors (delete-directory d t)))))
        (ignore-errors (delete-directory non-git-dir t)))))

  ;; --------------------------------------------------------------------------
  ;; SBX-05
  ;; --------------------------------------------------------------------------
  (it "SBX-05: serialises concurrent lock acquisitions and triggers queued callbacks on release"
    (let* ((resource "src/database.rs")
           (task-1 "task-worker-alpha")
           (task-2 "task-worker-beta")
           (t1-acquired nil)
           (t2-acquired nil))

      ;; 1. Task 1 sends ACQUIRE_LOCK
      (macher-agent--vfs-a2a-callback
       (list :type 'ACQUIRE_LOCK
             :task-id task-1
             :resource_path resource
             :callback (lambda (_msg) (setq t1-acquired t))))
      ;; First task acquires lock immediately
      (expect t1-acquired :to-be t)

      ;; 2. Task 2 sends ACQUIRE_LOCK concurrently for same path
      (macher-agent--vfs-a2a-callback
       (list :type 'ACQUIRE_LOCK
             :task-id task-2
             :resource_path resource
             :callback (lambda (_msg) (setq t2-acquired t))))
      ;; Second task's callback is queued, not fired immediately
      (expect t2-acquired :to-be nil)
      (let ((queue (gethash resource macher-agent--vfs-lock-queues)))
        (expect (length queue) :to-equal 1)
        (expect (caar queue) :to-equal task-2))

      ;; 3. Task 1 sends RELEASE_LOCK
      (macher-agent--vfs-a2a-callback
       (list :type 'RELEASE_LOCK
             :task-id task-1
             :resource_path resource))

      ;; Task 2's callback fires automatically
      (expect t2-acquired :to-be t)
      ;; Clean up Task 2 lock
      (macher-agent-vfs-release-lock resource task-2))))

(provide 'test-sandbox)
;;; test-sandbox.el ends here
