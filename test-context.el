;;; test-context.el --- Context lifecycle, session state, and buffer isolation -*- lexical-binding: t; -*-

;;; Commentary:
;; Specification 2: Context lifecycle, session state, and buffer isolation
;; CTX-01, CTX-02, CTX-03, CTX-04, CTX-05, CTX-06

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-test-helpers)

(describe "2. Context lifecycle, session state, and buffer isolation"

  (before-each
    (macher-agent-test-reset-env))

  ;; --------------------------------------------------------------------------
  ;; CTX-01
  ;; --------------------------------------------------------------------------
  (it "CTX-01: enables macher-agent-mode inside a Git repository directory and binds context"
    (let* ((repo-dir (macher-agent-test-create-git-repo))
           (buf (generate-new-buffer "*test-ctx-01*")))
      (unwind-protect
          (with-current-buffer buf
            (setq-local default-directory repo-dir)
            (macher-agent-mode 1)
            (expect macher-agent-mode :to-be t)
            (let ((ctx macher-agent--persistent-context))
              (expect (macher-agent-context-p ctx) :to-be t)
              (expect (macher-agent-context-owner-buffer ctx) :to-equal buf)
              (expect (macher-agent-context-parent-buffer ctx) :to-be nil)
              (expect (gethash repo-dir macher-agent-active-workspaces) :to-be ctx)))
        (when (buffer-live-p buf) (kill-buffer buf))
        (ignore-errors (delete-directory repo-dir t)))))

  ;; --------------------------------------------------------------------------
  ;; CTX-02
  ;; --------------------------------------------------------------------------
  (it "CTX-02: fails in uninitialised directory, then succeeds via macher-agent-force-enable"
    (let* ((non-repo-dir (make-temp-file "macher-non-repo-" t))
           (canonical (file-name-as-directory (file-truename non-repo-dir)))
           (buf (generate-new-buffer "*test-ctx-02*")))
      (unwind-protect
          (with-current-buffer buf
            (setq-local default-directory canonical)
            ;; Normal activation signals user-error and leaves hooks uninstalled
            (expect (macher-agent-mode 1) :to-throw 'user-error)
            (expect macher-agent-mode :to-be nil)
            (expect (memq #'macher-agent-sync-prompt-transformer
                          (bound-and-true-p gptel-prompt-transform-functions))
                    :to-be nil)
            ;; Force enable overrides check and initialises workspace context
            (macher-agent-force-enable)
            (expect macher-agent-mode :to-be t)
            (expect (macher-agent-context-p macher-agent--persistent-context) :to-be t)
            (expect (file-equal-p (macher-agent-context-project-root macher-agent--persistent-context)
                                  canonical)
                    :to-be t))
        (when (buffer-live-p buf) (kill-buffer buf))
        (ignore-errors (delete-directory non-repo-dir t)))))

  ;; --------------------------------------------------------------------------
  ;; CTX-03
  ;; --------------------------------------------------------------------------
  (it "CTX-03: spawns sub-agent with isolated session struct and lineage pointers"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
      (let* ((child-buf (macher-agent-add-subagent "*test-ctx-03-child*" nil parent-buf nil parent-ctx))
             (child-ctx (with-current-buffer child-buf macher-agent--persistent-context))
             (parent-session (macher-agent-context-session parent-ctx))
             (child-session (macher-agent-context-session child-ctx)))
        (expect (buffer-live-p child-buf) :to-be t)
        ;; Lineage verification
        (expect (macher-agent-context-owner-buffer child-ctx) :to-equal child-buf)
        (expect (macher-agent-context-parent-buffer child-ctx) :to-equal parent-buf)
        (expect (macher-agent-session-owner-buffer child-session) :to-equal child-buf)
        (expect (macher-agent-session-parent-buffer child-session) :to-equal parent-buf)
        ;; State isolation: mutating child session state does not affect parent session
        (setf (macher-agent-session-state child-session) :running)
        (setf (macher-agent-session-lifecycle child-session) :ephemeral)
        (expect (macher-agent-session-state parent-session) :not :to-equal :running)
        (expect (macher-agent-session-lifecycle parent-session) :to-equal :persistent))))

  ;; --------------------------------------------------------------------------
  ;; CTX-04
  ;; --------------------------------------------------------------------------
  (it "CTX-04: branches active chat buffer with deep clone of VFS and tool state"
    (macher-agent-test-with-git-workspace (root ctx parent-buf)
      (with-current-buffer parent-buf
        (text-mode)
        (gptel-mode 1)
        (insert "User turn 1: Please implement authentication.\n")
        (setq-local gptel-model 'gpt-4o)
        (setq-local macher-agent-presets '(developer-skill))
        ;; Stage VFS edits
        (macher-agent--update-context-file ctx "auth.el" "(defun auth () t)")
        ;; Branch chat
        (let* ((branch-buf (macher-agent-branch-chat "*test-ctx-04-branch*"))
               (branch-ctx (with-current-buffer branch-buf macher-agent--persistent-context)))
          (expect (buffer-live-p branch-buf) :to-be t)
          ;; Inherits conversation text
          (expect (with-current-buffer branch-buf (buffer-string))
                  :to-match "Please implement authentication")
          ;; Inherits settings
          (expect (buffer-local-value 'gptel-model branch-buf) :to-equal 'gpt-4o)
          (expect (buffer-local-value 'macher-agent-presets branch-buf) :to-equal '(developer-skill))
          ;; Holds independent deep clone of VFS
          (expect (macher-agent--read-context-file branch-ctx "auth.el")
                  :to-equal "(defun auth () t)")
          ;; Mutate branch VFS: must not affect parent VFS
          (macher-agent--update-context-file branch-ctx "auth.el" "(defun auth () :branch-mutated)")
          (expect (macher-agent--read-context-file branch-ctx "auth.el")
                  :to-equal "(defun auth () :branch-mutated)")
          (expect (macher-agent--read-context-file ctx "auth.el")
                  :to-equal "(defun auth () t)")))))

  ;; --------------------------------------------------------------------------
  ;; CTX-05
  ;; --------------------------------------------------------------------------
  (it "CTX-05: clears uncommitted VFS entries and dirty flags while preserving root and active FSM"
    (macher-agent-test-with-git-workspace (root ctx parent-buf)
      (with-current-buffer parent-buf
        ;; Stage modifications
        (macher-agent--update-context-file ctx "test.el" "(message \"modified\")")
        (macher-agent--update-context-file ctx "*virtual-buffer*" "virtual content")
        (expect (macher-agent--get-context-dirty-p ctx) :to-be t)
        (expect (length (macher-agent--get-context-contents ctx)) :to-equal 2)
        ;; Bind active FSM
        (let ((fsm (gptel-make-fsm :info (list :macher-agent-context ctx :buffer parent-buf))))
          (setq-local macher-agent--active-fsm fsm)
          ;; Invoke clear-context
          (macher-agent-clear-context)
          (let ((cleared-ctx macher-agent--persistent-context))
            (expect (macher-agent-context-p cleared-ctx) :to-be t)
            ;; Dirty flag and VFS entries cleared to disk baseline
            (expect (macher-agent--get-context-dirty-p cleared-ctx) :to-be nil)
            (expect (macher-agent--get-context-contents cleared-ctx) :to-be nil)
            ;; Project root preserved
            (expect (file-equal-p (macher-agent-context-project-root cleared-ctx) root) :to-be t)
            ;; Active FSM reference updated to cleared context
            (expect (plist-get (gptel-fsm-info macher-agent--active-fsm) :macher-agent-context)
                    :to-be cleared-ctx))))))

  ;; --------------------------------------------------------------------------
  ;; CTX-06
  ;; --------------------------------------------------------------------------
  (it "CTX-06: records standard gptel tool calls and PTC tool calls in chronological order in audit log"
    (macher-agent-test-with-git-workspace (root ctx parent-buf)
      (with-current-buffer parent-buf
        (let* ((fsm (gptel-make-fsm :info (list :macher-agent-context ctx :buffer parent-buf)))
               (macher-agent--active-fsm fsm)
               (tool-one (gptel-make-tool :name "standard_tool" :description "Tool 1" :category "test"))
               (macher-agent--active-ptc-primitives '(ptc_sandbox_tool)))
          ;; 1. Execute standard gptel tool call
          (macher-agent--log-gptel-pre-tool tool-one fsm :query "find bugs")
          ;; 2. Execute PTC sandbox tool call
          (macher-agent-sandbox-run '(ptc_sandbox_tool :action "compile") nil ctx parent-buf)
          ;; Inspect :audit-log slot on active context
          (let* ((plugins (macher-agent-context-plugins ctx))
                 (audit-log (reverse (plist-get plugins :audit-log))))
            (expect (length audit-log) :to-equal 2)
            ;; Chronological order
            (let ((entry1 (nth 0 audit-log))
                  (entry2 (nth 1 audit-log)))
              (expect (plist-get entry1 :tool) :to-equal "gptel-tool")
              (expect (plist-get entry1 :args) :to-equal "standard_tool")
              (expect (plist-get entry1 :target) :to-equal '(:query "find bugs"))
              (expect (stringp (plist-get entry1 :timestamp)) :to-be t)

              (expect (plist-get entry2 :tool) :to-equal "ptc")
              (expect (equal (format "%s" (plist-get entry2 :args)) "ptc_sandbox_tool") :to-be t)
              (expect (plist-get entry2 :target) :to-equal '(:action "compile"))
              (expect (stringp (plist-get entry2 :timestamp)) :to-be t))))))))

(provide 'test-context)
;;; test-context.el ends here
