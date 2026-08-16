;;; macher-agent-test-orchestration.el --- Orchestration and Session State Tests -*- lexical-binding: t; -*-

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

(describe "Orchestration and Session State"
  (macher-agent-test-setup-before-each)

  (it "bypasses UI when spawning background tasks via A2A dispatch"
      (let* ((buf (generate-new-buffer "subagent-bg-buf"))
             (payload (list (list :type 'SEND_MESSAGE
                                  :task-id "bg-task-1"
                                  :message "run"
                                  :metadata (list :buffer_name (buffer-name buf) :background t))))
             (callback-called nil)
             (ui-shown nil))
        (spy-on 'macher-agent-ui-show :and-call-fake (lambda (&rest _args) (setq ui-shown t)))
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda ()
                     (let ((cb (bound-and-true-p macher-agent--a2a-callback))
                           (task-id (bound-and-true-p macher-agent--current-task-id)))
                       (when cb
                         (funcall cb (list :status 'success :data "success-result" :task-id task-id)))))))
          (unwind-protect
              (progn
                (macher-agent-a2a-dispatch payload (lambda (_res) (setq callback-called t)))
                (expect callback-called :to-be t)
                (expect ui-shown :to-be nil))
            (kill-buffer buf)))))

  (it "rejects routing sub-agent payload back to originator buffer"
      (let* ((orig-buf (generate-new-buffer "orchestrator-self-buf"))
             (payload (list (list :type 'SEND_MESSAGE
                                  :task-id "self-task-1"
                                  :message "run"
                                  :metadata (list :buffer_name (buffer-name orig-buf))))))
        (unwind-protect
            (with-current-buffer orig-buf
              (let ((result nil))
                (macher-agent-a2a-dispatch payload (lambda (res) (setq result res)))
                (expect result :to-equal
                        (vector (list :status 'error
                                      :error (format "ERROR: Agent cannot route a sub-agent payload to its own buffer ('%s')." (buffer-name orig-buf))
                                      :buffer-name (buffer-name orig-buf)
                                      :task-id "self-task-1")))))
          (kill-buffer orig-buf))))

  (it "rejects routing sub-agent payload when originator in metadata matches buffer_name"
      (let* ((orig-buf (generate-new-buffer "orchestrator-explicit-buf"))
             (payload (list (list :type 'SEND_MESSAGE
                                  :task-id "self-task-2"
                                  :message "run"
                                  :metadata (list :buffer_name "orchestrator-explicit-buf"
                                                  :originator "orchestrator-explicit-buf")))))
        (unwind-protect
            (let ((result nil))
              (macher-agent-a2a-dispatch payload (lambda (res) (setq result res)))
              (expect result :to-equal
                      (vector (list :status 'error
                                    :error "ERROR: Agent cannot route a sub-agent payload to its own buffer ('orchestrator-explicit-buf')."
                                    :buffer-name "orchestrator-explicit-buf"
                                    :task-id "self-task-2"))))
          (kill-buffer orig-buf))))

  (it "rejects routing sub-agent payload when buffer object is passed as buffer_name"
      (let* ((orig-buf (generate-new-buffer "orchestrator-buf-obj-1"))
             (task-id "self-task-buf-obj-1")
             (payload (list (list :type 'SEND_MESSAGE
                                  :task-id task-id
                                  :message "run"
                                  :metadata (list :buffer_name orig-buf)))))
        (unwind-protect
            (with-current-buffer orig-buf
              (let ((result nil))
                (expect (gethash task-id macher-agent--task-registry) :to-be nil)
                (macher-agent-a2a-dispatch payload (lambda (res) (setq result res)))
                (expect result :to-equal
                        (vector (list :status 'error
                                      :error (format "ERROR: Agent cannot route a sub-agent payload to its own buffer ('%s')." (buffer-name orig-buf))
                                      :buffer-name (buffer-name orig-buf)
                                      :task-id task-id)))
                (expect (gethash task-id macher-agent--task-registry) :to-be nil)))
          (kill-buffer orig-buf))))

  (it "rejects routing sub-agent payload when buffer object is passed as originator"
      (let* ((orig-buf (generate-new-buffer "orchestrator-buf-obj-2"))
             (task-id "self-task-buf-obj-2")
             (payload (list (list :type 'SEND_MESSAGE
                                  :task-id task-id
                                  :message "run"
                                  :metadata (list :buffer_name (buffer-name orig-buf)
                                                  :originator orig-buf)))))
        (unwind-protect
            (let ((result nil))
              (expect (gethash task-id macher-agent--task-registry) :to-be nil)
              (macher-agent-a2a-dispatch payload (lambda (res) (setq result res)))
              (expect result :to-equal
                      (vector (list :status 'error
                                    :error (format "ERROR: Agent cannot route a sub-agent payload to its own buffer ('%s')." (buffer-name orig-buf))
                                    :buffer-name (buffer-name orig-buf)
                                    :task-id task-id)))
              (expect (gethash task-id macher-agent--task-registry) :to-be nil))
          (kill-buffer orig-buf))))

  (it "rejects routing sub-agent payload when both buffer_name and originator are buffer objects"
      (let* ((orig-buf (generate-new-buffer "orchestrator-buf-obj-3"))
             (task-id "self-task-buf-obj-3")
             (payload (list (list :type 'SEND_MESSAGE
                                  :task-id task-id
                                  :message "run"
                                  :metadata (list :buffer_name orig-buf
                                                  :originator orig-buf)))))
        (unwind-protect
            (let ((result nil))
              (expect (gethash task-id macher-agent--task-registry) :to-be nil)
              (macher-agent-a2a-dispatch payload (lambda (res) (setq result res)))
              (expect result :to-equal
                      (vector (list :status 'error
                                    :error (format "ERROR: Agent cannot route a sub-agent payload to its own buffer ('%s')." (buffer-name orig-buf))
                                    :buffer-name (buffer-name orig-buf)
                                    :task-id task-id)))
              (expect (gethash task-id macher-agent--task-registry) :to-be nil))
          (kill-buffer orig-buf))))

  (it "resolves target and updates task registry when valid buffer object is passed"
      (let* ((orig-buf (generate-new-buffer "orchestrator-parent-buf"))
             (target-buf (generate-new-buffer "subagent-target-buf"))
             (task-id "valid-task-obj-1")
             (payload (list (list :type 'SEND_MESSAGE
                                  :task-id task-id
                                  :message "run"
                                  :metadata (list :buffer_name target-buf
                                                  :originator orig-buf))))
             (callback-called nil))
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda ()
                     (let ((cb (bound-and-true-p macher-agent--a2a-callback))
                           (tid (bound-and-true-p macher-agent--current-task-id)))
                       (when cb
                         (funcall cb (list :status 'success :data "target-result" :task-id tid)))))))
          (unwind-protect
              (with-current-buffer orig-buf
                (macher-agent-a2a-dispatch payload (lambda (_res) (setq callback-called t)))
                (expect callback-called :to-be t)
                (expect (gethash task-id macher-agent--task-registry) :to-equal (buffer-name orig-buf)))
            (kill-buffer orig-buf)
            (kill-buffer target-buf)))))

  (it "does not falsely trigger self-referential error on nil buffer_name and nil originator"
      (let* ((task-id "nil-buf-task-1")
             (payload (list (list :type 'SEND_MESSAGE
                                  :task-id task-id
                                  :message "run"
                                  :metadata (list :buffer_name nil
                                                  :originator nil)))))
        (cl-letf (((symbol-function 'current-buffer) (lambda () nil)))
          (let ((result nil))
            (macher-agent-a2a-dispatch payload (lambda (res) (setq result res)))
            (expect result :to-equal
                    (vector (list :status 'error
                                  :error "ERROR: Sub-agent buffer 'nil' not found."
                                  :buffer-name nil
                                  :task-id task-id)))
            (expect (gethash task-id macher-agent--task-registry) :to-be nil)))))

  (it "handles dead buffer objects without triggering self-referential error or leaving orphan state"
      (let* ((dead-target (generate-new-buffer "dead-subagent-buf"))
             (dead-orig (generate-new-buffer "dead-orig-buf"))
             (task-id "dead-buf-task-1"))
        (kill-buffer dead-target)
        (kill-buffer dead-orig)
        (let* ((payload (list (list :type 'SEND_MESSAGE
                                    :task-id task-id
                                    :message "run"
                                    :metadata (list :buffer_name dead-target
                                                    :originator dead-orig))))
               (result nil))
          (macher-agent-a2a-dispatch payload (lambda (res) (setq result res)))
          (expect result :to-equal
                  (vector (list :status 'error
                                :error "ERROR: Sub-agent buffer 'nil' not found."
                                :buffer-name nil
                                :task-id task-id)))
          (expect (gethash task-id macher-agent--task-registry) :to-be nil))))

  (it "prevents hash-table collisions when multiple error payloads have nil task-ids"
      (let* ((payloads (list (list :type 'SEND_MESSAGE
                                   :message "run 1"
                                   :metadata (list :buffer_name "nonexistent-agent-1"))
                             (list :type 'SEND_MESSAGE
                                   :message "run 2"
                                   :metadata (list :buffer_name "nonexistent-agent-2"))
                             (list :type 'SEND_MESSAGE
                                   :message "run 3"
                                   :metadata (list :buffer_name "nonexistent-agent-3"))))
             (callback-result nil))
        (macher-agent-a2a-dispatch payloads (lambda (res) (setq callback-result res)))
        (expect (vectorp callback-result) :to-be t)
        (expect (length callback-result) :to-equal 3)
        (dotimes (i 3)
          (let ((res (aref callback-result i)))
            (expect (plist-get res :status) :to-equal 'error)
            (expect (plist-get res :task-id) :not :to-be nil)))
        (let ((tid0 (plist-get (aref callback-result 0) :task-id))
              (tid1 (plist-get (aref callback-result 1) :task-id))
              (tid2 (plist-get (aref callback-result 2) :task-id)))
          (expect (equal tid0 tid1) :to-be nil)
          (expect (equal tid1 tid2) :to-be nil)
          (expect (equal tid0 tid2) :to-be nil)
          (expect (gethash tid0 macher-agent--task-registry) :to-be nil)
          (expect (gethash tid1 macher-agent--task-registry) :to-be nil)
          (expect (gethash tid2 macher-agent--task-registry) :to-be nil))))

  (it "macher-agent-add-buffer-to-scope explicitly errors out if no existing session is found"
      (let ((buf (generate-new-buffer "lazy-target")))
        (let ((gptel--fsm-last nil)
              (macher-agent-active-workspaces (make-hash-table :test 'equal))
              (macher-agent--persistent-context nil))
          (cl-letf (((symbol-function 'buffer-list) (lambda () nil)))
            (expect (macher-agent-add-buffer-to-scope "lazy-target") :to-throw 'error)))
        (kill-buffer buf)))

  (it "macher-agent-add-subagent creates a buffer and tracks it globally"
      (let* ((mock-workspace (make-macher-agent-workspace :project-root "/tmp/"))
             (mock-context (macher-agent--make-vfs-context :workspace mock-workspace :contents nil)))
        (puthash (expand-file-name "/tmp/") mock-context macher-agent-active-workspaces)
        (spy-on 'macher-agent-resolve-context :and-return-value mock-context)
        (let ((buf (macher-agent-add-subagent "test-worker" "/tmp/" nil mock-context)))
          (expect (buffer-live-p buf) :to-be t)
          (expect (assoc "test-worker" (macher-agent-workspace-active-subagents (macher-agent--get-context-workspace (macher-agent-resolve-context)))) :to-be-truthy)
          (kill-buffer buf))))

  (it "triggers the UI safely on completion without modifying the FSM"
      (let* ((buf (generate-new-buffer "test-bridge"))
             (ctx (macher--make-context :dirty-p t))
             (file-path (expand-file-name "test.txt")))
        (push (macher-agent-vfs-make-entry file-path "old" "new") (macher-context-contents ctx))
        (with-current-buffer buf
          (setq-local macher-agent--is-workspace t)
          (setq-local macher-agent--persistent-context ctx)
          (setq-local gptel--fsm-last nil))
        (with-current-buffer buf
          (macher-agent-apply-virtual-buffers))
        (kill-buffer buf)))

  (it "macher-agent-apply-virtual-buffers applies pending context edits to live Emacs buffers"
      (let* ((buf (generate-new-buffer "live-target"))
             (ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry (buffer-name buf) "old" "new text")))))
        (with-current-buffer buf (insert "old"))
        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        (spy-on 'macher-agent--auto-sync-context)
        (macher-agent-apply-virtual-buffers)
        (with-current-buffer buf
          (expect (buffer-string) :to-equal "new text"))
        (kill-buffer buf)))

  (it "clears persistent context upon user request"
      (let* ((ws (make-macher-agent-workspace :project-root "/mock/proj/"))
             (ctx (macher--make-context :workspace ws :contents (list (macher-agent-vfs-make-entry "file.txt" "orig" "mod"))))
             (buf (generate-new-buffer "active-session")))
        (with-current-buffer buf
          (setq-local macher-agent--persistent-context ctx)
          (macher-agent-clear-context)
          (expect (macher-agent--get-context-contents macher-agent--persistent-context) :to-be nil))
        (kill-buffer buf)))

  (it "isolates subagent context from workspace and merges changes on completion"
      (let* ((temp-dir (file-name-as-directory (make-temp-file "isolated-proj-" t)))
             (doc-file (expand-file-name "doc.txt" temp-dir)))
        (unwind-protect
            (progn
              (with-temp-file doc-file (insert "v1"))
              (let* ((ws (make-macher-agent-workspace :project-root temp-dir))
                     (parent-ctx (macher-agent--make-vfs-context :workspace ws :contents (list (macher-agent-vfs-make-entry "doc.txt" "v1" "v1"))))
                     (parent-buf (generate-new-buffer "parent-agent")))
                (unwind-protect
                    (progn
                      (puthash (expand-file-name temp-dir) parent-ctx macher-agent-active-workspaces)
                      (with-current-buffer parent-buf
                        (setq-local default-directory temp-dir)
                        (setq-local macher-agent--persistent-context parent-ctx)
                        (let ((sub-buf (macher-agent-add-subagent "child-agent" temp-dir nil parent-ctx)))
                          (with-current-buffer sub-buf
                            (expect (eq macher-agent--persistent-context parent-ctx) :to-be nil)
                            (macher-agent--update-context-file macher-agent--persistent-context "doc.txt" "v2")
                            (expect (macher-agent-vfs-read (macher-agent-workspace-vfs-buffers (macher-agent--get-context-workspace parent-ctx)) (macher-context-contents parent-ctx) "doc.txt") :to-equal "v1")
                            (macher-agent-clear-context)
                            (expect (macher-agent-vfs-read (macher-agent-workspace-vfs-buffers (macher-agent--get-context-workspace macher-agent--persistent-context)) (macher-context-contents macher-agent--persistent-context) "doc.txt") :to-equal "v1")
                            (macher-agent--update-context-file macher-agent--persistent-context "doc.txt" "v3"))
                          (with-current-buffer parent-buf
                            (macher-agent-a2a-dispatch
                             (list (list :type 'SEND_MESSAGE
                                         :task-id "task-child"
                                         :message "run"
                                         :metadata (list :buffer_name "child-agent")))
                             (lambda (_res)
                               (expect (macher-agent-vfs-read (macher-agent-workspace-vfs-buffers (macher-agent--get-context-workspace parent-ctx)) (macher-context-contents parent-ctx) "doc.txt") :to-equal "v3"))))
                          (with-current-buffer sub-buf
                            (macher-agent-submit-task-result "Done"))
                          (kill-buffer sub-buf))))
                  (kill-buffer parent-buf))))
          (delete-directory temp-dir t))))

  (it "correctly initialises and binds the context during session restoration, preventing buffer-list fallback leakage"
      (let* ((other-buf (generate-new-buffer "other-chat-buffer"))
             (restore-buf (generate-new-buffer "restored-chat-buffer"))
             (other-dir "/mock/proj/other")
             (restore-dir "/mock/proj/restore")
             (macher-agent--allow-gptel-restore t)
             (orig-called nil)
             (orig-fun (lambda (&rest _args) (setq orig-called t))))
        (unwind-protect
            (progn
              (with-current-buffer other-buf
                (setq-local default-directory other-dir)
                (macher-agent--init-workspace-state other-dir))
              (with-current-buffer restore-buf
                (setq-local default-directory restore-dir)
                (expect (bound-and-true-p macher-agent--persistent-context) :to-be nil)
                (macher-agent--gptel-restore-advice orig-fun)
                (expect orig-called :to-be t)
                (let ((local-ctx (bound-and-true-p macher-agent--persistent-context)))
                  (expect local-ctx :not :to-be nil)
                  (let ((workspace (macher-agent--get-context-workspace local-ctx)))
                    (expect (macher-agent-workspace-project-root workspace) :to-equal (expand-file-name restore-dir))))))
          (when (buffer-live-p other-buf) (kill-buffer other-buf))
          (when (buffer-live-p restore-buf) (kill-buffer restore-buf)))))

  (it "synchronises deserialised persistent-context into active-workspaces after restore"
      (let* ((restore-buf (generate-new-buffer "restore-test-buf"))
             (restore-dir "/mock/proj/restored-ws/")
             (ws (make-macher-agent-workspace :project-root restore-dir))
             (deserialised-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
             (macher-agent--allow-gptel-restore t)
             (orig-fun (lambda (&rest _args)
                         (setq-local macher-agent--persistent-context deserialised-ctx))))
        (unwind-protect
            (with-current-buffer restore-buf
              (setq-local default-directory restore-dir)
              (macher-agent--gptel-restore-advice orig-fun)
              (let ((registered (gethash (expand-file-name restore-dir) macher-agent-active-workspaces)))
                (expect registered :to-equal deserialised-ctx))
              (let ((resolved-sub (macher-agent--resolve-context-from-ws (concat restore-dir "submodule-a/"))))
                (expect resolved-sub :to-equal deserialised-ctx)))
          (when (buffer-live-p restore-buf) (kill-buffer restore-buf)))))

  (it "clears active presets during setup if the restored session tag is present"
      (let ((buf (generate-new-buffer "restored-session-buf"))
            (ctx (macher--make-context :workspace (make-macher-agent-workspace :project-root "/tmp/") :contents nil)))
        (unwind-protect
            (with-current-buffer buf
              (setq-local default-directory "/tmp/")
              (setq-local macher-agent--is-workspace t)
              (setq-local macher-agent--persistent-context ctx)
              (setq-local macher-agent-presets '(some-preset))
              (setq-local macher-agent--is-restored-session t)
              (spy-on 'macher-agent-root :and-return-value "/tmp/")
              (macher-agent-setup-gptel-buffer)
              (expect macher-agent-presets :to-be nil)
              (expect macher-agent--is-restored-session :to-be nil))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (setq-local macher-agent-presets nil))
            (kill-buffer buf)))))

  (it "pushes and pops macher-agent--routing-stack with task-id, originator-name, and suppress-patch"
      (let ((buf (generate-new-buffer "test-routing-stack-buf")))
        (unwind-protect
            (with-current-buffer buf
              (expect (bound-and-true-p macher-agent--routing-stack) :to-be nil)
              (macher-agent--push-routing "task-100" "orchestrator-main" t)
              (expect (length macher-agent--routing-stack) :to-equal 1)
              (expect macher-agent--current-task-id :to-equal "task-100")
              (expect macher-agent--suppress-patch :to-be t)
              (let ((frame (car macher-agent--routing-stack)))
                (expect (plist-get frame :task-id) :to-equal "task-100")
                (expect (plist-get frame :originator-name) :to-equal "orchestrator-main")
                (expect (plist-get frame :suppress-patch) :to-be t))

              ;; Push a nested frame
              (macher-agent--push-routing "task-200" "peer-worker" nil)
              (expect (length macher-agent--routing-stack) :to-equal 2)
              (expect macher-agent--current-task-id :to-equal "task-200")
              (expect macher-agent--suppress-patch :to-be nil)

              ;; Pop the top frame
              (let ((popped (macher-agent--pop-routing)))
                (expect (plist-get popped :task-id) :to-equal "task-200")
                (expect (plist-get popped :originator-name) :to-equal "peer-worker")
                (expect (plist-get popped :suppress-patch) :to-be nil)
                (expect (length macher-agent--routing-stack) :to-equal 1)
                (expect macher-agent--current-task-id :to-equal "task-100")
                (expect macher-agent--suppress-patch :to-be t))

              ;; Pop remaining frame
              (let ((popped (macher-agent--pop-routing)))
                (expect (plist-get popped :task-id) :to-equal "task-100")
                (expect (length macher-agent--routing-stack) :to-equal 0)))
          (kill-buffer buf))))

  (it "macher-agent-submit-task-result transmits ARTIFACT_UPDATE payload via pending-callbacks registry"
      (let* ((buf (generate-new-buffer "submit-artifact-test-buf"))
             (task-id "task-artifact-001")
             (received-payload nil))
        (unwind-protect
            (progn
              (puthash task-id (lambda (payload) (setq received-payload payload)) macher-agent--pending-callbacks)
              (with-current-buffer buf
                (macher-agent--push-routing task-id "originator-agent" t)
                (macher-agent-submit-task-result "Completed artifact content.")
                (expect macher-agent--final-result :to-equal "Completed artifact content.")
                (expect macher-agent-task-finished :to-be t)
                (expect macher-agent--ready-to-reap :to-be t)
                (expect (gethash task-id macher-agent--pending-callbacks) :to-be nil))
              (expect (plist-get received-payload :type) :to-equal 'ARTIFACT_UPDATE)
              (expect (plist-get received-payload :task-id) :to-equal task-id)
              (let ((msg (plist-get received-payload :message)))
                (expect (plist-get msg :status) :to-equal 'success)
                (expect (plist-get msg :data) :to-equal "Completed artifact content.")
                (expect (plist-get msg :buffer-name) :to-equal (buffer-name buf))))
          (kill-buffer buf))))

  (it "normalises suppress-patch boundary to :suppress-patch exclusively"
      (let* ((orig-buf (generate-new-buffer "suppress-orig-buf"))
             (target-buf (generate-new-buffer "suppress-target-buf"))
             (task-id "task-suppress-norm"))
        (unwind-protect
            (with-current-buffer orig-buf
              (let ((payload (list (list :type 'SEND_MESSAGE
                                         :task-id task-id
                                         :message "test suppress"
                                         :metadata (list :buffer_name target-buf
                                                         :originator orig-buf
                                                         :suppress_patch t))))
                    (result nil))
                (cl-letf (((symbol-function 'gptel-send)
                           (lambda ()
                             (with-current-buffer target-buf
                               (expect (bound-and-true-p macher-agent--suppress-patch) :to-be t)
                               (macher-agent-submit-task-result "Done")))))
                  (macher-agent-a2a-dispatch payload (lambda (res) (setq result res)))
                  (expect (vectorp result) :to-be t))))
          (kill-buffer orig-buf)
          (kill-buffer target-buf))))

  (it "generates unique UUIDs when task-ids are missing in A2A dispatch"
      (let* ((orig-buf (generate-new-buffer "uuid-orig-buf"))
             (target-buf1 (generate-new-buffer "uuid-target-1"))
             (target-buf2 (generate-new-buffer "uuid-target-2"))
             (dispatched-task-ids nil))
        (unwind-protect
            (with-current-buffer orig-buf
              (cl-letf (((symbol-function 'gptel-send)
                         (lambda ()
                           (push (bound-and-true-p macher-agent--current-task-id) dispatched-task-ids)
                           (macher-agent-submit-task-result "ok"))))
                (macher-agent-a2a-dispatch
                 (list (list :type 'SEND_MESSAGE
                             :message "msg 1"
                             :metadata (list :buffer_name target-buf1))
                       (list :type 'SEND_MESSAGE
                             :message "msg 2"
                             :metadata (list :buffer_name target-buf2)))
                 (lambda (_res) nil))
                (expect (length dispatched-task-ids) :to-equal 2)
                (expect (car dispatched-task-ids) :not :to-be nil)
                (expect (cadr dispatched-task-ids) :not :to-be nil)
                (expect (equal (car dispatched-task-ids) (cadr dispatched-task-ids)) :to-be nil)))
          (kill-buffer orig-buf)
          (kill-buffer target-buf1)
          (kill-buffer target-buf2))))

  (it "maintains results tracking in bind-closure under mixed success and error conditions"
      (let* ((orig-buf (generate-new-buffer "mixed-orig-buf"))
             (valid-target (generate-new-buffer "mixed-valid-target"))
             (final-results nil))
        (unwind-protect
            (with-current-buffer orig-buf
              (cl-letf (((symbol-function 'gptel-send)
                         (lambda ()
                           (macher-agent-submit-task-result "Valid target result"))))
                (macher-agent-a2a-dispatch
                 (list (list :type 'SEND_MESSAGE
                             :task-id "task-valid"
                             :message "run valid"
                             :metadata (list :buffer_name valid-target))
                       (list :type 'SEND_MESSAGE
                             :task-id "task-invalid"
                             :message "run invalid"
                             :metadata (list :buffer_name "nonexistent-worker-buffer")))
                 (lambda (res) (setq final-results res))))
              (expect (vectorp final-results) :to-be t)
              (expect (length final-results) :to-equal 2)
              (expect (plist-get (aref final-results 0) :status) :to-equal 'success)
              (expect (plist-get (aref final-results 0) :data) :to-equal "Valid target result")
              (expect (plist-get (aref final-results 1) :status) :to-equal 'error)
              (expect (plist-get (aref final-results 1) :task-id) :to-equal "task-invalid"))
          (kill-buffer orig-buf)
          (kill-buffer valid-target))))

          (it "normalises empty string task-id to a newly generated UUID"
              (let* ((orig-buf (generate-new-buffer "empty-tid-orig-buf"))
                     (target-buf (generate-new-buffer "empty-tid-target-buf"))
                     (captured-task-id nil))
                (unwind-protect
                    (with-current-buffer orig-buf
                      (cl-letf (((symbol-function 'gptel-send)
                                 (lambda ()
                                   (setq captured-task-id (bound-and-true-p macher-agent--current-task-id))
                                   (macher-agent-submit-task-result "done"))))
                        (macher-agent-a2a-dispatch
                         (list (list :type 'SEND_MESSAGE
                                     :task-id ""
                                     :message "test empty tid"
                                     :metadata (list :buffer_name target-buf)))
                         (lambda (_res) nil))
                        (expect captured-task-id :not :to-be nil)
                        (expect (string-empty-p captured-task-id) :to-be nil)))
                  (kill-buffer orig-buf)
                  (kill-buffer target-buf))))

          (it "preserves explicitly provided non-empty task identifier during payload normalisation"
              (let* ((orig-buf (generate-new-buffer "explicit-tid-orig-buf"))
                     (target-buf (generate-new-buffer "explicit-tid-target-buf"))
                     (captured-task-id nil))
                (unwind-protect
                    (with-current-buffer orig-buf
                      (cl-letf (((symbol-function 'gptel-send)
                                 (lambda ()
                                   (setq captured-task-id (bound-and-true-p macher-agent--current-task-id))
                                   (macher-agent-submit-task-result "done"))))
                        (macher-agent-a2a-dispatch
                         (list (list :type 'SEND_MESSAGE
                                     :task-id "custom-task-id-12345"
                                     :message "test explicit tid"
                                     :metadata (list :buffer_name target-buf)))
                         (lambda (_res) nil))
                        (expect captured-task-id :to-equal "custom-task-id-12345")))
                  (kill-buffer orig-buf)
                  (kill-buffer target-buf))))

          (it "maps task-id to originator buffer name in task registry upon newly spawned subagent"
              (let* ((orig-buf (generate-new-buffer "spawn-orig-buf"))
                     (task-id "spawn-registry-task-1")
                     (target-name "spawn-new-worker-buf"))
                (unwind-protect
                    (with-current-buffer orig-buf
                      (cl-letf (((symbol-function 'macher-agent-add-subagent)
                                 (lambda (name presets parent dir ctx)
                                   (generate-new-buffer name)))
                                ((symbol-function 'gptel-send)
                                 (lambda ()
                                   (macher-agent-submit-task-result "spawned done"))))
                        (macher-agent-a2a-dispatch
                         (list (list :type 'SEND_MESSAGE
                                     :task-id task-id
                                     :message "spawn test"
                                     :metadata (list :buffer_name target-name
                                                     :presets 'worker)))
                         (lambda (_res) nil))
                        (expect (gethash task-id macher-agent--task-registry) :to-equal (buffer-name orig-buf))))
                  (kill-buffer orig-buf)
                  (when-let* ((buf (get-buffer target-name)))
                    (kill-buffer buf)))))

          (it "falls back to error payload task identifier in bind-closure when msg lacks task-id"
              (let* ((err-payload (list :status 'error
                                        :error "some error"
                                        :buffer-name "err-buf"
                                        :task-id "error-task-id-777"))
                     (initial-state (list :a2a-msg (list :type 'SEND_MESSAGE :message "hello")
                                          :error-payload err-payload
                                          :shared-state (list :results (make-hash-table :test 'equal)
                                                              :total 1
                                                              :final-callback nil
                                                              :parent-buf nil
                                                              :parent-fsm nil
                                                              :original-payloads (list (list :type 'SEND_MESSAGE :message "hello")))))
                     (res-state (macher-agent-a2a-pipe--bind-closure initial-state))
                     (results-tbl (plist-get (plist-get res-state :shared-state) :results)))
                (expect (gethash "error-task-id-777" results-tbl) :to-equal err-payload))))

(provide 'macher-agent-test-orchestration)
;;; macher-agent-test-orchestration.el ends here