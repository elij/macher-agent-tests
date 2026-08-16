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
                             (let* ((task-id (bound-and-true-p macher-agent--current-task-id))
                                    (cb (when task-id (gethash task-id macher-agent--pending-callbacks))))
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
                             (let* ((tid (bound-and-true-p macher-agent--current-task-id))
                                    (cb (when tid (gethash tid macher-agent--pending-callbacks))))
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
                        (expect macher-agent--ready-to-reap :to-be nil)
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
                (expect (gethash "error-task-id-777" results-tbl) :to-equal err-payload)))

          (it "dispatches submit_task_result tool via macher-agent-a2a-dispatch with ARTIFACT_UPDATE"
              (load (expand-file-name "skills/scripts/submit_task_result.el") nil t)
              (let* ((buf (generate-new-buffer "submit-tool-test-buf"))
                     (task-id "task-tool-artifact-999")
                     (received-payload nil)
                     (tool-cmd (or (get 'macher-agent-submit-task-result-tool 'command-fn)
                                   (get 'macher-agent-tool-submit-task-result 'command-fn))))
                (unwind-protect
                    (progn
                      (puthash task-id (lambda (payload) (setq received-payload payload)) macher-agent--pending-callbacks)
                      (spy-on 'macher-agent-a2a-dispatch :and-call-through)
                      (with-current-buffer buf
                        (macher-agent--push-routing task-id "originator-agent" t)
                        (let ((res (funcall tool-cmd '(:final_answer "Result from tool.") nil nil)))
                          (expect res :to-equal "Result from tool.")
                          (expect 'macher-agent-a2a-dispatch :to-have-been-called)
                          (expect macher-agent--final-result :to-equal "Result from tool.")
                          (expect (gethash task-id macher-agent--pending-callbacks) :to-be nil)))
                      (expect (plist-get received-payload :type) :to-equal 'ARTIFACT_UPDATE)
                      (expect (plist-get received-payload :task-id) :to-equal task-id)
                      (let ((msg (plist-get received-payload :message)))
                        (expect (plist-get msg :status) :to-equal 'success)
                        (expect (plist-get msg :data) :to-equal "Result from tool.")))
                  (kill-buffer buf))))

          (it "cleanly copies and does not mutate existing VFS entry in macher-agent-a2a-pipe--bind-closure"
              (let* ((parent-buf (generate-new-buffer "parent-diff-buf"))
                     (child-buf (generate-new-buffer "child-diff-buf"))
                     (orig-entry (cons "file1.txt" (cons "original text" "original text")))
                     (parent-ctx (macher--make-context :contents (list orig-entry)))
                     (task-id "task-clean-copy-123")
                     (results-tbl (make-hash-table :test 'equal))
                     (shared-state (list :results results-tbl
                                         :total 1
                                         :final-callback nil
                                         :parent-buf parent-buf
                                         :parent-fsm nil
                                         :original-payloads (list (list :type 'SEND_MESSAGE :task-id task-id))))
                     (initial-state (list :a2a-msg (list :type 'SEND_MESSAGE :task-id task-id)
                                          :child-buf child-buf
                                          :shared-state shared-state)))
                (unwind-protect
                    (progn
                      (with-current-buffer parent-buf
                        (setq-local macher-agent--persistent-context parent-ctx))
                      ;; Temporarily undefine macher-agent--update-context-file to exercise fallback branch
                      (cl-letf (((symbol-function 'macher-agent--update-context-file) nil))
                        (fmakunbound 'macher-agent--update-context-file)
                        (macher-agent-a2a-pipe--bind-closure initial-state)
                        (let ((cb (gethash task-id macher-agent--pending-callbacks))
                              (diff-entry (macher-agent-vfs-make-entry "file1.txt" "original text" "new modified text")))
                          (expect cb :not :to-be nil)
                          (funcall cb (list :type 'ARTIFACT_UPDATE
                                            :task-id task-id
                                            :message (list :status 'success
                                                           :data "all done"
                                                           :diff (list diff-entry))))
                          ;; Check that orig-entry was NOT mutated destructively
                          (expect (cdr (cdr orig-entry)) :to-equal "original text")
                          ;; Check that parent-ctx contents has the updated entry
                          (let ((updated (cl-find "file1.txt" (macher-agent--get-context-contents parent-ctx) :key #'car :test #'equal)))
                            (expect (macher-agent-vfs-entry-curr updated) :to-equal "new modified text")))))
                  (kill-buffer parent-buf)
                  (kill-buffer child-buf))))

          (it "confirms macher-agent--a2a-callback is undefined and unreferenced"
              (expect (boundp 'macher-agent--a2a-callback) :to-be nil)
              (expect (fboundp 'macher-agent--a2a-callback) :to-be nil))))

(describe "Generational Reaping and Ownership Registry"
          (macher-agent-test-setup-before-each)

          (before-each
           (clrhash macher-agent--a2a-ownership)
           (clrhash macher-agent--task-registry)
           (clrhash macher-agent--pending-callbacks))

          (it "verifies global ownership registry structure"
              (expect (hash-table-p macher-agent--a2a-ownership) :to-be t)
              (expect (hash-table-test macher-agent--a2a-ownership) :to-equal 'equal))

          (it "registers child buffers in pipeline resolve-target under originator-name"
              (let* ((orig-buf (generate-new-buffer "orch-orig-buf"))
                     (child-buf1 (generate-new-buffer "worker-buf-1"))
                     (child-buf2 (generate-new-buffer "worker-buf-2"))
                     (orig-name (buffer-name orig-buf))
                     (child-name1 (buffer-name child-buf1))
                     (child-name2 (buffer-name child-buf2)))
                (unwind-protect
                    (progn
                      ;; Resolve first child buffer
                      (let* ((state1 (list :a2a-msg (list :type 'SEND_MESSAGE
                                                          :metadata (list :buffer_name child-name1
                                                                          :originator orig-name))))
                             (res1 (macher-agent-a2a-pipe--resolve-target state1)))
                        (expect (plist-get res1 :child-buf) :to-equal child-buf1)
                        (expect (gethash orig-name macher-agent--a2a-ownership) :to-equal (list child-name1)))

                      ;; Resolve second child buffer under same originator
                      (let* ((state2 (list :a2a-msg (list :type 'SEND_MESSAGE
                                                          :metadata (list :buffer_name child-name2
                                                                          :originator orig-name))))
                             (res2 (macher-agent-a2a-pipe--resolve-target state2)))
                        (expect (plist-get res2 :child-buf) :to-equal child-buf2)
                        (expect (member child-name1 (gethash orig-name macher-agent--a2a-ownership)) :to-be-truthy)
                        (expect (member child-name2 (gethash orig-name macher-agent--a2a-ownership)) :to-be-truthy))

                      ;; Re-resolve first child buffer: should avoid duplicate in list
                      (let* ((state3 (list :a2a-msg (list :type 'SEND_MESSAGE
                                                          :metadata (list :buffer_name child-name1
                                                                          :originator orig-name))))
                             (res3 (macher-agent-a2a-pipe--resolve-target state3)))
                        (expect (plist-get res3 :child-buf) :to-equal child-buf1)
                        (let ((list-after (gethash orig-name macher-agent--a2a-ownership)))
                          (expect (length list-after) :to-equal 2))))
                  (kill-buffer orig-buf)
                  (kill-buffer child-buf1)
                  (kill-buffer child-buf2))))

          (it "does not register child buffer on self-route error in resolve-target"
              (let* ((orig-buf (generate-new-buffer "orch-self-route-buf"))
                     (orig-name (buffer-name orig-buf)))
                (unwind-protect
                    (let* ((state (list :a2a-msg (list :type 'SEND_MESSAGE
                                                       :metadata (list :buffer_name orig-name
                                                                       :originator orig-name))))
                           (res (macher-agent-a2a-pipe--resolve-target state)))
                      (expect (plist-get res :error-payload) :not :to-be nil)
                      (expect (gethash orig-name macher-agent--a2a-ownership) :to-be nil))
                  (kill-buffer orig-buf))))

          (it "submit_task_result tool preserves sub-agent state without marking ready-to-reap"
              (load (expand-file-name "skills/scripts/submit_task_result.el") nil t)
              (let* ((child-buf (generate-new-buffer "preserved-subagent-buf"))
                     (task-id "task-preserve-001")
                     (tool-cmd (or (get 'macher-agent-submit-task-result-tool 'command-fn)
                                   (get 'macher-agent-tool-submit-task-result 'command-fn))))
                (unwind-protect
                    (with-current-buffer child-buf
                      (setq-local macher-agent--is-subagent t)
                      (setq-local macher-agent--ready-to-reap nil)
                      (macher-agent--push-routing task-id "orch-parent" t)
                      (let ((res (funcall tool-cmd '(:final_answer "Task finished cleanly.") nil nil)))
                        (expect res :to-equal "Task finished cleanly.")
                        (expect macher-agent-task-finished :to-be t)
                        ;; Sub-agent state must be preserved: ready-to-reap remains nil
                        (expect macher-agent--ready-to-reap :to-be nil)))
                  (kill-buffer child-buf))))

          (it "macher-agent-sweep-subagents forces ready-to-reap, schedules reap, and deletes registry key"
              (let* ((orig-buf (generate-new-buffer "orch-parent-buf"))
                     (child1 (generate-new-buffer "worker-sweep-1"))
                     (child2 (generate-new-buffer "worker-sweep-2"))
                     (orig-name (buffer-name orig-buf))
                     (child-name1 (buffer-name child1))
                     (child-name2 (buffer-name child2))
                     (scheduled nil))
                (unwind-protect
                    (progn
                      (with-current-buffer child1
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent-task-finished t)
                        (setq-local macher-agent--ready-to-reap nil))
                      (with-current-buffer child2
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent-task-finished t)
                        (setq-local macher-agent--ready-to-reap nil))
                      (puthash orig-name (list child-name1 child-name2) macher-agent--a2a-ownership)
                      (spy-on 'macher-agent--schedule-buffer-reap :and-call-fake
                              (lambda (buf) (push buf scheduled)))
                      ;; Execute generational collector
                      (macher-agent-sweep-subagents orig-name)
                      ;; Both subagent buffers should now have ready-to-reap = t
                      (expect (buffer-local-value 'macher-agent--ready-to-reap child1) :to-be t)
                      (expect (buffer-local-value 'macher-agent--ready-to-reap child2) :to-be t)
                      ;; Both subagent buffers should have been scheduled for reap
                      (expect (member child1 scheduled) :to-be-truthy)
                      (expect (member child2 scheduled) :to-be-truthy)
                      ;; Ownership key must be removed
                      (expect (gethash orig-name macher-agent--a2a-ownership) :to-be nil))
                  (kill-buffer orig-buf)
                  (kill-buffer child1)
                  (kill-buffer child2))))

          (it "macher-agent-sweep-subagents recursively sweeps grandchildren subagents"
              (let* ((orig-buf (generate-new-buffer "orch-top-parent"))
                     (child-buf (generate-new-buffer "worker-child"))
                     (grandchild-buf (generate-new-buffer "worker-grandchild"))
                     (orig-name (buffer-name orig-buf))
                     (child-name (buffer-name child-buf))
                     (grandchild-name (buffer-name grandchild-buf))
                     (scheduled nil))
                (unwind-protect
                    (progn
                      (with-current-buffer child-buf
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent-task-finished t)
                        (setq-local macher-agent--ready-to-reap nil))
                      (with-current-buffer grandchild-buf
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent-task-finished t)
                        (setq-local macher-agent--ready-to-reap nil))
                      (puthash orig-name (list child-name) macher-agent--a2a-ownership)
                      (puthash child-name (list grandchild-name) macher-agent--a2a-ownership)
                      (spy-on 'macher-agent--schedule-buffer-reap :and-call-fake
                              (lambda (buf) (push buf scheduled)))
                      ;; Sweep top parent
                      (macher-agent-sweep-subagents orig-name)
                      ;; Both child and grandchild should be marked ready-to-reap and scheduled
                      (expect (buffer-local-value 'macher-agent--ready-to-reap child-buf) :to-be t)
                      (expect (buffer-local-value 'macher-agent--ready-to-reap grandchild-buf) :to-be t)
                      (expect (member child-buf scheduled) :to-be-truthy)
                      (expect (member grandchild-buf scheduled) :to-be-truthy)
                      ;; All ownership keys removed
                      (expect (gethash orig-name macher-agent--a2a-ownership) :to-be nil)
                      (expect (gethash child-name macher-agent--a2a-ownership) :to-be nil))
                  (kill-buffer orig-buf)
                  (kill-buffer child-buf)
                  (kill-buffer grandchild-buf))))

          (it "macher-agent-sweep-subagents maintains registry consistency by removing from active tracking lists"
              (let* ((orig-buf (generate-new-buffer "orch-reg-consistency"))
                     (child-buf (generate-new-buffer "child-reg-clean"))
                     (orig-name (buffer-name orig-buf))
                     (child-name (buffer-name child-buf))
                     (mock-ws (make-macher-agent-workspace :project-root "/tmp/test-reg/"))
                     (mock-ctx (macher--make-context :workspace mock-ws)))
                (unwind-protect
                    (progn
                      (with-current-buffer child-buf
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent-task-finished t)
                        (setq-local macher-agent--persistent-context mock-ctx))
                      (setq macher-agent-active-subagents (list (cons child-name "/tmp/test-reg/")))
                      (macher-agent--set-workspace-active-subagents mock-ws (list (cons child-name "/tmp/test-reg/")))
                      (puthash orig-name (list child-name) macher-agent--a2a-ownership)
                      (macher-agent-sweep-subagents orig-name)
                      ;; Verified removed from global and workspace registries
                      (expect (assoc child-name macher-agent-active-subagents) :to-be nil)
                      (expect (assoc child-name (macher-agent-workspace-active-subagents mock-ws)) :to-be nil))
                  (kill-buffer orig-buf)
                  (kill-buffer child-buf))))

          (it "macher-agent-sweep-subagents does not kill subagents still busy executing or with task-finished nil"
              (let* ((orig-buf (generate-new-buffer "orch-busy-parent"))
                     (busy-child (generate-new-buffer "busy-worker"))
                     (done-child (generate-new-buffer "done-worker"))
                     (orig-name (buffer-name orig-buf))
                     (busy-name (buffer-name busy-child))
                     (done-name (buffer-name done-child))
                     (scheduled nil))
                (unwind-protect
                    (progn
                      (with-current-buffer busy-child
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent-task-finished nil)
                        (setq-local macher-agent--ready-to-reap nil))
                      (with-current-buffer done-child
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent-task-finished t)
                        (setq-local macher-agent--ready-to-reap nil))
                      (puthash orig-name (list busy-name done-name) macher-agent--a2a-ownership)
                      (spy-on 'macher-agent--schedule-buffer-reap :and-call-fake
                              (lambda (buf) (push buf scheduled)))
                      (macher-agent-sweep-subagents orig-name)
                      ;; Done child is reaped
                      (expect (buffer-local-value 'macher-agent--ready-to-reap done-child) :to-be t)
                      (expect (member done-child scheduled) :to-be-truthy)
                      ;; Busy child is NOT reaped
                      (expect (buffer-local-value 'macher-agent--ready-to-reap busy-child) :to-be nil)
                      (expect (member busy-child scheduled) :to-be nil)
                      ;; Parent key is retained with remaining busy child
                      (expect (gethash orig-name macher-agent--a2a-ownership) :to-equal (list busy-name))
                      ;; Now mark busy child as finished and sweep again
                      (with-current-buffer busy-child
                        (setq-local macher-agent-task-finished t))
                      (macher-agent-sweep-subagents orig-name)
                      (expect (member busy-child scheduled) :to-be-truthy)
                      (expect (gethash orig-name macher-agent--a2a-ownership) :to-be nil))
                  (kill-buffer orig-buf)
                  (kill-buffer busy-child)
                  (kill-buffer done-child))))

          (it "macher-agent-post-response-reaper strictly resolves FSM belonging to current buffer"
              (let* ((buf1 (generate-new-buffer "buf-one"))
                     (buf2 (generate-new-buffer "buf-two"))
                     (swept-originator nil))
                (unwind-protect
                    (progn
                      (spy-on 'macher-agent-sweep-subagents :and-call-fake
                              (lambda (name) (setq swept-originator name)))
                      ;; Set up gptel--fsm belonging to buf2 with active tool calls
                      (let* ((mock-info-buf2 (list :tool-use (list '(:name "delegate_tasks")) :buffer buf2))
                             (mock-fsm-buf2 (list :info mock-info-buf2))
                             (gptel--fsm mock-fsm-buf2))
                        (spy-on 'gptel-fsm-info :and-call-fake
                                (lambda (f) (if (eq f mock-fsm-buf2) mock-info-buf2 nil)))
                        ;; When running post-response-reaper in buf1, it should NOT match buf2's FSM
                        (with-current-buffer buf1
                          (setq-local macher-agent--is-subagent nil)
                          (macher-agent-post-response-reaper (point-min) (point-max))
                          ;; buf1 has no tool calls and concludes workflow
                          (expect swept-originator :to-equal (buffer-name buf1)))))
                  (kill-buffer buf1)
                  (kill-buffer buf2))))

          (it "verifies macher-agent-task-finished and macher-agent--task-result are buffer-local in core"
              (let ((buf1 (generate-new-buffer "core-decl-buf1"))
                    (buf2 (generate-new-buffer "core-decl-buf2")))
                (unwind-protect
                    (progn
                      (with-current-buffer buf1
                        (setq-local macher-agent-task-finished t)
                        (setq-local macher-agent--task-result "buf1 result"))
                      (with-current-buffer buf2
                        (expect macher-agent-task-finished :to-be nil)
                        (expect macher-agent--task-result :to-be nil)))
                  (kill-buffer buf1)
                  (kill-buffer buf2))))

          (it "bind-closure applies incoming context diff to parent context"
              (let* ((parent-buf (generate-new-buffer "parent-diff-test"))
                     (child-buf (generate-new-buffer "child-diff-test"))
                     (orig-entry (cons "file1.txt" (cons "original text" "original text")))
                     (parent-ctx (macher--make-context :contents (list orig-entry)))
                     (task-id "task-diff-apply-999")
                     (results-tbl (make-hash-table :test 'equal))
                     (shared-state (list :results results-tbl
                                         :total 1
                                         :final-callback nil
                                         :parent-buf parent-buf
                                         :parent-fsm nil
                                         :original-payloads (list (list :type 'SEND_MESSAGE :task-id task-id))))
                     (initial-state (list :a2a-msg (list :type 'SEND_MESSAGE
                                                         :task-id task-id)
                                          :child-buf child-buf
                                          :shared-state shared-state)))
                (unwind-protect
                    (progn
                      (with-current-buffer parent-buf
                        (setq-local macher-agent--persistent-context parent-ctx))
                      (macher-agent-a2a-pipe--bind-closure initial-state)
                      (let ((cb (gethash task-id macher-agent--pending-callbacks))
                            (diff-entry (macher-agent-vfs-make-entry "file1.txt" "original text" "modified text")))
                        (expect cb :not :to-be nil)
                        (funcall cb (list :type 'ARTIFACT_UPDATE
                                          :task-id task-id
                                          :message (list :status 'success
                                                         :data "done"
                                                         :diff (list diff-entry))))
                        ;; Parent context should be updated with the incoming diff
                        (let ((entry (cl-find "file1.txt" (macher-agent--get-context-contents parent-ctx) :key #'car :test #'equal)))
                          (expect (macher-agent-vfs-entry-curr entry) :to-equal "modified text"))))
                  (kill-buffer parent-buf)
                  (kill-buffer child-buf))))

          (it "macher-agent--reap-buffer executes kill-buffer without suppressing kill-buffer-hook"
              (let* ((reap-buf (generate-new-buffer "reap-hook-test-buf"))
                     (hook-executed nil))
                (with-current-buffer reap-buf
                  (setq-local macher-agent--is-subagent t)
                  (setq-local macher-agent--ready-to-reap t)
                  (setq-local macher-agent-task-finished t)
                  (add-hook 'kill-buffer-hook (lambda () (setq hook-executed t)) nil t))
                (macher-agent--reap-buffer reap-buf)
                (expect hook-executed :to-be t)
                (expect (buffer-live-p reap-buf) :to-be nil)))

          (it "macher-agent-sweep-subagents handles dead buffer references gracefully"
              (let* ((orig-name "orch-dead-refs")
                     (scheduled nil))
                (puthash orig-name (list "non-existent-buffer-1" "non-existent-buffer-2") macher-agent--a2a-ownership)
                (spy-on 'macher-agent--schedule-buffer-reap :and-call-fake
                        (lambda (buf) (push buf scheduled)))
                (macher-agent-sweep-subagents orig-name)
                (expect scheduled :to-equal nil)
                (expect (gethash orig-name macher-agent--a2a-ownership) :to-be nil)))

          (it "macher-agent-post-response-reaper sweeps subagents when top-level orchestrator concludes without tool calls"
              (let* ((orch-buf (generate-new-buffer "orch-top-level-buf"))
                     (child-buf (generate-new-buffer "child-sweep-tgt"))
                     (orch-name (buffer-name orch-buf))
                     (child-name (buffer-name child-buf))
                     (swept-originator nil))
                (unwind-protect
                    (progn
                      (puthash orch-name (list child-name) macher-agent--a2a-ownership)
                      (spy-on 'macher-agent-sweep-subagents :and-call-fake
                              (lambda (name) (setq swept-originator name)))
                      (with-current-buffer orch-buf
                        (setq-local macher-agent--is-subagent nil)
                        ;; Mock FSM info with NO tool calls
                        (let* ((mock-info (list :tool-use nil :buffer orch-buf))
                               (gptel--fsm-last (list :info mock-info)))
                          (spy-on 'gptel-fsm-info :and-return-value mock-info)
                          (macher-agent-post-response-reaper (point-min) (point-max))
                          (expect swept-originator :to-equal orch-name))))
                  (kill-buffer orch-buf)
                  (kill-buffer child-buf))))

          (it "macher-agent-post-response-reaper does not sweep subagents when orchestrator generates tool calls"
              (let* ((orch-buf (generate-new-buffer "orch-active-tools-buf"))
                     (orch-name (buffer-name orch-buf))
                     (sweep-called nil))
                (unwind-protect
                    (progn
                      (spy-on 'macher-agent-sweep-subagents :and-call-fake
                              (lambda (_name) (setq sweep-called t)))
                      (with-current-buffer orch-buf
                        (setq-local macher-agent--is-subagent nil)
                        ;; Mock FSM info WITH tool calls
                        (let* ((mock-info (list :tool-use (list '(:name "delegate_tasks_to_subagents"))
                                                :buffer orch-buf))
                               (gptel--fsm-last (list :info mock-info)))
                          (spy-on 'gptel-fsm-info :and-return-value mock-info)
                          (macher-agent-post-response-reaper (point-min) (point-max))
                          (expect sweep-called :to-be nil))))
                  (kill-buffer orch-buf))))

          (it "subagent execution lifecycle flags ready-to-reap for background and ephemeral tasks on submission"
              (load (expand-file-name "skills/scripts/submit_task_result.el") nil t)
              (let* ((bg-subagent (generate-new-buffer "bg-subagent-worker"))
                     (eph-subagent (generate-new-buffer "eph-subagent-worker"))
                     (tool-cmd (or (get 'macher-agent-submit-task-result-tool 'command-fn)
                                   (get 'macher-agent-tool-submit-task-result 'command-fn)))
                     (scheduled nil))
                (unwind-protect
                    (progn
                      (spy-on 'macher-agent--schedule-buffer-reap :and-call-fake
                              (lambda (buf) (push buf scheduled)))
                      ;; 1. Background subagent submits task
                      (with-current-buffer bg-subagent
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent--is-background t)
                        (setq-local macher-agent--ready-to-reap nil)
                        (macher-agent--push-routing "task-bg-1" "parent-orch")
                        (funcall tool-cmd '(:final_answer "Background task done") nil nil)
                        (expect macher-agent--ready-to-reap :to-be t)
                        ;; Post-response hook fires and schedules reap
                        (macher-agent-post-response-reaper (point-min) (point-max))
                        (expect (member bg-subagent scheduled) :to-be-truthy))

                      ;; 2. Ephemeral subagent submits task
                      (with-current-buffer eph-subagent
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent--is-ephemeral t)
                        (setq-local macher-agent--ready-to-reap nil)
                        (macher-agent--push-routing "task-eph-1" "parent-orch")
                        (funcall tool-cmd '(:final_answer "Ephemeral task done") nil nil)
                        (expect macher-agent--ready-to-reap :to-be t)
                        ;; Post-response hook fires and schedules reap
                        (macher-agent-post-response-reaper (point-min) (point-max))
                        (expect (member eph-subagent scheduled) :to-be-truthy)))
                  (kill-buffer bg-subagent)
                  (kill-buffer eph-subagent))))

          (it "submit_task_result computes context differences conditionally based on suppress-patch"
              (load (expand-file-name "skills/scripts/submit_task_result.el") nil t)
              (let* ((child-buf (generate-new-buffer "diff-cond-subagent"))
                     (mock-entry (macher-agent-vfs-make-entry "file1.txt" "orig" "modified"))
                     (mock-ctx (macher--make-context :contents (list mock-entry)))
                     (tool-cmd (or (get 'macher-agent-submit-task-result-tool 'command-fn)
                                   (get 'macher-agent-tool-submit-task-result 'command-fn)))
                     (dispatched-payloads nil))
                (unwind-protect
                    (progn
                      (spy-on 'macher-agent-resolve-context :and-return-value mock-ctx)
                      (spy-on 'macher-agent-a2a-dispatch :and-call-fake
                              (lambda (payloads _cb) (setq dispatched-payloads payloads)))
                      ;; Test with suppress-patch = t: diff should be nil
                      (with-current-buffer child-buf
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent--suppress-patch t)
                        (setq-local macher-agent-task-finished nil)
                        (macher-agent--push-routing "task-suppress-t" "parent" t)
                        (funcall tool-cmd '(:final_answer "Suppressed diff answer") nil nil)
                        (let* ((msg (plist-get (car dispatched-payloads) :message))
                               (diff (plist-get msg :diff)))
                          (expect diff :to-be nil)))

                      ;; Test with suppress-patch = nil: diff should be included
                      (with-current-buffer child-buf
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent--suppress-patch nil)
                        (setq-local macher-agent-task-finished nil)
                        (macher-agent--push-routing "task-suppress-nil" "parent" nil)
                        (funcall tool-cmd '(:final_answer "Non-suppressed diff answer") nil nil)
                        (let* ((msg (plist-get (car dispatched-payloads) :message))
                               (diff (plist-get msg :diff)))
                          (expect (length diff) :to-equal 1)
                          (expect (macher-agent-vfs-entry-path (car diff)) :to-equal "file1.txt"))))
                  (kill-buffer child-buf))))

          (it "submit_task_result aborts on double submissions for ephemeral tasks"
              (load (expand-file-name "skills/scripts/submit_task_result.el") nil t)
              (let* ((child-buf (generate-new-buffer "double-sub-subagent"))
                     (tool-cmd (or (get 'macher-agent-submit-task-result-tool 'command-fn)
                                   (get 'macher-agent-tool-submit-task-result 'command-fn)))
                     (abort-called nil))
                (unwind-protect
                    (with-current-buffer child-buf
                      (setq-local macher-agent--is-subagent t)
                      (setq-local macher-agent--is-ephemeral t)
                      (setq-local macher-agent--ready-to-reap nil)
                      (macher-agent--push-routing "task-double-1" "parent")
                      (spy-on 'gptel-abort :and-call-fake (lambda (&rest _) (setq abort-called t)))
                      ;; First submission succeeds
                      (let ((res1 (funcall tool-cmd '(:final_answer "First submission") nil nil)))
                        (expect res1 :to-equal "First submission")
                        (expect macher-agent-task-finished :to-be t)
                        (expect macher-agent--ready-to-reap :to-be t))
                      ;; Second submission aborts
                      (let ((res2 (funcall tool-cmd '(:final_answer "Second submission") nil nil)))
                        (expect res2 :to-equal "ERROR: Task has already been submitted.")
                        (expect abort-called :to-be t)
                        (expect macher-agent--ready-to-reap :to-be t)))
                  (kill-buffer child-buf))))

          (it "resolve-target sets is-background and is-ephemeral on existing and child buffers"
              (let* ((existing-buf (generate-new-buffer "resolve-tgt-existing"))
                     (state-existing (list :a2a-msg (list :type 'SEND_MESSAGE
                                                          :task-id "task-meta-1"
                                                          :metadata (list :buffer_name (buffer-name existing-buf)
                                                                          :background t
                                                                          :ephemeral t))
                                           :shared-state (list :parent-buf (current-buffer))))
                     (state-child (list :a2a-msg (list :type 'SEND_MESSAGE
                                                       :task-id "task-meta-2"
                                                       :metadata (list :buffer_name "resolve-tgt-child-new"
                                                                       :presets '("elisp-coder")
                                                                       :background t
                                                                       :ephemeral t))
                                        :shared-state (list :parent-buf (current-buffer)))))
                (unwind-protect
                    (progn
                      ;; Existing buffer
                      (let ((res-ext (macher-agent-a2a-pipe--resolve-target state-existing)))
                        (with-current-buffer existing-buf
                          (expect macher-agent--is-background :to-be t)
                          (expect macher-agent--is-ephemeral :to-be t)))
                      ;; Child spawned buffer
                      (spy-on 'macher-agent-add-subagent :and-call-fake
                              (lambda (name _p _parent _d _ctx) (generate-new-buffer name)))
                      (let* ((res-ch (macher-agent-a2a-pipe--resolve-target state-child))
                             (ch-buf (plist-get res-ch :child-buf)))
                        (when ch-buf
                          (with-current-buffer ch-buf
                            (expect macher-agent--is-background :to-be t)
                            (expect macher-agent--is-ephemeral :to-be t)
                            (kill-buffer ch-buf)))))
                  (kill-buffer existing-buf))))

          (it "delegate_tasks_to_subagents tool includes ephemeral in schema and sends ephemeral metadata"
              (load (expand-file-name "skills/scripts/delegate_tasks_to_subagents.el") nil t)
              (let* ((tool macher-agent-delegate-tasks-to-subagents-tool)
                     (args (gptel-tool-args tool))
                     (tasks-arg (car args))
                     (props (plist-get (plist-get tasks-arg :items) :properties))
                     (dispatched nil))
                ;; Schema verification
                (expect (plist-member props :ephemeral) :not :to-be nil)
                (expect (plist-get (plist-get props :ephemeral) :type) :to-equal 'boolean)
                ;; Command function execution verification
                (spy-on 'macher-agent-a2a-dispatch :and-call-fake
                        (lambda (payloads _cb) (setq dispatched payloads)))
                (let ((tool-fn (gptel-tool-function tool)))
                  (funcall tool-fn (lambda (_res) nil)
                           (list (list :buffer_name "sub1" :instructions "inst1" :ephemeral t)
                                 (list :buffer_name "sub2" :instructions "inst2" :ephemeral nil)))
                  (expect (length dispatched) :to-equal 2)
                  (let ((meta1 (plist-get (nth 0 dispatched) :metadata))
                        (meta2 (plist-get (nth 1 dispatched) :metadata)))
                    (expect (plist-get meta1 :ephemeral) :to-be t)
                    (expect (plist-get meta2 :ephemeral) :to-be nil)))))

          (it "loads macher-agent-orchestration without syntax errors"
              (expect (featurep 'macher-agent-orchestration) :to-be t)
              (expect (fboundp 'macher-agent-a2a-pipe--bind-closure) :to-be t))

          (it "macher-agent-a2a-pipe--bind-closure binds callback and updates state"
              (let* ((child-buf (generate-new-buffer "bind-closure-test-child"))
                     (task-id "task-bind-closure-001")
                     (results-tbl (make-hash-table :test 'equal))
                     (shared-state (list :results results-tbl
                                         :total 1
                                         :final-callback nil
                                         :parent-buf nil
                                         :parent-fsm nil
                                         :original-payloads (list (list :type 'SEND_MESSAGE :task-id task-id))))
                     (initial-state (list :a2a-msg (list :type 'SEND_MESSAGE :task-id task-id)
                                          :child-buf child-buf
                                          :shared-state shared-state)))
                (unwind-protect
                    (let ((res-state (macher-agent-a2a-pipe--bind-closure initial-state)))
                      (expect (plist-get res-state :a2a-cb) :not :to-be nil)
                      (expect (gethash task-id macher-agent--pending-callbacks) :not :to-be nil))
                  (kill-buffer child-buf))))

          (it "macher-agent-post-response-reaper reaps subagent when task-finished is t even if ready-to-reap was nil"
              (let* ((sub-buf (generate-new-buffer "sub-post-resp-test"))
                     (scheduled nil))
                (unwind-protect
                    (progn
                      (spy-on 'macher-agent--schedule-buffer-reap :and-call-fake
                              (lambda (buf) (push buf scheduled)))
                      (with-current-buffer sub-buf
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent-task-finished t)
                        (setq-local macher-agent--ready-to-reap nil)
                        (macher-agent-post-response-reaper (point-min) (point-max))
                        (expect (member sub-buf scheduled) :to-be-truthy)))
                  (kill-buffer sub-buf))))

          (it "macher-agent-post-response-reaper strictly avoids conflating FSM with null buffer"
              (let* ((buf1 (generate-new-buffer "orch-null-buf-fsm-1"))
                     (swept-originator nil))
                (unwind-protect
                    (progn
                      (spy-on 'macher-agent-sweep-subagents :and-call-fake
                              (lambda (name) (setq swept-originator name)))
                      ;; Mock FSM with null buffer but active tool calls
                      (let* ((mock-info-null (list :tool-use (list '(:name "delegate_tasks")) :buffer nil))
                             (mock-fsm (list :info mock-info-null))
                             (gptel--fsm mock-fsm))
                        (spy-on 'gptel-fsm-info :and-call-fake
                                (lambda (f) (if (eq f mock-fsm) mock-info-null nil)))
                        (with-current-buffer buf1
                          (setq-local macher-agent--is-subagent nil)
                          (macher-agent-post-response-reaper (point-min) (point-max))
                          ;; buf1 should NOT match the null-buffer FSM and sweeps subagents
                          (expect swept-originator :to-equal (buffer-name buf1)))))
                  (kill-buffer buf1))))

          (it "macher-agent-sweep-subagents resolves scoped keys for dead parent buffers"
              (let* ((dead-parent-name "dead-parent-agent")
                     (scoped-key (format "/test/workspace/root::%s" dead-parent-name))
                     (child-buf (generate-new-buffer "dead-parent-child"))
                     (grandchild-buf (generate-new-buffer "dead-parent-grandchild"))
                     (child-name (buffer-name child-buf))
                     (grandchild-name (buffer-name grandchild-buf))
                     (scheduled nil))
                (unwind-protect
                    (progn
                      (with-current-buffer child-buf
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent-task-finished t)
                        (setq-local macher-agent--ready-to-reap nil))
                      (with-current-buffer grandchild-buf
                        (setq-local macher-agent--is-subagent t)
                        (setq-local macher-agent-task-finished t)
                        (setq-local macher-agent--ready-to-reap nil))
                      (puthash scoped-key (list child-name) macher-agent--a2a-ownership)
                      (puthash child-name (list grandchild-name) macher-agent--a2a-ownership)
                      (spy-on 'macher-agent--schedule-buffer-reap :and-call-fake
                              (lambda (buf) (push buf scheduled)))
                      (macher-agent-sweep-subagents dead-parent-name)
                      (expect (member child-buf scheduled) :to-be-truthy)
                      (expect (member grandchild-buf scheduled) :to-be-truthy)
                      (expect (gethash scoped-key macher-agent--a2a-ownership) :to-be nil)
                      (expect (gethash child-name macher-agent--a2a-ownership) :to-be nil))
                  (kill-buffer child-buf)
                  (kill-buffer grandchild-buf))))

          (it "spawn_subagent passes current-buffer as parent-buf to macher-agent-add-subagent"
              (load (expand-file-name "skills/scripts/spawn_subagent.el") nil t)
              (let* ((caller-buf (generate-new-buffer "caller-parent-buf"))
                     (passed-parent nil)
                     (tool-cmd (or (get 'macher-agent-spawn-subagent-tool 'command-fn)
                                   (get 'macher-agent-tool-spawn-subagent 'command-fn))))
                (unwind-protect
                    (progn
                      (spy-on 'macher-agent-add-subagent :and-call-fake
                              (lambda (_name _presets parent-buf _dir _context)
                                (setq passed-parent parent-buf)
                                caller-buf))
                      (with-current-buffer caller-buf
                        (funcall tool-cmd (list :name "worker-sub") nil nil)
                        (expect passed-parent :to-equal caller-buf)))
                  (kill-buffer caller-buf))))

          (it "macher-agent--remove-active-subagent-registries removes child from all parent ownership lists"
              (let* ((child-name "child-to-remove")
                     (child-buf (generate-new-buffer child-name)))
                (unwind-protect
                    (progn
                      (puthash "parent-1" (list child-name "other-worker") macher-agent--a2a-ownership)
                      (puthash "parent-2" (list child-name) macher-agent--a2a-ownership)
                      (macher-agent--remove-active-subagent-registries child-name child-buf)
                      (expect (gethash "parent-1" macher-agent--a2a-ownership) :to-equal (list "other-worker"))
                      (expect (gethash "parent-2" macher-agent--a2a-ownership) :to-be nil))
                  (kill-buffer child-buf))))

          (it "macher-agent-a2a-pipe--bind-closure respects explicit :suppress-patch nil overriding buffer-local truthy default"
              (let* ((parent-buf (generate-new-buffer "parent-override-suppress"))
                     (child-buf (generate-new-buffer "child-override-suppress"))
                     (orig-entry (cons "file2.txt" (cons "original content" "original content")))
                     (parent-ctx (macher--make-context :contents (list orig-entry)))
                     (task-id "task-override-suppress-123")
                     (results-tbl (make-hash-table :test 'equal))
                     (shared-state (list :results results-tbl
                                         :total 1
                                         :final-callback nil
                                         :parent-buf parent-buf
                                         :parent-fsm nil
                                         :original-payloads (list (list :type 'SEND_MESSAGE :task-id task-id))))
                     (initial-state (list :a2a-msg (list :type 'SEND_MESSAGE
                                                         :task-id task-id
                                                         :metadata (list :suppress-patch nil))
                                          :child-buf child-buf
                                          :shared-state shared-state)))
                (unwind-protect
                    (progn
                      (with-current-buffer parent-buf
                        (setq-local macher-agent--persistent-context parent-ctx))
                      (with-current-buffer child-buf
                        (setq-local macher-agent--suppress-patch t))
                      (macher-agent-a2a-pipe--bind-closure initial-state)
                      (let ((cb (gethash task-id macher-agent--pending-callbacks))
                            (diff-entry (macher-agent-vfs-make-entry "file2.txt" "original content" "new modified content")))
                        (expect cb :not :to-be nil)
                        (funcall cb (list :type 'ARTIFACT_UPDATE
                                          :task-id task-id
                                          :message (list :status 'success
                                                         :data "done"
                                                         :diff (list diff-entry))))
                        ;; Parent context SHOULD be updated because explicit :suppress-patch nil overrode buffer-local t
                        (let ((entry (cl-find "file2.txt" (macher-agent--get-context-contents parent-ctx) :key #'car :test #'equal)))
                          (expect (macher-agent-vfs-entry-curr entry) :to-equal "new modified content"))))
                  (kill-buffer parent-buf)
                  (kill-buffer child-buf)))))

(provide 'macher-agent-test-orchestration)
;;; macher-agent-test-orchestration.el ends here
