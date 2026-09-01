;;; macher-agent-integration-test.el --- Tests for macher-agent-skills -*- lexical-binding: t; -*-

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

(require 'buttercup)
(require 'macher-agent-test-setup)
(require 'macher-agent)
(require 'macher-agent-macher nil t)
(require 'macher-agent-orchestration)
(require 'macher-agent-vfs)
(require 'macher-agent-test-harness)

(defvar macher-agent--garbage-queue nil)
(put 'macher-agent--ready-to-reap 'permanent-local t)

(defun macher-agent--reap-buffers-on-idle ()
  "Reap all buffers that are marked ready-to-reap."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (bound-and-true-p macher-agent--ready-to-reap)
          (macher-agent--reap-buffer buf))))))

(describe "Macher-Agent Orchestration Integration"
          (before-each
           (setq macher-agent--garbage-queue nil)
           (setq gptel-api-key "mock-key")
           (let* ((ws (make-macher-agent-workspace :project-root (expand-file-name default-directory)))
                  (ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))
             (setq-local macher-agent--persistent-context ctx)
             (puthash (expand-file-name default-directory) ctx macher-agent-active-workspaces)
             (macher-agent-initialize-skills ctx (or (bound-and-true-p macher-agent--bundled-skills-dir)
                                                     (bound-and-true-p macher-agent-bundled-skills-directory))))

           ;; 1. Mock the LLM: Intercept gptel-send to act as the AI for the sub-agents
           (let ((orig-send (symbol-function 'gptel-send)))
             (spy-on 'gptel-send :and-call-fake
                     (lambda (&rest args)
                       (let ((buf (current-buffer))
                             (name (buffer-name (current-buffer))))
                         (cond
                          ((string-match-p "agent-france" name)
                           (with-current-buffer buf
                             (setq-local macher-agent--ready-to-reap t))
                           (let* ((ctx (macher-agent-context-from-buffer buf))
                                  (tool (or (when ctx (gethash "submit_task_result" (macher-agent-workspace-tools-registry (macher-agent-context-workspace ctx))))
                                            macher-agent-submit-task-result-tool))
                                  (submit-fn (gptel-tool-function tool)))
                             (funcall submit-fn (lambda (_) nil) :final_answer "The capital of France is Paris.")))
                          
                          ((string-match-p "agent-spain" name)
                           (with-current-buffer buf
                             (setq-local macher-agent--ready-to-reap t))
                           (let* ((ctx (macher-agent-context-from-buffer buf))
                                  (tool (or (when ctx (gethash "submit_task_result" (macher-agent-workspace-tools-registry (macher-agent-context-workspace ctx))))
                                            macher-agent-submit-task-result-tool))
                                  (submit-fn (gptel-tool-function tool)))
                             (funcall submit-fn (lambda (_) nil) :final_answer "The capital of Spain is Madrid.")))
                          (t
                           (when (functionp orig-send)
                             (apply orig-send args))))))))

           ;; 2. Mock timers: Force deferred buffer cleanups to happen synchronously
           (spy-on 'run-at-time :and-call-fake
                   (lambda (_time _repeat fn &rest args)
                     (apply fn args)))
           (spy-on 'run-with-idle-timer :and-call-fake
                   (lambda (_secs _repeat fn &rest args)
                     (apply fn args))))

          (it "executes the full workflow: spawn -> delegate -> await responses -> return combined result"
              (let* ((master-buf (get-buffer-create "*orchestrator-test*"))
                     (final-result nil))
                
                (with-current-buffer master-buf
                  
                  ;; --- A. Setup the Master Orchestrator Context ---
                  (let ((macher-agent--allow-lazy-init t))
                    (let* ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                                    (macher-agent-context-from-buffer (current-buffer))))
                           (spawn-tool (or (when ctx (gethash "spawn_subagent" (macher-agent-workspace-tools-registry (macher-agent-context-workspace ctx))))
                                           (bound-and-true-p macher-agent-spawn-subagent-tool)))
                           (delegate-tool (or (when ctx (gethash "delegate_tasks" (macher-agent-workspace-tools-registry (macher-agent-context-workspace ctx))))
                                              (when ctx (gethash "delegate_tasks_to_subagents" (macher-agent-workspace-tools-registry (macher-agent-context-workspace ctx))))
                                              (bound-and-true-p macher-agent-delegate-tasks-tool)
                                              (bound-and-true-p macher-agent-delegate-tasks-to-subagents-tool))))
                      
                      ;; --- B. Spawn Sub-agents via Tool ---
                      (let ((spawn-fn (gptel-tool-function spawn-tool)))
                        (if (gptel-tool-async spawn-tool)
                            (progn
                              (funcall spawn-fn (lambda (res) (setq final-result (cons 'spawn1 res))) "agent-france")
                              (funcall spawn-fn (lambda (res) (setq final-result (cons 'spawn2 res))) "agent-spain"))
                          (funcall spawn-fn nil "agent-france")
                          (funcall spawn-fn nil "agent-spain")))

                      (unless (buffer-live-p (get-buffer "agent-france"))
                        (error "SPAWN FAILED! final-result=%S spawn-tool=%S" final-result spawn-tool))
                      
                      (expect (buffer-live-p (get-buffer "agent-france")) :to-be t)
                      (expect (buffer-live-p (get-buffer "agent-spain")) :to-be t)

                      ;; --- C. Delegate Tasks via Tool ---
                      (let ((tasks (vector
                                    (list :buffer_name "agent-france"
                                          :instructions "What is the capital of France?"
                                          :preset "@macher-agent-worker")
                                    (list :buffer_name "agent-spain"
                                          :instructions "What is the capital of Spain?"
                                          :preset "@macher-agent-worker")))
                            (delegate-fn (gptel-tool-function delegate-tool)))
                        
                        (funcall delegate-fn
                                 (lambda (result)
                                   (setq final-result result))
                                 :tasks tasks)

                        ;; --- D. Assertions ---
                        (expect final-result :to-be-truthy)
                        
                        (expect final-result :to-match "=== Response from sub-agent ===")
                        (expect final-result :to-match "The capital of France is Paris.")
                        (expect final-result :to-match "The capital of Spain is Madrid.")
                        
                        ;; --- E. Reaper Invocation ---
                        (macher-agent--reap-buffers-on-idle)
                        (expect (buffer-live-p (get-buffer "agent-france")) :to-be nil)
                        (expect (buffer-live-p (get-buffer "agent-spain")) :to-be nil)))))))

          (it "routes subagent task execution and artifact updates via point-to-point A2A dispatch"
              (let* ((parent-buf (get-buffer-create "*event-bus-parent*"))
                     (results nil))
                (with-current-buffer parent-buf
                  (macher-agent-add-subagent "agent-france")
                  (macher-agent-a2a-dispatch
                   (list (macher-agent-make-a2a-payload
                          :type 'SEND_MESSAGE
                          :task-id "fake-task"
                          :metadata (list :buffer_name "agent-france" :presets "@macher-agent-worker")
                          :payload (list :instructions "What is the capital of France?")))
                   (lambda (res)
                     (setq results res)))
                  (expect results :to-be-truthy)
                  (let ((m (or (plist-get (aref results 0) :message) (aref results 0))))
                    (expect (if (stringp m) m (or (plist-get m :message) (plist-get m :data))) :to-match "Paris")))))

          (it "handles VFS resource lock acquisition and notification via point-to-point A2A callback"
              (let* ((parent-buf (get-buffer-create "*lock-test-parent*"))
                     (lock-result nil)
                     (task-id "task-lock-001")
                     (resource-path "src/critical-file.el"))
                (with-current-buffer parent-buf
                  (puthash task-id (buffer-name parent-buf) macher-agent--task-registry)
                  (puthash resource-path (lambda (res) (setq lock-result res)) macher-agent--pending-callbacks)
                  (macher-agent--vfs-a2a-callback
                   `(:type ACQUIRE_LOCK
                           :task-id ,task-id
                           :metadata (:resource_path ,resource-path)))
                  (expect (gethash resource-path macher-agent--vfs-lock-table) :to-equal (cons task-id 1))
                  (expect lock-result :to-equal "Resource lock acquired."))))

          (it "invokes macher-agent--vfs-a2a-callback directly from wait_for_vfs_semaphore"
              (let* ((lock-result nil)
                     (resource-path "src/semaphore-test.el")
                     (ctx (or (bound-and-true-p macher-agent--persistent-context)
                              (macher-agent-context-from-buffer (current-buffer))))
                     (sem-tool (or (when ctx (gethash "wait_for_vfs_semaphore"
                                                      (macher-agent-workspace-tools-registry
                                                       (macher-agent-context-workspace ctx))))
                                   (bound-and-true-p macher-agent-wait-for-vfs-semaphore)))
                     (tool-fn (gptel-tool-function sem-tool)))
                (funcall tool-fn
                         (lambda (res) (setq lock-result res))
                         :path resource-path)
                (expect (gethash resource-path macher-agent--vfs-lock-table) :to-be-truthy)
                (expect lock-result :to-match "Resource lock acquired")))

          (it "interleaves macher-agent tools and macher tools seamlessly across turns"
              (let* ((call-count 0)
                     (temp-dir (make-temp-file "macher-ws-" t))
                     (parent-buffer (generate-new-buffer "*macher-test-interleave*")))
                (unwind-protect
                    (let ((default-directory (file-name-as-directory (expand-file-name temp-dir))))
                      (cl-letf (((symbol-function 'macher-agent-resolve-workspace-root)
                                 (lambda (&rest _) (file-name-as-directory (expand-file-name temp-dir))))
                                ((symbol-function 'vc-root-dir)
                                 (lambda (&rest _) (file-name-as-directory (expand-file-name temp-dir))))
                                ((symbol-function 'project-current)
                                 (lambda (&rest _) (cons 'transient (file-name-as-directory (expand-file-name temp-dir))))))
                        (with-current-buffer parent-buffer
                          (setq default-directory (file-name-as-directory (expand-file-name temp-dir)))
                          (when (fboundp 'markdown-mode) (markdown-mode))
                          (when (fboundp 'gptel-mode) (gptel-mode 1))
                          (when (fboundp 'macher-agent-mode) (macher-agent-mode 1))

                          ;; Prevent interactive diff patch generation from deadlocking the test
                          (setq-local macher-agent--suppress-patch t)

                          (insert "Interleave macher-agent and macher tool edits.")

                          (with-macher-agent-test-context
                           '(("macher-test-interleave" .
                              ((:text nil :tool-use (("write_buffer_in_workspace" "interleave-full.txt" "Agent Edit 1")))
                               (:text nil :tool-use (("read_buffer_in_workspace" "interleave-full.txt")))
                               (:text "Read final interleaved edit" :tool-use nil))))
                           call-count

                           (fset 'macher-agent--mock-midturn
                                 (lambda (&rest _)
                                   (remove-hook 'macher-agent-post-tool-use-hook #'macher-agent--mock-midturn)
                                   (macher-agent-context-update ctx "interleave-full.txt" "Macher Edit 2 Overwrite")))
                           (add-hook 'macher-agent-post-tool-use-hook #'macher-agent--mock-midturn)

                           (unwind-protect
                               (progn
                                 (if (and (fboundp 'macher-agent-send) (symbol-function 'macher-agent-send))
                                     (funcall #'macher-agent-send)
                                   (gptel-send))

                                 (let ((timeout 100))
                                   (while (and (< call-count 3) (> timeout 0))
                                     (sleep-for 0.02)
                                     (accept-process-output nil 0.02)
                                     (cl-decf timeout)))

                                 (expect (>= call-count 2) :to-be t)
                                 (let ((norm-key (macher-agent--normalize-path-key "interleave-full.txt" ctx)))
                                   (expect (macher-agent--read-context-file ctx norm-key) :to-equal "Macher Edit 2 Overwrite")))
                             (remove-hook 'macher-agent-post-tool-use-hook #'macher-agent--mock-midturn))))))

                  (when (buffer-live-p parent-buffer) (kill-buffer parent-buffer))
                  (when (file-directory-p temp-dir) (delete-directory temp-dir t))))))
(provide 'macher-agent-integration-test)
;;; macher-agent-integration-test.el ends here
