;;; test-a2a.el --- Agent-to-agent orchestration, focus semantics, and reaping -*- lexical-binding: t; -*-

;;; Commentary:
;; Specification 6: Agent-to-agent (A2A) orchestration, focus semantics, and reaping
;; A2A-01, A2A-02, A2A-03, A2A-04, A2A-05, A2A-06, A2A-07, A2A-08, A2A-09

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-test-helpers)

(describe "6. Agent-to-agent (A2A) orchestration, focus semantics, and reaping"

  (before-each
    (macher-agent-test-reset-env))

  ;; --------------------------------------------------------------------------
  ;; A2A-01
  ;; --------------------------------------------------------------------------
  (it "A2A-01: delegates tasks via delegate_tasks_to_subagents with routing stack and ordered results"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
      (let* ((tasks (vector (list :buffer_name "*a2a-01-worker-1*" :presets ["default"] :instructions "Compute 2+2")
                            (list :buffer_name "*a2a-01-worker-2*" :presets ["default"] :instructions "Compute 3*3")))
             (tool-fn (get 'macher-agent-delegate-tasks-to-subagents-tool 'ptc-function))
             (aggregated-result nil))
        (with-current-buffer parent-buf
          (cl-letf (((symbol-function 'gptel-send)
                     (lambda ()
                       (let* ((cur-buf (current-buffer))
                              (tid macher-agent--current-task-id)
                              (submit-fn (get 'macher-agent-submit-task-result-tool 'ptc-function))
                              (ans (if (string-match-p "worker-1" (buffer-name cur-buf)) "Answer: 4" "Answer: 9")))
                         ;; Verify sub-agent has routing frame pushed and receives submit_task_result and directive
                         (expect (length macher-agent--routing-stack) :to-equal 1)
                         (expect (plist-get (car macher-agent--routing-stack) :originator-name)
                                 :to-equal (buffer-name parent-buf))
                         (let ((tx-state (macher-agent-run-pipeline
                                          'transmission
                                          (make-macher-agent-transmission-state
                                           :target-buffer cur-buf
                                           :tools (when (boundp 'gptel-tools) gptel-tools)))))
                           (expect (cl-some (lambda (tl) (equal (macher-agent-canonical-tool-name tl) "submit_task_result"))
                                            (macher-agent-transmission-state-tools tx-state))
                                   :to-be-truthy)
                           (expect (macher-agent-transmission-state-compiled-prompt tx-state)
                                   :to-match "submit_task_result"))
                         ;; Call submit_task_result
                         (funcall submit-fn ans tid macher-agent--persistent-context)))))
            ;; Execute delegation
            (let ((thunk (funcall tool-fn tasks parent-ctx)))
              (funcall thunk (lambda (res) (setq aggregated-result res))))))

        ;; Await aggregated results in exact order of input vector
        (macher-agent-test-wait-until (lambda () aggregated-result) 5.0)
        (expect (stringp aggregated-result) :to-be t)
        (expect aggregated-result :to-match "Answer: 4")
        (expect aggregated-result :to-match "Answer: 9")
        (expect (string-match-p "Answer: 4" aggregated-result)
                :to-be-less-than
                (string-match-p "Answer: 9" aggregated-result)))))

  ;; --------------------------------------------------------------------------
  ;; A2A-02
  ;; --------------------------------------------------------------------------
  (it "A2A-02: dispatches tasks via execute_subagents immediately with empty routing stacks"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
      (let* ((tasks (vector (list :buffer_name "*a2a-02-bg-1*" :presets ["default"] :instructions "Background 1")
                            (list :buffer_name "*a2a-02-bg-2*" :presets ["default"] :instructions "Background 2")))
             (tool-fn (get 'macher-agent-execute-subagents-tool 'ptc-function))
             (dispatch-resolved nil))
        (with-current-buffer parent-buf
          (cl-letf (((symbol-function 'gptel-send) #'ignore))
            (let ((thunk (funcall tool-fn tasks parent-ctx)))
              ;; Tool call resolves continuation callback immediately at dispatch time
              (funcall thunk (lambda (res) (setq dispatch-resolved res))))))

        (expect dispatch-resolved :not :to-be nil)
        (expect (stringp dispatch-resolved) :to-be t)
        (expect dispatch-resolved :to-match "SUCCESS: Background sub-agent '\\*a2a-02-bg-1\\*' dispatched")
        (expect dispatch-resolved :to-match "SUCCESS: Background sub-agent '\\*a2a-02-bg-2\\*' dispatched")

        ;; Sub-agents have empty routing stacks and do not have submit_task_result in tool lists
        (let ((bg-buf1 (get-buffer "*a2a-02-bg-1*"))
              (bg-buf2 (get-buffer "*a2a-02-bg-2*")))
          (expect (buffer-live-p bg-buf1) :to-be t)
          (expect (buffer-live-p bg-buf2) :to-be t)
          (with-current-buffer bg-buf1
            (expect macher-agent--routing-stack :to-be nil)
            (expect (cl-some (lambda (tl) (equal (gptel-tool-name tl) "submit_task_result"))
                             gptel-tools)
                    :to-be nil))
          (with-current-buffer bg-buf2
            (expect macher-agent--routing-stack :to-be nil)
            (expect (cl-some (lambda (tl) (equal (gptel-tool-name tl) "submit_task_result"))
                             gptel-tools)
                    :to-be nil))))))

  ;; --------------------------------------------------------------------------
  ;; A2A-03
  ;; --------------------------------------------------------------------------
  (it "A2A-03: invokes macher-agent-display-subagent-fn only for foreground subagents"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
      (let* ((displayed-buffers nil)
             (recorder (lambda (b) (push (buffer-name b) displayed-buffers)))
             (macher-agent-display-subagent-fn recorder)
             (fg-buf (get-buffer-create "*a2a-03-fg*"))
             (bg-buf (get-buffer-create "*a2a-03-bg*")))
        (unwind-protect
            (progn
              ;; Dispatch foreground sub-agent (:background nil)
              (macher-agent-a2a-pipe--acquire-target
               (list :a2a-msg (make-macher-agent-transit-payload :metadata '(:background nil))
                     :target (buffer-name fg-buf)
                     :target-name (buffer-name fg-buf)
                     :shared-state (list :parent-buffer parent-buf)))
              ;; Dispatch background sub-agent (:background t)
              (macher-agent-a2a-pipe--acquire-target
               (list :a2a-msg (make-macher-agent-transit-payload :metadata '(:background t))
                     :target (buffer-name bg-buf)
                     :target-name (buffer-name bg-buf)
                     :shared-state (list :parent-buffer parent-buf)))
              ;; Display function called for foreground, but not for background
              (expect (member "*a2a-03-fg*" displayed-buffers) :to-be-truthy)
              (expect (member "*a2a-03-bg*" displayed-buffers) :to-be nil))
          (when (buffer-live-p fg-buf) (kill-buffer fg-buf))
          (when (buffer-live-p bg-buf) (kill-buffer bg-buf))))))

  ;; --------------------------------------------------------------------------
  ;; A2A-04
  ;; --------------------------------------------------------------------------
  (it "A2A-04: signals contract violation error when submit_task_result is invoked without task-id or routing stack"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (let ((submit-fn (get 'macher-agent-submit-task-result-tool 'ptc-function)))
        ;; In root orchestrator buffer: routing-stack and current-task-id are empty
        (with-current-buffer owner-buf
          (setq-local macher-agent--routing-stack nil)
          (setq-local macher-agent--current-task-id nil)
          (expect (funcall submit-fn "Premature answer" nil ctx)
                  :to-throw 'error))

        ;; Inside a sub-agent launched with :background t
        (let ((bg-buf (generate-new-buffer "*a2a-04-bg-orphan*")))
          (unwind-protect
              (with-current-buffer bg-buf
                (setq-local macher-agent--is-background t)
                (setq-local macher-agent--routing-stack nil)
                (setq-local macher-agent--current-task-id nil)
                (expect (funcall submit-fn "Orphan answer" nil nil)
                        :to-throw 'error))
            (when (buffer-live-p bg-buf) (kill-buffer bg-buf)))))))

  ;; --------------------------------------------------------------------------
  ;; A2A-05
  ;; --------------------------------------------------------------------------
  (it "A2A-05: detects circular routing and halts transmission with error payload"
    (let* ((parent-name "*a2a-05-circular-parent*")
           (parent-buf (get-buffer-create parent-name))
           (msg (make-macher-agent-transit-payload
                 :target-buffer parent-name
                 :metadata (list :buffer_name parent-name)))
           (state (list :a2a-msg msg
                        :shared-state (list :parent-buffer parent-buf))))
      (unwind-protect
          (let* ((res (macher-agent-a2a-pipe--validate-routing state))
                 (err-payload (plist-get res :error-payload)))
            (expect err-payload :not :to-be nil)
            (expect (plist-get err-payload :status) :to-equal 'error)
            (expect (plist-get err-payload :error) :to-match "Circular routing detected"))
        (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

  ;; --------------------------------------------------------------------------
  ;; A2A-06
  ;; --------------------------------------------------------------------------
  (it "A2A-06: assigns fresh unique UUID when task-id collides with existing registered callback"
    (let* ((existing-id "task-collision-id-100")
           (child-buf (get-buffer-create "*a2a-06-child*"))
           (state (list :a2a-msg (make-macher-agent-transit-payload :task-id existing-id)
                        :child-buf child-buf
                        :shared-state (list :results (make-hash-table :test 'equal)
                                            :total 1
                                            :final-callback nil
                                            :parent-buffer (current-buffer)
                                            :original-payloads nil))))
      (unwind-protect
          (progn
            ;; Pre-register the existing ID
            (puthash existing-id #'ignore macher-agent--task-callbacks)
            (let* ((bound-state (macher-agent-a2a-pipe--bind-closure state))
                   (msg (plist-get bound-state :a2a-msg))
                   (assigned-id (macher-agent-transit-payload-task-id msg)))
              ;; Pipeline assigns a fresh unique UUID
              (expect assigned-id :not :to-equal existing-id)
              (expect (string-prefix-p "task-" assigned-id) :to-be t)
              ;; Existing callback is preserved
              (expect (gethash existing-id macher-agent--task-callbacks) :to-equal #'ignore)))
        (remhash existing-id macher-agent--task-callbacks)
        (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

  ;; --------------------------------------------------------------------------
  ;; A2A-07
  ;; --------------------------------------------------------------------------
  (it "A2A-07: triggers parent error callback when awaited sub-agent halts without submitting result"
    (let* ((task-id "task-halt-07")
           (child-buf (get-buffer-create "*a2a-07-child*"))
           (error-received nil))
      (unwind-protect
          (progn
            (puthash task-id (lambda (res) (setq error-received res)) macher-agent--pending-callbacks)
            (with-current-buffer child-buf
              (setq-local macher-agent--current-task-id task-id)
              (setq-local macher-agent-task-finished nil)
              (setq-local macher-agent--is-ephemeral t)
              ;; Simulate LLM turn response completing without invoking submit_task_result
              (macher-agent-post-response-reaper (point-min) (point-max)))

            ;; Parent error callback is invoked with failure notification
            (expect error-received :not :to-be nil)
            (expect (plist-get error-received :status) :to-equal 'error)
            (expect (plist-get error-received :error) :to-match "Agent halted without invoking submit_task_result\\|Sub-agent halted before submitting task result")
            ;; Pending callback is cleaned up
            (expect (gethash task-id macher-agent--pending-callbacks) :to-be nil))
        (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

  ;; --------------------------------------------------------------------------
  ;; A2A-08
  ;; --------------------------------------------------------------------------
  (it "A2A-08: kills ephemeral sub-agent upon turn completion while persistent sub-agent survives"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
      (let* ((eph-buf (macher-agent-add-subagent "*a2a-08-ephemeral*" nil parent-buf nil parent-ctx))
             (pers-buf (macher-agent-add-subagent "*a2a-08-persistent*" nil parent-buf nil parent-ctx)))
        ;; Configure ephemeral sub-agent
        (with-current-buffer eph-buf
          (setq-local macher-agent--is-ephemeral t)
          (setq-local macher-agent-task-finished t)
          (setq-local macher-agent--ready-to-reap t))
        ;; Configure persistent sub-agent
        (with-current-buffer pers-buf
          (setq-local macher-agent--is-ephemeral nil)
          (setq-local macher-agent-task-finished t)
          (setq-local macher-agent--ready-to-reap nil))

        ;; Register in active subagents and ownership
        (setq macher-agent-active-subagents
              (list (cons (buffer-name eph-buf) root)
                    (cons (buffer-name pers-buf) root)))
        (puthash (buffer-name parent-buf)
                 (list (buffer-name eph-buf) (buffer-name pers-buf))
                 macher-agent--a2a-ownership)

        ;; Trigger reaping on both
        (macher-agent--reap-buffer eph-buf)
        (macher-agent--reap-buffer pers-buf)

        ;; Ephemeral sub-agent is killed and cleaned from ownership
        (expect (buffer-live-p eph-buf) :to-be nil)
        (expect (assoc (buffer-name eph-buf) macher-agent-active-subagents) :to-be nil)
        (expect (member "*a2a-08-ephemeral*"
                        (gethash (buffer-name parent-buf) macher-agent--a2a-ownership))
                :to-be nil)

        ;; Persistent sub-agent buffer remains live and registered
        (expect (buffer-live-p pers-buf) :to-be t)
        (expect (assoc "*a2a-08-persistent*" macher-agent-active-subagents) :not :to-be nil)
        (kill-buffer pers-buf))))

  ;; --------------------------------------------------------------------------
  ;; A2A-09
  ;; --------------------------------------------------------------------------
  (it "A2A-09: explicitly reaps persistent sub-agent when marked ready-to-reap"
    (let ((pers-buf (get-buffer-create "*a2a-09-pers*"))
          (parent-name "*a2a-09-parent*"))
      (unwind-protect
          (progn
            (with-current-buffer pers-buf
              (setq-local macher-agent--is-ephemeral nil)
              (setq-local macher-agent-task-finished t)
              ;; Explicitly mark as reapable
              (setq-local macher-agent--ready-to-reap t))
            (setq macher-agent-active-subagents (list (cons "*a2a-09-pers*" "/tmp")))
            (puthash parent-name (list "*a2a-09-pers*") macher-agent--a2a-ownership)

            ;; Call reaper
            (macher-agent--reap-buffer pers-buf)

            ;; Buffer is killed and cleaned from all registries regardless of ephemeral flag
            (expect (buffer-live-p pers-buf) :to-be nil)
            (expect (assoc "*a2a-09-pers*" macher-agent-active-subagents) :to-be nil)
            (expect (gethash parent-name macher-agent--a2a-ownership) :to-equal nil))
        (when (buffer-live-p pers-buf) (kill-buffer pers-buf))))))

(provide 'test-a2a)
;;; test-a2a.el ends here
