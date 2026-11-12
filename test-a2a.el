;;; test-a2a.el --- Agent-to-agent orchestration, focus semantics, and reaping -*- lexical-binding: t; -*-

;;; Commentary:
;; Specification 6: Agent-to-agent (A2A) orchestration, focus semantics, and reaping
;; A2A-01, A2A-02, A2A-03, A2A-04, A2A-05, A2A-06, A2A-07, A2A-08, A2A-09, A2A-10, A2A-11, A2A-12

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
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
        (when (buffer-live-p pers-buf) (kill-buffer pers-buf)))))

  ;; --------------------------------------------------------------------------
  ;; A2A-10
  ;; --------------------------------------------------------------------------
  (it "A2A-10: rejects sub-agent task batch containing duplicate target buffer names"
    (let* ((payload-1 (make-macher-agent-transit-payload
                       :transit-type :task-delegation
                       :type 'TASK_DELEGATION
                       :task-id "task-dup-1"
                       :metadata (list :buffer_name "*a2a-worker-dup*")))
           (payload-2 (make-macher-agent-transit-payload
                       :transit-type :task-delegation
                       :type 'TASK_DELEGATION
                       :task-id "task-dup-2"
                       :metadata (list :buffer_name "*a2a-worker-dup*")))
           (batch (vector payload-1 payload-2))
           (dispatch-result nil))
      (macher-agent-a2a-dispatch
       batch
       (lambda (res)
         (setq dispatch-result res)))
      (expect dispatch-result :to-match "ERROR: Duplicate target buffer in sub-agent batch.")))

  ;; --------------------------------------------------------------------------
  ;; A2A-11
  ;; --------------------------------------------------------------------------
  (it "A2A-11: rejects delegation to target sub-agent buffer already running a task"
    (let* ((busy-buf (get-buffer-create "*a2a-busy-worker*"))
           (task-id "task-busy-01")
           (ctx (macher-agent--make-context :id "busy-ctx"))
           (session (macher-agent-context-session ctx)))
      (unwind-protect
          (progn
            (setf (macher-agent-session-state session) :running)
            (with-current-buffer busy-buf
              (setq-local macher-agent--persistent-context ctx))
            (let* ((msg (make-macher-agent-transit-payload
                         :transit-type :task-delegation
                         :type 'TASK_DELEGATION
                         :task-id task-id
                         :metadata (list :buffer_name (buffer-name busy-buf))))
                   (state (list :a2a-msg msg
                                :shared-state (list :parent-buffer (current-buffer))))
                   (res (macher-agent-a2a-pipe--validate-routing state))
                   (err-payload (plist-get res :error-payload)))
              (expect err-payload :not :to-be nil)
              (expect (plist-get err-payload :status) :to-equal 'error)
              (expect (plist-get err-payload :error) :to-match "already busy running a task")))
        (when (buffer-live-p busy-buf) (kill-buffer busy-buf)))))

  ;; --------------------------------------------------------------------------
  ;; A2A-12
  ;; --------------------------------------------------------------------------
  (it "A2A-12: unwinds all routing stack frames when sub-agent halts prematurely"
    (let* ((task-id-1 "nested-task-parent")
           (task-id-2 "nested-task-child")
           (subagent-buf (get-buffer-create "*a2a-nested-subagent*"))
           (errors-received nil))
      (unwind-protect
          (progn
            ;; Register callbacks for both nested callers
            (puthash task-id-1
                     (lambda (res) (push (cons task-id-1 res) errors-received))
                     macher-agent--pending-callbacks)
            (puthash task-id-2
                     (lambda (res) (push (cons task-id-2 res) errors-received))
                     macher-agent--pending-callbacks)
            (with-current-buffer subagent-buf
              ;; Push two routing frames
              (macher-agent--push-routing task-id-1 "*parent-caller-1*" t)
              (macher-agent--push-routing task-id-2 "*parent-caller-2*" t)
              (setq-local macher-agent-task-finished nil)
              (setq-local macher-agent--is-ephemeral nil)
              ;; Trigger premature halt reaper
              (macher-agent-post-response-reaper (point-min) (point-max)))

            ;; Both callbacks were invoked and routing stack is completely empty
            (expect (length errors-received) :to-equal 2)
            (expect (with-current-buffer subagent-buf macher-agent--routing-stack) :to-equal nil)
            (expect (with-current-buffer subagent-buf macher-agent--current-task-id) :to-equal nil)
            (let ((err1 (cdr (assoc task-id-1 errors-received)))
                  (err2 (cdr (assoc task-id-2 errors-received))))
              (expect (plist-get err1 :status) :to-equal 'error)
              (expect (plist-get err2 :status) :to-equal 'error)
              (expect (plist-get err1 :error) :to-match "Sub-agent halted before submitting task result")
              (expect (plist-get err2 :error) :to-match "Sub-agent halted before submitting task result")))
        (when (buffer-live-p subagent-buf) (kill-buffer subagent-buf)))))

  ;; --------------------------------------------------------------------------
  ;; A2A-13
  ;; --------------------------------------------------------------------------
  (it "A2A-13: transitions TRET directly to DONE on staged submit_task_result without firing extra WAIT request"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
      (let* ((child-buf (macher-agent-add-subagent "*a2a-13-worker*" nil parent-buf nil parent-ctx))
             (child-ctx (with-current-buffer child-buf macher-agent--persistent-context))
             (wait-entries 0)
             (delivered-payload nil)
             (task-id "task-a2a-13")
             (fsm (gptel-make-fsm
                   :state 'TOOL
                   :table (copy-tree gptel-send--transitions)
                   :handlers `((WAIT ,(lambda (_f) (setq wait-entries (1+ wait-entries))))
                               (TRET ,(lambda (f) (gptel--fsm-transition f)))
                               (DONE ,#'ignore))
                   :info (list :buffer child-buf
                               :macher-agent-context child-ctx
                               :tool-result t))))
        (unwind-protect
            (progn
              (macher-agent-register-task-callback
               task-id (lambda (msg) (setq delivered-payload msg)))
              (with-current-buffer child-buf
                (setq-local macher-agent--is-ephemeral t)
                (setq-local macher-agent--active-fsm fsm)
                (macher-agent--push-routing task-id (buffer-name parent-buf) t)
                ;; Hijack FSM to install TRET -> DONE edge and DONE flush handler
                (macher-agent--fsm-hijack-transform nil fsm)
                ;; Call submit_task_result while FSM is in TOOL state
                (let ((ack (funcall (get 'macher-agent-submit-task-result-tool 'ptc-function)
                                    "Direct DONE answer" task-id child-ctx)))
                  (expect ack :to-match "SUCCESS: Result submitted"))
                ;; Submission is staged; parent has NOT woken yet while worker is still in TOOL
                (expect macher-agent--staged-submission :not :to-be nil)
                (expect delivered-payload :to-be nil)
                ;; Advance TOOL -> TRET -> DONE
                (gptel--fsm-transition fsm))

              ;; FSM transitioned directly to DONE with zero WAIT entries
              (expect (gptel-fsm-state fsm) :to-equal 'DONE)
              (expect wait-entries :to-equal 0)
              (expect delivered-payload :not :to-be nil)
              (expect (plist-get (macher-agent-transit-payload-payload delivered-payload) :payload)
                      :to-equal "Direct DONE answer")
              ;; Ephemeral worker reaped asynchronously via timer
              (macher-agent-test-wait-until (lambda () (not (buffer-live-p child-buf))) 2.0)
              (expect (buffer-live-p child-buf) :to-be nil))
          (when (buffer-live-p child-buf)
            (kill-buffer child-buf))))))

  ;; --------------------------------------------------------------------------
  ;; A2A-14
  ;; --------------------------------------------------------------------------
  (it "A2A-14: preserves :persistent sub-agents spawned via spawn_subagent across delegation and initialises gptel-mode with nil presets"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
      (let* ((spawn-fn (get 'macher-agent-spawn-subagent-tool 'ptc-function))
             (del-fn (get 'macher-agent-delegate-tasks-to-subagents-tool 'ptc-function))
             (pers-name (with-current-buffer parent-buf
                          (funcall spawn-fn "*a2a-14-persistent*" nil parent-ctx)))
             (pers-buf (get-buffer pers-name))
             (res-1 nil)
             (res-2 nil))
        (unwind-protect
            (progn
              (expect (buffer-live-p pers-buf) :to-be t)
              (expect (macher-agent-session-lifecycle
                       (macher-agent-context-session
                        (buffer-local-value 'macher-agent--persistent-context pers-buf)))
                      :to-equal :persistent)

              ;; 1. Delegate to persistent worker AND a brand-new nil-preset worker
              (with-current-buffer parent-buf
                (cl-letf (((symbol-function 'gptel-send)
                           (lambda ()
                             (let ((cur (current-buffer))
                                   (submit-fn (get 'macher-agent-submit-task-result-tool 'ptc-function)))
                               ;; Both buffers MUST have gptel-mode and prompt transformer installed
                               (expect (bound-and-true-p gptel-mode) :to-be t)
                               (expect (memq #'macher-agent-sync-prompt-transformer
                                             gptel-prompt-transform-functions)
                                       :to-be-truthy)
                               (funcall submit-fn
                                        (format "Done by %s" (buffer-name cur))
                                        macher-agent--current-task-id
                                        macher-agent--persistent-context)
                               (macher-agent--finalize-buffer-turn cur nil)))))
                  (let ((thunk (funcall del-fn
                                        (vector (list :buffer_name pers-name
                                                      :instructions "First task")
                                                (list :buffer_name "*a2a-14-nil-preset*"
                                                      :presets nil
                                                      :instructions "Ephemeral task"))
                                        parent-ctx)))
                    (funcall thunk (lambda (r) (setq res-1 r))))
                  (macher-agent-test-wait-until (lambda () res-1) 2.0)
                  (expect res-1 :to-match "Done by \\*a2a-14-persistent\\*")
                  (expect res-1 :to-match "Done by \\*a2a-14-nil-preset\\*")

                  ;; Ephemeral worker is reaped, whereas persistent worker remains live & :idle!
                  (macher-agent-test-wait-until
                   (lambda () (not (buffer-live-p (get-buffer "*a2a-14-nil-preset*")))) 2.0)
                  (expect (buffer-live-p pers-buf) :to-be t)
                  (with-current-buffer pers-buf
                    (expect macher-agent--is-ephemeral :to-be nil)
                    (let ((sess (macher-agent-context-session macher-agent--persistent-context)))
                      (expect (macher-agent-session-lifecycle sess) :to-equal :persistent)
                      (expect (macher-agent-session-state sess) :to-equal :idle)))

                  ;; 2. Immediate second delegation to the persistent worker succeeds (not busy!)
                  (let ((thunk2 (funcall del-fn
                                         (vector (list :buffer_name pers-name
                                                       :instructions "Second task"))
                                         parent-ctx)))
                    (funcall thunk2 (lambda (r) (setq res-2 r))))
                  (macher-agent-test-wait-until (lambda () res-2) 2.0)
                  (expect res-2 :to-equal "Done by *a2a-14-persistent*"))))
          (when (buffer-live-p pers-buf)
            (kill-buffer pers-buf))))))

  ;; --------------------------------------------------------------------------
  ;; A2A-15
  ;; --------------------------------------------------------------------------
  (it "A2A-15: purges pending callbacks and unwinds child routing frames when waiting parent aborts"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
      (let* ((del-fn (get 'macher-agent-delegate-tasks-to-subagents-tool 'ptc-function))
             (aborted-turn-resolved nil)
             (fresh-turn-resolved nil))
        (with-current-buffer parent-buf
          ;; 1. Launch slow delegation (gptel-send leaves child running without submitting)
          (cl-letf (((symbol-function 'gptel-send) #'ignore))
            (let ((thunk (funcall del-fn
                                  (vector (list :buffer_name "*a2a-15-slow-child*"
                                                :instructions "Long task"))
                                  parent-ctx)))
              (funcall thunk (lambda (r) (setq aborted-turn-resolved r)))))

          (let ((slow-child (get-buffer "*a2a-15-slow-child*")))
            (expect (buffer-live-p slow-child) :to-be t)
            (expect (> (hash-table-count macher-agent--task-callbacks) 0) :to-be t)

            ;; 2. Parent aborts (ABRT)
            (let ((parent-fsm (gptel-make-fsm :state 'ABRT
                                              :info (list :buffer parent-buf
                                                          :macher-agent-context parent-ctx))))
              (macher-agent-gptel--trigger-flush parent-fsm))

            ;; Stale task callbacks purged and child routing stack unwound
            (expect (hash-table-count macher-agent--task-callbacks) :to-equal 0)
            (expect aborted-turn-resolved :to-be nil)
            (macher-agent-test-wait-until (lambda () (not (buffer-live-p slow-child))) 2.0)

            ;; 3. Subsequent fresh delegation on parent-buf resolves cleanly with fresh result
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda ()
                         (funcall (get 'macher-agent-submit-task-result-tool 'ptc-function)
                                  "FRESH_ANSWER"
                                  macher-agent--current-task-id
                                  macher-agent--persistent-context))))
              (let ((thunk2 (funcall del-fn
                                     (vector (list :buffer_name "*a2a-15-fresh-child*"
                                                   :instructions "Quick task"))
                                     parent-ctx)))
                (funcall thunk2 (lambda (r) (setq fresh-turn-resolved r)))))
            (macher-agent-test-wait-until (lambda () fresh-turn-resolved) 2.0)
            (expect fresh-turn-resolved :to-equal "FRESH_ANSWER")))))))

(provide 'test-a2a)
;;; test-a2a.el ends here
