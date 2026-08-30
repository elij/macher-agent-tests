;;; macher-agent-message-test.el --- Point-to-point message passing tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Point-to-point message passing tests for macher-agent.

;;; Code:
(require 'buttercup)
(require 'macher-agent-macher)
(require 'macher-agent)

(describe "Point-to-point message passing tools"

          (before-each
           (clrhash macher-agent--pending-callbacks)
           (load (expand-file-name "skills/scripts/wait_for_message.el") nil t)
           (load (expand-file-name "skills/scripts/send_message.el") nil t))

          (describe "wait_for_message"
                    (it "registers the current buffer in pending callbacks"
                        (let* ((buf (get-buffer-create "*test-wait-buf*"))
                               (wait-fn (gptel-tool-function macher-agent-wait-for-message-tool))
                               (resolved nil)
                               (resolve-cb (lambda (msg) (setq resolved msg))))
                          (with-current-buffer buf
                            (funcall wait-fn resolve-cb)
                            (expect (gethash (buffer-name buf) macher-agent--pending-callbacks) :not :to-be nil))
                          (kill-buffer buf)))

                    (it "formats success message via success-fn"
                        (let* ((buf (get-buffer-create "*test-wait-fmt-buf*"))
                               (wait-fn (gptel-tool-function macher-agent-wait-for-message-tool))
                               (resolved nil)
                               (resolve-cb (lambda (msg) (setq resolved msg))))
                          (with-current-buffer buf
                            (funcall wait-fn resolve-cb)
                            (let ((pending-cb (gethash (buffer-name buf) macher-agent--pending-callbacks)))
                              (funcall pending-cb "Hello from sender")))
                          (expect resolved :to-equal "SYSTEM: Message received! Waking up.\n\n=== MESSAGE ===\nHello from sender")
                          (kill-buffer buf))))

          (describe "send_message"
                    (it "sends a message to a waiting subagent via A2A dispatch without blocking"
                        (let* ((target-buf (get-buffer-create "*test-target-waiting*"))
                               (sender-buf (get-buffer-create "*test-sender*"))
                               (wait-fn (gptel-tool-function macher-agent-wait-for-message-tool))
                               (send-fn (gptel-tool-function macher-agent-send-message-tool))
                               (received-msg nil)
                               (sender-reply nil))

                          (with-current-buffer target-buf
                            (funcall wait-fn (lambda (msg) (setq received-msg msg))))

                          (with-current-buffer sender-buf
                            (funcall send-fn (lambda (reply) (setq sender-reply reply))
                                     :buffer_name (buffer-name target-buf)
                                     :message "Task for subagent"))

                          (expect received-msg :to-equal "SYSTEM: Message received! Waking up.\n\n=== MESSAGE ===\nTask for subagent")
                          (expect (gethash (buffer-name target-buf) macher-agent--pending-callbacks) :to-be nil)

                          (expect sender-reply :to-equal "SYSTEM: Message successfully dispatched to *test-target-waiting*.")

                          (kill-buffer target-buf)
                          (kill-buffer sender-buf)))

                    (it "returns an error if the target buffer does not exist"
                        (let* ((sender-buf (get-buffer-create "*test-sender-spawn*"))
                               (send-fn (gptel-tool-function macher-agent-send-message-tool))
                               (target-buf-name "*test-spawned-subagent*")
                               (sender-reply nil))

                          (with-current-buffer sender-buf
                            (funcall send-fn (lambda (reply) (setq sender-reply reply))
                                     :buffer_name target-buf-name
                                     :message "Instructions for new subagent"))

                          (expect (get-buffer target-buf-name) :to-be nil)
                          (expect sender-reply :to-equal (format "ERROR: Target agent buffer '%s' does not exist." target-buf-name))
                          (kill-buffer sender-buf))))

          (describe "routing stack behavior"
                    (it "restores the previous routing frame and transmits ARTIFACT_UPDATE after popping frame"
                        (load (expand-file-name "skills/scripts/submit_task_result.el") nil t)
                        (let* ((root-buf (get-buffer-create "*test-root-parent*"))
                               (child-buf (get-buffer-create "*test-child-agent*"))
                               (peer-buf (get-buffer-create "*test-peer-agent*"))
                               (submit-fn (gptel-tool-function macher-agent-submit-task-result-tool))
                               (root-received nil)
                               (peer-received nil))

                          ;; 1. Root registers task and callback
                          (puthash "root-task-1" (lambda (res) (setq root-received res)) macher-agent--pending-callbacks)
                          (with-current-buffer child-buf
                            (macher-agent--push-routing "root-task-1" (buffer-name root-buf)))

                          ;; 2. Peer communicates with child via A2A callback
                          (puthash "peer-task-2" (lambda (res) (setq peer-received res)) macher-agent--pending-callbacks)
                          (with-current-buffer child-buf
                            (macher-agent--push-routing "peer-task-2" (buffer-name peer-buf)))

                          ;; 3. Child completes peer request -> submit_task_result pops peer frame
                          (with-current-buffer child-buf
                            (funcall submit-fn (lambda (_) nil) :final_answer "Peer answer"))

                          (expect peer-received :not :to-be nil)
                          (expect (macher-agent-transit-payload-type peer-received) :to-equal 'ARTIFACT_UPDATE)
                          (expect (plist-get (macher-agent-transit-payload-payload peer-received) :message) :to-equal "Peer answer")
                          (expect root-received :to-be nil)

                          ;; 4. Child completes root task -> submit_task_result pops root frame
                          (with-current-buffer child-buf
                            (funcall submit-fn (lambda (_) nil) :final_answer "Root final answer"))

                          (expect root-received :not :to-be nil)
                          (expect (macher-agent-transit-payload-type root-received) :to-equal 'ARTIFACT_UPDATE)
                          (expect (plist-get (macher-agent-transit-payload-payload root-received) :message) :to-equal "Root final answer")

                          (kill-buffer root-buf)
                          (kill-buffer child-buf)
                          (kill-buffer peer-buf)))))

(provide 'macher-agent-message-test)
;;; macher-agent-message-test.el ends here
