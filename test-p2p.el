;;; test-p2p.el --- Peer-to-peer messaging and wakeup synchronisation -*- lexical-binding: t; -*-

;;; Commentary:
;; Specification 7: Peer-to-peer messaging and wakeup synchronisation
;; P2P-01, P2P-02, P2P-03, P2P-04, P2P-05

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-test-helpers)

(describe "7. Peer-to-peer messaging and wakeup synchronisation"

  (before-each
    (macher-agent-test-reset-env))

  ;; --------------------------------------------------------------------------
  ;; P2P-01
  ;; --------------------------------------------------------------------------
  (it "P2P-01: wait-before-send invokes wake callback without overwriting routing stack or ephemeral state"
    (macher-agent-test-with-git-workspace (root orch-ctx orch-buf)
      (let* ((rec-buf (macher-agent-add-subagent "*p2p-01-receiver*" nil orch-buf nil orch-ctx))
             (rec-ctx (with-current-buffer rec-buf macher-agent--persistent-context))
             (sender-buf (macher-agent-add-subagent "*p2p-01-sender*" nil orch-buf nil orch-ctx))
             (sender-ctx (with-current-buffer sender-buf macher-agent--persistent-context))
             (orch-final-result nil)
             (task-id "orch-to-rec-task-01")
             (wait-fn (get 'macher-agent-wait-for-message-tool 'ptc-function))
             (send-fn (get 'macher-agent-send-message-tool 'ptc-function))
             (submit-fn (get 'macher-agent-submit-task-result-tool 'ptc-function))
             (wake-received nil))

        (unwind-protect
            (progn
              ;; 1. Orchestrator delegates task to receiver
              (puthash task-id (lambda (res) (setq orch-final-result res)) macher-agent--pending-callbacks)
              (with-current-buffer rec-buf
                (setq-local macher-agent--is-ephemeral t)
                (macher-agent--push-routing task-id (buffer-name orch-buf) t))

              (let ((saved-stack (with-current-buffer rec-buf macher-agent--routing-stack)))
                ;; 2. Receiver calls wait_for_message and suspends
                (with-current-buffer rec-buf
                  (let ((thunk (funcall wait-fn nil rec-ctx)))
                    (funcall thunk (lambda (msg) (setq wake-received msg)))))

                ;; Verify wake callback is registered
                (expect (macher-agent-get-wake-callback (buffer-name rec-buf)) :not :to-be nil)

                ;; 3. Sender calls send_message targeting receiver
                (with-current-buffer sender-buf
                  (funcall send-fn (buffer-name rec-buf) "Hello Receiver from Sender" sender-ctx))

                ;; Receiver wakes
                (expect wake-received :to-match "Hello Receiver from Sender")

                ;; send_message does NOT overwrite receiver's routing stack or ephemeral state
                (with-current-buffer rec-buf
                  (expect macher-agent--routing-stack :to-equal saved-stack)
                  (expect macher-agent--is-ephemeral :to-be t)
                  ;; 4. Receiver calls submit_task_result
                  (funcall submit-fn "Receiver finished after wake" nil rec-ctx))

                ;; submit_task_result delivers final answer back to originating orchestrator
                (expect orch-final-result :not :to-be nil)
                (let ((payload (plist-get (macher-agent-transit-payload-payload orch-final-result) :payload)))
                  (expect payload :to-equal "Receiver finished after wake"))))
          (when (buffer-live-p rec-buf) (kill-buffer rec-buf))
          (when (buffer-live-p sender-buf) (kill-buffer sender-buf))))))

  ;; --------------------------------------------------------------------------
  ;; P2P-02
  ;; --------------------------------------------------------------------------
  (it "P2P-02: send-before-wait appends to FIFO message queue without reset, then wait pops and resolves immediately"
    (macher-agent-test-with-git-workspace (root orch-ctx orch-buf)
      (let* ((rec-buf (macher-agent-add-subagent "*p2p-02-receiver*" nil orch-buf nil orch-ctx))
             (rec-ctx (with-current-buffer rec-buf macher-agent--persistent-context))
             (sender-buf (macher-agent-add-subagent "*p2p-02-sender*" nil orch-buf nil orch-ctx))
             (sender-ctx (with-current-buffer sender-buf macher-agent--persistent-context))
             (task-id "orch-to-rec-task-02")
             (orch-final-result nil)
             (send-fn (get 'macher-agent-send-message-tool 'ptc-function))
             (wait-fn (get 'macher-agent-wait-for-message-tool 'ptc-function))
             (submit-fn (get 'macher-agent-submit-task-result-tool 'ptc-function))
             (resolved-msg nil)
             (gptel-send-called nil))

        (unwind-protect
            (progn
              ;; 1. Orchestrator delegates task to receiver
              (puthash task-id (lambda (res) (setq orch-final-result res)) macher-agent--pending-callbacks)
              (with-current-buffer rec-buf
                (setq-local macher-agent--is-ephemeral t)
                (macher-agent--push-routing task-id (buffer-name orch-buf) t))

              ;; 2. Before receiver calls wait_for_message, sender calls send_message targeting receiver
              (cl-letf (((symbol-function 'gptel-send) (lambda () (setq gptel-send-called t))))
                (with-current-buffer sender-buf
                  (funcall send-fn (buffer-name rec-buf) "Early message payload" sender-ctx)))

              ;; Message appended to receiver's FIFO message-queue without calling gptel-send
              (expect gptel-send-called :to-be nil)
              (with-current-buffer rec-buf
                (expect (length macher-agent--message-queue) :to-equal 1)
                (expect (car macher-agent--message-queue) :to-equal "Early message payload")

                ;; 3. Receiver calls wait_for_message: pops queued message and resolves immediately
                (let ((thunk (funcall wait-fn nil rec-ctx)))
                  (funcall thunk (lambda (msg) (setq resolved-msg msg))))
                (expect resolved-msg :to-match "Early message payload")
                (expect macher-agent--message-queue :to-be nil)

                ;; Receiver calls submit_task_result
                (funcall submit-fn "Final answer after queued message" nil rec-ctx))

              (expect orch-final-result :not :to-be nil)
              (let ((payload (plist-get (macher-agent-transit-payload-payload orch-final-result) :payload)))
                (expect payload :to-equal "Final answer after queued message")))
          (when (buffer-live-p rec-buf) (kill-buffer rec-buf))
          (when (buffer-live-p sender-buf) (kill-buffer sender-buf))))))

  ;; --------------------------------------------------------------------------
  ;; P2P-03
  ;; --------------------------------------------------------------------------
  (it "P2P-03: handles multiple messages from different senders in strict FIFO order"
    (let* ((rec-buf (get-buffer-create "*p2p-03-receiver*"))
           (sender1-buf (get-buffer-create "*p2p-03-sender1*"))
           (sender2-buf (get-buffer-create "*p2p-03-sender2*"))
           (send-fn (get 'macher-agent-send-message-tool 'ptc-function))
           (wait-fn (get 'macher-agent-wait-for-message-tool 'ptc-function))
           (msg1-res nil)
           (msg2-res nil))
      (unwind-protect
          (progn
            ;; Sender 1 sends first message
            (with-current-buffer sender1-buf
              (funcall send-fn (buffer-name rec-buf) "FIRST_MESSAGE"))
            ;; Sender 2 sends second message
            (with-current-buffer sender2-buf
              (funcall send-fn (buffer-name rec-buf) "SECOND_MESSAGE"))

            (with-current-buffer rec-buf
              (expect (length macher-agent--message-queue) :to-equal 2)
              ;; First call to wait_for_message pops FIRST_MESSAGE
              (let ((thunk1 (funcall wait-fn)))
                (funcall thunk1 (lambda (res) (setq msg1-res res))))
              (expect msg1-res :to-match "FIRST_MESSAGE")
              (expect (length macher-agent--message-queue) :to-equal 1)

              ;; Second call to wait_for_message pops SECOND_MESSAGE
              (let ((thunk2 (funcall wait-fn)))
                (funcall thunk2 (lambda (res) (setq msg2-res res))))
              (expect msg2-res :to-match "SECOND_MESSAGE")
              (expect macher-agent--message-queue :to-be nil)))
        (when (buffer-live-p rec-buf) (kill-buffer rec-buf))
        (when (buffer-live-p sender1-buf) (kill-buffer sender1-buf))
        (when (buffer-live-p sender2-buf) (kill-buffer sender2-buf)))))

  ;; --------------------------------------------------------------------------
  ;; P2P-04
  ;; --------------------------------------------------------------------------
  (it "P2P-04: send_message resolves immediately upon dispatch without blocking on receiver"
    (let* ((rec-buf (get-buffer-create "*p2p-04-receiver*"))
           (sender-buf (get-buffer-create "*p2p-04-sender*"))
           (send-fn (get 'macher-agent-send-message-tool 'ptc-function))
           (resolved-val nil))
      (unwind-protect
          (with-current-buffer sender-buf
            ;; Calling send_message returns completion string immediately
            (setq resolved-val (funcall send-fn (buffer-name rec-buf) "Non-blocking dispatch"))
            (expect resolved-val :to-match "SYSTEM: Message successfully dispatched"))
        (when (buffer-live-p rec-buf) (kill-buffer rec-buf))
        (when (buffer-live-p sender-buf) (kill-buffer sender-buf)))))

  ;; --------------------------------------------------------------------------
  ;; P2P-05
  ;; --------------------------------------------------------------------------
  (it "P2P-05: syncs sender baseline VFS state to receiver context on peer-to-peer dispatch"
    (macher-agent-test-with-git-workspace (root orch-ctx orch-buf)
      (let* ((rec-buf (macher-agent-add-subagent "*p2p-05-receiver*" nil orch-buf nil orch-ctx))
             (sender-buf (macher-agent-add-subagent "*p2p-05-sender*" nil orch-buf nil orch-ctx))
             (sender-ctx (with-current-buffer sender-buf macher-agent--persistent-context))
             (rec-ctx (with-current-buffer rec-buf macher-agent--persistent-context))
             (send-fn (get 'macher-agent-send-message-tool 'ptc-function)))
        (unwind-protect
            (progn
              ;; Create a file entry in sender VFS
              (macher-agent--update-context-file sender-ctx "shared.txt" "Shared baseline content")
              ;; Send message from sender to receiver
              (with-current-buffer sender-buf
                (funcall send-fn (buffer-name rec-buf) "Check shared file" sender-ctx))
              ;; Receiver context now has shared.txt with :pending-patch nil
              (let* ((rec-contents (macher-agent--get-context-contents rec-ctx))
                     (entry (or (cl-find "shared.txt" rec-contents
                                         :key #'macher-agent-vfs-entry-path :test #'equal)
                                (cl-find (expand-file-name "shared.txt" root) rec-contents
                                         :key #'macher-agent-vfs-entry-path :test #'equal)
                                (cl-find (expand-file-name "shared.txt" root) rec-contents
                                         :key #'macher-agent-vfs-entry-path :test #'file-equal-p))))
                (expect entry :not :to-be nil)
                (expect (macher-agent-vfs-entry-curr entry) :to-equal "Shared baseline content")
                (expect (macher-agent-vfs-entry-pending-patch entry) :to-be nil)))
          (when (buffer-live-p rec-buf) (kill-buffer rec-buf))
          (when (buffer-live-p sender-buf) (kill-buffer sender-buf)))))))

(provide 'test-p2p)
;;; test-p2p.el ends here
