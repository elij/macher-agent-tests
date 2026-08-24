;;; macher-agent-transmission-pipeline-test.el --- Transmission Pipeline & Formatting Tests -*- lexical-binding: t; -*-

(let* ((file (or load-file-name buffer-file-name))
       (test-dir (cond
                  (file (file-name-directory file))
                  ((file-exists-p (expand-file-name "macher-agent-test-setup.el" default-directory))
                   default-directory)
                  ((file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" default-directory))
                   (expand-file-name "tests" default-directory))
                  (t default-directory))))
  (add-to-list 'load-path test-dir)
  (add-to-list 'load-path (expand-file-name "helpers" test-dir)))

(require 'macher-agent-test-setup)

(describe "Transmission Pipeline and Formatting"
          (macher-agent-test-setup-before-each)

          (it "truncates context history using model-specific alist limits"
              (with-temp-buffer
                (insert "Early user prompt content\n")
                (let ((resp "Previous response boundary\n"))
                  (put-text-property 0 (length resp) 'gptel 'response resp)
                  (insert resp))
                (insert "Latest user query content")
                (let ((macher-agent-max-context-chars '((gpt-4o . 25) (nil . 2000000)))
                      (gptel-model 'gpt-4o))
                  (macher-agent-memory-pipe--truncate-buffer nil (current-buffer) nil nil nil))
                (expect (buffer-string) :to-match "Latest user query content")))

          (describe "Refactored Unified Transmission Reducer Pipeline"

                    (it "processes task submission and boot directives across turns in transmission state"
                        (let* ((init-buf (generate-new-buffer "test-init-boot-buf"))
                               (subseq-buf (generate-new-buffer "test-subseq-boot-buf"))
                               (state1 (make-macher-agent-transmission-state
                                        :target-buffer init-buf
                                        :tools (list (gptel-make-tool :name "submit_task_result"
                                                                      :description "Submit result"
                                                                      :args nil))))
                               (state2 (make-macher-agent-transmission-state :target-buffer subseq-buf)))
                          ;; 1. Core directive when submit_task_result is present
                          (with-current-buffer init-buf
                            (setq-local macher-agent--boot-directive "Execute boot setup now."))
                          (setq state1 (macher-agent-pipe--init-core-directives state1 init-buf nil nil nil))
                          (setq state1 (macher-agent-pipe--append-boot-directive state1 init-buf nil nil nil))
                          (expect (length (macher-agent-transmission-state-directives state1)) :to-equal 2)
                          (expect (car (macher-agent-transmission-state-directives state1)) :to-equal "Execute boot setup now.")
                          (expect (cadr (macher-agent-transmission-state-directives state1)) :to-match "CRITICAL DIRECTIVE:")

                          ;; 2. Subsequent request (has response property) skips boot directive
                          (with-current-buffer subseq-buf
                            (setq-local macher-agent--boot-directive "Execute boot setup now.")
                            (insert "Previous assistant response")
                            (put-text-property (point-min) (point-max) 'gptel 'response))
                          (setq state2 (macher-agent-pipe--append-boot-directive state2 subseq-buf nil nil nil))
                          (expect (macher-agent-transmission-state-directives state2) :to-be nil)

                          (kill-buffer init-buf)
                          (kill-buffer subseq-buf)))

                    (it "drains thought queue, appends PTC directives, and compiles system prompt"
                        (let* ((orig-buf (generate-new-buffer "test-thought-ptc-buf"))
                               (state (make-macher-agent-transmission-state
                                       :base-prompt "Base System Prompt"
                                       :target-buffer orig-buf
                                       :ptc-primitives '(spawn-subagent)
                                       :tools (list (gptel-make-tool :name "spawn-subagent"
                                                                     :description "Spawn subagent"
                                                                     :args '((:name "path" :type "string")))))))
                          (with-current-buffer orig-buf
                            (macher-agent-add-pending-instruction "Thought 1"))
                          (setq state (macher-agent-pipe--drain-thought-queue state orig-buf nil nil nil))
                          (setq state (macher-agent-sandbox-append-ptc-directive state orig-buf nil nil nil))
                          (setq state (macher-agent-pipe--compile-directives state orig-buf nil nil nil))
                          (let ((compiled (macher-agent-transmission-state-compiled-prompt state)))
                            (expect compiled :to-match "Base System Prompt")
                            (expect compiled :to-match "USER OVERRIDE DIRECTIVE:\nThought 1")
                            (expect compiled :to-match "=== PROGRAMMATIC TOOL CALLING (PTC) ==="))
                          (kill-buffer orig-buf))))

          (it "triggers flush hook on completion when FSM transitions to DONE"
              (let* ((buf (generate-new-buffer "*test-trigger-flush-hook*"))
                     (mock-ctx (macher--make-context :contents nil))
                     (fsm (gptel-make-fsm :info (list :buffer buf :macher-agent-context mock-ctx)
                                          :state 'DONE))
                     (flush-called nil)
                     (hook-fn (lambda (&rest _) (setq flush-called t))))
                (unwind-protect
                    (with-current-buffer buf
                      (setq-local macher-agent--persistent-context mock-ctx)
                      (setq-local macher-agent--pending-instructions-queue '("queue-item"))
                      (add-hook 'macher-agent-task-flush-hook hook-fn)
                      (macher-agent-gptel--trigger-flush fsm)
                      (expect flush-called :to-be t)
                      (expect macher-agent--pending-instructions-queue :to-be nil))
                  (remove-hook 'macher-agent-task-flush-hook hook-fn)
                  (when (buffer-live-p buf)
                    (kill-buffer buf))))))

(provide 'macher-agent-transmission-pipeline-test)
;;; macher-agent-transmission-pipeline-test.el ends here
