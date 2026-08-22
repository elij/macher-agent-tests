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
                    
                    (it "inits core subagent directive when buffer is a subagent"
                        (let* ((orig-buf (generate-new-buffer "test-subagent-buf"))
                               (state (make-macher-agent-transmission-state :target-buffer orig-buf)))
                          (with-current-buffer orig-buf
                            (setq-local macher-agent--is-subagent t))
                          (setq state (macher-agent-pipe--init-core-directives state orig-buf nil nil nil))
                          (expect (length (macher-agent-transmission-state-directives state)) :to-equal 1)
                          (expect (car (macher-agent-transmission-state-directives state)) :to-match "CRITICAL DIRECTIVE:")
                          (kill-buffer orig-buf)))

                    (it "appends boot directive on initial request when no gptel response property exists"
                        (let* ((orig-buf (generate-new-buffer "test-initial-request-boot-buf"))
                               (state (make-macher-agent-transmission-state :target-buffer orig-buf)))
                          (with-current-buffer orig-buf
                            (setq-local macher-agent--boot-directive "Execute boot setup now."))
                          (setq state (macher-agent-pipe--append-boot-directive state orig-buf nil nil nil))
                          (expect (length (macher-agent-transmission-state-directives state)) :to-equal 1)
                          (expect (car (macher-agent-transmission-state-directives state)) :to-equal "Execute boot setup now.")
                          (kill-buffer orig-buf)))

                    (it "does not append boot directive on subsequent request when gptel response property exists"
                        (let* ((orig-buf (generate-new-buffer "test-subsequent-request-boot-buf"))
                               (state (make-macher-agent-transmission-state :target-buffer orig-buf)))
                          (with-current-buffer orig-buf
                            (setq-local macher-agent--boot-directive "Execute boot setup now.")
                            (insert "Previous assistant response")
                            (put-text-property (point-min) (point-max) 'gptel 'response))
                          (setq state (macher-agent-pipe--append-boot-directive state orig-buf nil nil nil))
                          (expect (macher-agent-transmission-state-directives state) :to-be nil)
                          (kill-buffer orig-buf)))

                    (it "drains thought queue and compiles directives into system prompt"
                        (let* ((orig-buf (generate-new-buffer "test-thought-queue-buf"))
                               (state (make-macher-agent-transmission-state :base-prompt "Base System Prompt"
                                                                            :target-buffer orig-buf)))
                          (with-current-buffer orig-buf
                            (macher-agent-add-pending-instruction "Thought 1"))
                          (setq state (macher-agent-pipe--drain-thought-queue state orig-buf nil nil nil))
                          (expect (length (macher-agent-transmission-state-directives state)) :to-equal 1)
                          (with-current-buffer orig-buf
                            (expect macher-agent--pending-instructions-queue :not :to-be nil))
                          (setq state (macher-agent-pipe--compile-directives state orig-buf nil nil nil))
                          (expect (macher-agent-transmission-state-compiled-prompt state) :to-match "Base System Prompt\n\nUSER OVERRIDE DIRECTIVE:\nThought 1")
                          (kill-buffer orig-buf)))

                    (it "appends ptc directive when ptc-primitives are active on state"
                        (let* ((orig-buf (generate-new-buffer "test-ptc-directive-buf"))
                               (state (make-macher-agent-transmission-state
                                       :target-buffer orig-buf
                                       :ptc-primitives '(spawn-subagent)
                                       :tools (list (gptel-make-tool :name "spawn-subagent"
                                                                     :description "Spawn subagent"
                                                                     :args '((:name "path" :type "string")))))))
                          (setq state (macher-agent-sandbox-append-ptc-directive state orig-buf nil nil nil))
                          (expect (length (macher-agent-transmission-state-directives state)) :to-equal 1)
                          (expect (car (macher-agent-transmission-state-directives state))
                                  :to-match "=== PROGRAMMATIC TOOL CALLING (PTC) ===")
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
