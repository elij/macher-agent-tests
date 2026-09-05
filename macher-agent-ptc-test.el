;;; macher-agent-ptc-test.el --- Programmatic Tool Calling (PTC) Tests -*- lexical-binding: t; -*-

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

(describe "Programmatic Tool Calling (PTC)"
          (macher-agent-test-setup-before-each)

          (it "handles generator yield and normalizes tool call interrupts"
              (let* ((macher-agent--active-ptc-primitives '(spawn-subagent)))
                ;; Direct eval-iter interrupt
                (let* ((iter (macher-agent-sandbox--eval-iter '(spawn-subagent "test" nil) nil))
                       (yield-val (iter-next iter)))
                  (expect (macher-agent-tool-call-p yield-val) :to-be t)
                  (expect (macher-agent-tool-call-name yield-val) :to-equal 'spawn-subagent))
                ;; Indirect funcall / apply invocation
                (let* ((iter (macher-agent-sandbox--funcall-iter 'spawn-subagent '("agent-alpha")))
                       (yield (iter-next iter)))
                  (expect (macher-agent-tool-call-p yield) :to-be t)
                  (expect (macher-agent-tool-call-name yield) :to-equal 'spawn-subagent)
                  (expect (macher-agent-tool-call-args yield) :to-equal '("agent-alpha")))
                ;; Alist arguments converted to plist
                (let* ((iter1 (macher-agent-sandbox--funcall-iter 'spawn-subagent '((path . "/tmp") (name . "test"))))
                       (yield1 (iter-next iter1))
                       (iter2 (macher-agent-sandbox--funcall-iter 'spawn-subagent '(((path . "/tmp") (name . "test")))))
                       (yield2 (iter-next iter2)))
                  (expect (macher-agent-tool-call-args yield1) :to-equal '(:path "/tmp" :name "test"))
                  (expect (macher-agent-tool-call-args yield2) :to-equal '(:path "/tmp" :name "test")))))

          (it "injects PTC instructions into prompts and transmission state"
              (let* ((tool (gptel-make-tool
                            :name "spawn-subagent"
                            :description "Spawn subagent"
                            :args '((:name "path" :type "string"))))
                     (gptel-tools (list tool))
                     (macher-agent--active-ptc-primitives '(spawn-subagent))
                     (base-prompt "Initial system prompt.")
                     (injected (macher-agent--inject-ptc-prompt base-prompt)))
                (expect injected :to-match "^Initial system prompt\\.")
                (expect injected :to-match "=== PROGRAMMATIC TOOL CALLING (PTC) ===")
                (expect injected :to-match "spawn-subagent -> (defun spawn-subagent (path)")
                (let ((macher-agent--active-ptc-primitives '(unmatched-primitive)))
                  (expect (macher-agent--inject-ptc-prompt base-prompt) :to-equal base-prompt))

                (let* ((orig-buf (generate-new-buffer "test-ptc-append-directive-buf"))
                       (state (make-macher-agent-transmission-state
                               :target-buffer orig-buf
                               :ptc-primitives '(spawn-subagent)
                               :tools (list tool))))
                  (unwind-protect
                      (progn
                        (setq state (macher-agent-sandbox-append-ptc-directive state))
                        (expect (length (macher-agent-transmission-state-directives state)) :to-equal 1)
                        (expect (car (macher-agent-transmission-state-directives state))
                                :to-match "=== PROGRAMMATIC TOOL CALLING (PTC) ==="))
                    (kill-buffer orig-buf)))))

          (describe "Plugin State and Lifecycle"
                    (it "manages sandbox state on context structs"
                        (let ((ctx (make-macher-agent-context :id "ctx-ptc-struct" :plugins '(:existing-key "value"))))
                          (expect (macher-agent-sandbox-get-state ctx) :to-be nil)
                          (macher-agent-sandbox-set-state ctx '(:active-primitives (spawn-subagent) :eval-mode safe))
                          (expect (macher-agent-sandbox-get-state ctx) :to-equal '(:active-primitives (spawn-subagent) :eval-mode safe))
                          (expect (plist-get (macher-agent-context-plugins ctx) :sandbox) :to-equal '(:active-primitives (spawn-subagent) :eval-mode safe))
                          (expect (plist-get (macher-agent-context-plugins ctx) :existing-key) :to-equal "value")
                          ;; Overwrite existing sandbox state
                          (macher-agent-sandbox-set-state ctx '(:active-primitives (spawn-subagent test-tool) :eval-mode isolated))
                          (expect (macher-agent-sandbox-get-state ctx) :to-equal '(:active-primitives (spawn-subagent test-tool) :eval-mode isolated))
                          (expect (plist-get (macher-agent-context-plugins ctx) :sandbox) :to-equal '(:active-primitives (spawn-subagent test-tool) :eval-mode isolated))))

                    (it "returns nil when getting or setting state on non-context-struct objects"
                        (expect (macher-agent-sandbox-get-state nil) :to-throw 'wrong-type-argument)
                        (expect (macher-agent-sandbox-get-state "invalid-string") :to-throw 'wrong-type-argument)
                        (expect (macher-agent-sandbox-get-state '(:sandbox (:active-primitives (spawn-subagent)))) :to-throw 'wrong-type-argument)
                        (expect (macher-agent-sandbox-set-state nil '(:active-primitives (spawn-subagent))) :to-throw 'wrong-type-argument)
                        (expect (macher-agent-sandbox-set-state "invalid-string" '(:active-primitives (spawn-subagent))) :to-throw 'wrong-type-argument)
                        (expect (macher-agent-sandbox-set-state '(:plugins nil) '(:active-primitives (spawn-subagent))) :to-throw 'wrong-type-argument))

                    (it "handles unexpected yielded values with 1-argument stop-iter-fn invocation"
                        (let* ((stopped-reason nil)
                               (error-received nil)
                               (stop-fn (lambda (reason) (setq stopped-reason reason)))
                               (error-fn (lambda (err) (setq error-received err))))
                          (macher-agent--ptc-handle-yielded-value
                           'unrecognized-yield-val
                           (make-macher-agent-context)
                           (current-buffer)
                           #'ignore
                           error-fn
                           stop-fn)
                          (expect stopped-reason :to-match "Unexpected yield from PTC sandbox")
                          (expect (plist-get error-received :status) :to-equal 'error)
                          (expect (plist-get error-received :error) :to-match "Unexpected yield from PTC sandbox")))

                    (it "registers and unregisters pipeline steps via install and uninstall"
                        (let ((saved-registry (copy-hash-table macher-agent-pipeline-registry)))
                          (unwind-protect
                              (progn
                                (clrhash macher-agent-pipeline-registry)
                                (macher-agent-sandbox-install)
                                (expect (member #'macher-agent-ptc--inject-tool
                                                (macher-agent-get-pipeline-steps 'preset-composition))
                                        :to-be-truthy)
                                (expect (member #'macher-agent-sandbox-append-ptc-to-transmission
                                                (macher-agent-get-pipeline-steps 'transmission))
                                        :to-be-truthy)
                                (when (fboundp 'macher-agent-sandbox-uninstall)
                                  (macher-agent-sandbox-uninstall)
                                  (expect (member #'macher-agent-ptc--inject-tool
                                                  (macher-agent-get-pipeline-steps 'preset-composition))
                                          :to-be nil)
                                  (expect (member #'macher-agent-sandbox-append-ptc-to-transmission
                                                  (macher-agent-get-pipeline-steps 'transmission))
                                          :to-be nil)))
                            (setq macher-agent-pipeline-registry saved-registry))))))

(provide 'macher-agent-ptc-test)
;;; macher-agent-ptc-test.el ends here
