;;; tests/macher-agent-gptel-test.el --- Tests for Macher Agent gptel Boundary -*- lexical-binding: t; -*-

(let* ((file (or load-file-name buffer-file-name))
       (this-dir (if file (file-name-directory (expand-file-name file)) (expand-file-name default-directory)))
       (root-dir (or (locate-dominating-file this-dir "macher-agent.el")
                     (locate-dominating-file default-directory "macher-agent.el")
                     (locate-dominating-file default-directory "tests")
                     default-directory))
       (test-dir (cond
                  ((file-exists-p (expand-file-name "macher-agent-test-setup.el" this-dir))
                   this-dir)
                  ((file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" root-dir))
                   (expand-file-name "tests" root-dir))
                  ((file-exists-p (expand-file-name "macher-agent-test-setup.el" default-directory))
                   (expand-file-name default-directory))
                  ((file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" default-directory))
                   (expand-file-name "tests" default-directory))
                  (t (or (locate-dominating-file default-directory "tests") (expand-file-name "tests" root-dir))))))
  (when root-dir
    (add-to-list 'load-path (file-name-as-directory (expand-file-name root-dir))))
  (add-to-list 'load-path (expand-file-name "tests" default-directory))
  (add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name) default-directory)))
  (when test-dir
    (add-to-list 'load-path (file-name-as-directory (expand-file-name test-dir)))
    (add-to-list 'load-path (file-name-as-directory (expand-file-name "helpers" test-dir)))))

(require 'macher-agent-test-setup)
(require 'macher-agent-core)
(require 'macher-agent-gptel)
(require 'macher-agent-presets)
(require 'macher-agent-tools)
(require 'macher-agent-vfs)

(describe "Macher Agent gptel Boundary Suite"
  (macher-agent-test-setup-before-each)

  (describe "1. FSM Hijack Transform and Lifecycle Callbacks"
    (it "injects origin-buffer, context, and handlers into FSM"
      (let* ((target-buf (generate-new-buffer "*test-target-ctx*"))
             (ctx (make-macher-agent-context :id "ctx-fsm-1" :project-root "/tmp/test-proj"))
             (fsm (gptel-make-fsm :info (list :buffer target-buf))))
        (unwind-protect
            (progn
              (with-current-buffer target-buf
                (setq-local macher-agent--persistent-context ctx)
                (insert "Agent prompt query"))
              (macher-agent--fsm-hijack-transform nil fsm)
              (let ((info (gptel-fsm-info fsm))
                    (handlers (gptel-fsm-handlers fsm)))
                (expect (plist-get info :origin-buffer) :to-equal target-buf)
                (expect (plist-get info :macher-agent-context) :to-equal ctx)
                (expect (plist-get info :prompt) :to-equal "Agent prompt query")
                (expect (memq #'macher-agent--inject-media-fsm-logic (alist-get 'WAIT handlers)) :to-be-truthy)
                (expect (memq #'macher-agent-gptel--trigger-flush (alist-get 'DONE handlers)) :to-be-truthy)))
          (when (buffer-live-p target-buf)
            (kill-buffer target-buf))))))

  (describe "2. Prompt Transformation and Inline Skill Extraction"
    (it "extracts and strips inline @skill tags matching known presets"
      (with-temp-buffer
        (let* ((orig-buf (generate-new-buffer "*test-orig*")))
          (unwind-protect
              (progn
                (with-current-buffer orig-buf
                  (setq-local gptel--known-presets '((coder :description "Coder preset")
                                                     (tester :description "Tester preset"))))
                (insert "Please run @coder and check the code.")
                (let ((res (macher-agent--extract-inline-skills (point-min) orig-buf)))
                  (expect (car res) :to-equal '(coder))
                  (expect (cdr res) :to-be t)
                  (expect (buffer-string) :to-equal "Please run and check the code.")))
            (when (buffer-live-p orig-buf)
              (kill-buffer orig-buf))))))

    (it "resolves transmission skills handling exclusive presets"
      (let* ((known '((exclusive-preset :exclusive t)
                      (normal-preset-1 :exclusive nil)
                      (normal-preset-2 :exclusive nil))))
        ;; Non-exclusive presets combine normally
        (expect (macher-agent--transformer-resolve-skills
                 '(normal-preset-1) '(normal-preset-2) known)
                :to-equal '(normal-preset-1 normal-preset-2))
        ;; Exclusive preset supersedes other presets
        (expect (macher-agent--transformer-resolve-skills
                 '(normal-preset-1) '(exclusive-preset normal-preset-2) known)
                :to-equal '(exclusive-preset))))

    (it "detects redirect when inline preset is used with no remaining prompt text"
      (with-temp-buffer
        (insert "   \t\n  ")
        (expect (macher-agent--transformer-detect-redirect t (point-min) '(coder)) :to-equal 'coder)
        (expect (macher-agent--transformer-detect-redirect nil (point-min) '(coder)) :to-be nil))
      (with-temp-buffer
        (insert "some actual task instruction")
        (expect (macher-agent--transformer-detect-redirect t (point-min) '(coder)) :to-be nil))))

  (describe "3. Tool Deduplication Transformer"
    (it "omits duplicate tool call blocks exceeding max duplicate limit"
      (with-temp-buffer
        (let ((macher-agent-max-duplicate-tools 1))
          (insert "```tool read_file\n{\"path\": \"foo.txt\"}\n```\n")
          (insert "Some text\n")
          (insert "```tool read_file\n{\"path\": \"foo.txt\"}\n```\n")
          (put-text-property (point-min) (point-max) 'gptel t)
          (macher-agent-transformer-deduplicate-tools nil nil)
          (expect (buffer-string) :to-match "{\"status\": \"omitted\", \"reason\": \"duplicate\"}")))))

  (describe "4. Transmission Pipeline"
    (it "installs and processes transmission pipeline steps"
      (macher-agent-transmission-install)
      (with-temp-buffer
        (let* ((buf (current-buffer))
               (mock-tool (gptel-make-tool :name "submit_task_result" :description "Finish task" :function #'ignore))
               (ctx (make-macher-agent-context :id "pipe-ctx" :project-root "/tmp/pipe-test")))
          (setq-local macher-agent--persistent-context ctx)
          (setq-local gptel-model 'mock-model)
          (setq-local gptel-system-prompt "Base system prompt.")
          (setq-local gptel-tools (list mock-tool))
          (setq-local macher-agent--boot-directive "INITIAL BOOT DIRECTIVE")
          (setq-local macher-agent--pending-instructions-queue '("PENDING 1" "PENDING 2"))
          (let ((state (macher-agent--compile-transmission-payload buf nil nil nil ctx)))
            (expect (macher-agent-transmission-state-model state) :to-equal 'mock-model)
            (expect (macher-agent-transmission-state-compiled-prompt state) :to-match "Base system prompt.")
            (expect (macher-agent-transmission-state-compiled-prompt state) :to-match "CRITICAL DIRECTIVE: You MUST use the `submit_task_result' tool")
            (expect (macher-agent-transmission-state-compiled-prompt state) :to-match "INITIAL BOOT DIRECTIVE")
            (expect (macher-agent-transmission-state-compiled-prompt state) :to-match "PENDING 1")
            (expect (macher-agent-transmission-state-compiled-prompt state) :to-match "PENDING 2"))))))

  (describe "5. Tool Scope Enforcement and Pre-Tool Logging"
    (it "blocks execution of unauthorized tools in active FSM scope"
      (let* ((allowed-tool (gptel-make-tool :name "allowed_tool" :description "allowed" :function #'ignore))
             (fsm (gptel-make-fsm :info (list :tools (list allowed-tool)))))
        (expect (macher-agent--enforce-tool-scope allowed-tool fsm) :to-be nil)
        (expect (macher-agent--enforce-tool-scope "disallowed_tool" fsm)
                :to-equal '(:block "ERROR: Tool 'disallowed_tool' is not accessible in this context or is no longer available. Please select another tool or approach.")))))

  (describe "7. Context Clearing and FSM Resolution"
    (it "clears persistent context and resets to physical baseline"
      (with-temp-buffer
        (let* ((ctx (make-macher-agent-context :id "ctx-to-clear" :project-root "/tmp/clean-test")))
          (setq-local macher-agent--persistent-context ctx)
          (macher-agent-clear-context)
          (expect (macher-agent-valid-context-p macher-agent--persistent-context) :to-be t)
          (expect (macher-agent-context-id macher-agent--persistent-context) :not :to-equal "ctx-to-clear"))))

    (it "extracts target buffer and context safely from FSM"
      (let* ((buf (generate-new-buffer "*test-fsm-extract*"))
             (ctx (make-macher-agent-context :id "ctx-fsm-ext" :project-root "/tmp/proj"))
             (fsm (gptel-make-fsm :info (list :buffer buf :macher-agent-context ctx))))
        (unwind-protect
            (progn
              (expect (macher-agent-gptel--fsm-target-buffer fsm) :to-equal buf)
              (expect (macher-agent-gptel-context-from-fsm fsm) :to-equal ctx))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

(provide 'tests/macher-agent-gptel-test)
(provide 'macher-agent-gptel-test)
;;; macher-agent-gptel-test.el ends here
