;;; macher-agent-gptel-test.el --- Tests for Macher Agent GPTel Boundary -*- lexical-binding: t; -*-

(let* ((file (or load-file-name buffer-file-name))
       (test-dir (cond
                  (file (file-name-directory (expand-file-name file)))
                  ((file-exists-p (expand-file-name "macher-agent-test-setup.el" default-directory))
                   default-directory)
                  ((file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" default-directory))
                   (expand-file-name "tests" default-directory))
                  (t default-directory)))
       (root-dir (locate-dominating-file (or file default-directory) "macher-agent.el")))
  (when root-dir
    (add-to-list 'load-path (expand-file-name root-dir))
    (add-to-list 'load-path (expand-file-name "macher" root-dir))
    (add-to-list 'load-path (expand-file-name "gptel" root-dir)))
  (add-to-list 'load-path test-dir)
  (add-to-list 'load-path (expand-file-name "helpers" test-dir)))

(require 'macher-agent-test-setup)
(require 'cl-lib)
(require 'macher nil t)
(unless (fboundp 'macher--make-context)
  (cl-defstruct (macher-context (:constructor macher--make-context))
    contents
    workspace
    prompt
    process-request-function
    data
    dirty-p
    shadow-buffers))
(require 'macher-agent-gptel)

(describe "Macher-Agent GPTel Boundary Suite"
  (macher-agent-test-setup-before-each)

  (describe "Specialised Context Accessors and Slots"
    (it "constructs and inspects macher-agent-task-context structure slots directly"
      (let* ((buf (generate-new-buffer "test-task-ctx-buf"))
             (tctx (make-macher-agent-task-context
                    :workspace (cons 'project "/mock/workspace/")
                    :target-buffer buf
                    :skill-sym 'expert-reviewer
                    :system-message "Specialised task system message")))
        (unwind-protect
            (progn
              (expect (macher-agent-task-context-p tctx) :to-be t)
              (expect (macher-agent-task-context-workspace tctx) :to-equal (cons 'project "/mock/workspace/"))
              (expect (macher-agent-task-context-target-buffer tctx) :to-be buf)
              (expect (macher-agent-task-context-skill-sym tctx) :to-equal 'expert-reviewer)
              (expect (macher-agent-task-context-system-message tctx) :to-equal "Specialised task system message")
              (let ((cloned (copy-macher-agent-task-context tctx)))
                (expect (macher-agent-task-context-p cloned) :to-be t)
                (expect (macher-agent-task-context-workspace cloned) :to-equal (cons 'project "/mock/workspace/"))
                (expect (macher-agent-task-context-target-buffer cloned) :to-be buf)
                (expect (macher-agent-task-context-skill-sym cloned) :to-equal 'expert-reviewer)
                (expect (macher-agent-task-context-system-message cloned) :to-equal "Specialised task system message")))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "accesses and mutates context prompt and workspace via specialised accessors"
      (let ((ctx (make-macher-agent-context :project-root "/mock/specialised/proj/")))
        (expect (macher-agent-context-p ctx) :to-be t)
        (expect (macher-agent-context-workspace ctx) :to-equal (cons 'project (expand-file-name "/mock/specialised/proj/")))
        (expect (macher-agent-context-prompt ctx) :to-be nil)
        (setf (macher-agent-context-prompt ctx) "Initial prompt test")
        (expect (macher-agent-context-prompt ctx) :to-equal "Initial prompt test")
        (set-macher-agent-context-prompt ctx "Updated prompt test")
        (expect (macher-agent-context-prompt ctx) :to-equal "Updated prompt test"))))

  (describe "Resting State Consolidation and Context Resolution"
    (it "resolves context through macher-agent--persistent-context when idle via macher-agent-gptel-context-from-fsm"
      (let* ((buf (generate-new-buffer "test-gptel-resting-buf"))
             (mock-ctx (make-macher-agent-context :project-root "/mock/resting/")))
        (unwind-protect
            (with-current-buffer buf
              (setq-local macher-agent--persistent-context mock-ctx)
              (setq-local macher-agent--active-fsm nil)

              ;; When idle, active FSM is nil
              (expect macher-agent--active-fsm :to-be nil)
              (expect (macher-agent-get-active-fsm) :to-be nil)

              ;; Resting state resolution
              (expect (macher-agent-gptel-context-from-fsm nil buf) :to-be mock-ctx)
              (expect (macher-agent-gptel-context-from-fsm nil) :to-be mock-ctx))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "resolves context directly from FSM info plist during active execution via macher-agent-gptel-context-from-fsm"
      (let* ((buf (generate-new-buffer "test-gptel-active-buf"))
             (mock-ctx (make-macher-agent-context :project-root "/mock/active/"))
             (fsm (gptel-make-fsm :info (list :buffer buf :macher-agent-context mock-ctx)))
             (fsm-empty (gptel-make-fsm :info nil)))
        (unwind-protect
            (progn
              ;; Buffer extraction
              (expect (macher-agent-gptel--fsm-target-buffer nil) :to-be nil)
              (expect (macher-agent-gptel--fsm-target-buffer fsm-empty) :to-be nil)
              (expect (macher-agent-gptel--fsm-target-buffer fsm) :to-be buf)

              ;; Direct extraction from FSM info plist
              (expect (macher-agent-gptel-context-from-fsm fsm) :to-be mock-ctx)
              (expect (macher-agent-gptel-context-from-fsm mock-ctx) :to-be mock-ctx)

              ;; Fallback to buffer-local persistent-context when FSM info lacks it
              (let ((fsm-no-ctx (gptel-make-fsm :info (list :buffer buf))))
                (with-current-buffer buf
                  (setq-local macher-agent--persistent-context mock-ctx))
                (expect (macher-agent-gptel-context-from-fsm fsm-no-ctx) :to-be mock-ctx)))
          (when (buffer-live-p buf)
            (kill-buffer buf))))))

  (describe "Active FSM Lifecycle and Buffer-Local State"
    (it "binds active FSM buffer-locally during hijack and injects context into FSM info"
      (let* ((buf (generate-new-buffer "test-gptel-hijack-buf"))
             (mock-ctx (make-macher-agent-context :project-root "/mock/hijack/"))
             (cb-called nil)
             (orig-cb (lambda (resp &rest _args) (setq cb-called resp)))
             (fsm (gptel-make-fsm :info (list :buffer buf :callback orig-cb))))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (setq-local macher-agent--persistent-context mock-ctx)
                (setq-local macher-agent--active-fsm nil)
                (insert "User prompt to be captured"))

              (macher-agent--fsm-hijack-transform #'ignore fsm)

              ;; FSM bound buffer-locally in target buffer
              (with-current-buffer buf
                (expect macher-agent--active-fsm :to-be fsm))

              ;; Context injected into FSM info plist
              (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be mock-ctx)
              (expect (plist-get (gptel-fsm-info fsm) :origin-buffer) :to-be buf)
              (expect (macher-agent-context-prompt mock-ctx) :to-equal "User prompt to be captured")

              ;; Handlers augmented
              (let ((handlers (gptel-fsm-handlers fsm)))
                (expect (memq #'macher-agent--inject-media-fsm-logic (alist-get 'WAIT handlers)) :to-be-truthy)
                (expect (memq #'macher-agent-gptel--trigger-flush (alist-get 'DONE handlers)) :to-be-truthy)
                (expect (memq #'macher-agent-gptel--trigger-flush (alist-get 'ABRT handlers)) :to-be-truthy)
                (expect (memq #'macher-agent-gptel--trigger-flush (alist-get 'ERRS handlers)) :to-be-truthy))

              ;; Protected callback invocation
              (let ((wrapped-cb (plist-get (gptel-fsm-info fsm) :callback)))
                (funcall wrapped-cb "test response")
                (expect cb-called :to-equal "test response")
                (funcall wrapped-cb nil (list :tool-use '((:name "test_tool"))))
                (expect cb-called :to-equal "")))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "prevents duplicate hook handler accumulation on repeated transformations"
      (let* ((buf (generate-new-buffer "test-duplicate-handlers-buf"))
             (mock-ctx (make-macher-agent-context :project-root "/mock/dup/"))
             (fsm (gptel-make-fsm :info (list :buffer buf))))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (setq-local macher-agent--persistent-context mock-ctx)
                (setq-local macher-agent--active-fsm nil)
                (insert "Prompt text for repeated transforms"))

              ;; Apply transform multiple times on the same FSM instance
              (macher-agent--fsm-hijack-transform #'ignore fsm)
              (macher-agent--fsm-hijack-transform #'ignore fsm)
              (macher-agent--fsm-hijack-transform #'ignore fsm)

              (let ((handlers (gptel-fsm-handlers fsm)))
                ;; Verify each handler is present exactly once without duplication
                (expect (cl-count #'macher-agent--inject-media-fsm-logic (alist-get 'WAIT handlers)) :to-equal 1)
                (expect (cl-count #'macher-agent-gptel--trigger-flush (alist-get 'DONE handlers)) :to-equal 1)
                (expect (cl-count #'macher-agent-gptel--trigger-flush (alist-get 'ABRT handlers)) :to-equal 1)
                (expect (cl-count #'macher-agent-gptel--trigger-flush (alist-get 'ERRS handlers)) :to-equal 1)))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "resets macher-agent--active-fsm to nil on terminal state flush"
      (let* ((buf (generate-new-buffer "test-flush-reset-buf"))
             (mock-ctx (make-macher-agent-context :project-root "/mock/flush/"))
             (fsm (gptel-make-fsm :info (list :buffer buf :macher-agent-context mock-ctx)
                                  :state 'DONE))
             (flush-called nil)
             (hook-fn (lambda (&rest _) (setq flush-called t))))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (setq-local macher-agent--persistent-context mock-ctx)
                (setq-local macher-agent--active-fsm fsm)
                (setq-local macher-agent--pending-instructions-queue '("queued-instruction")))
              (add-hook 'macher-agent-task-flush-hook hook-fn)

              (macher-agent-gptel--trigger-flush fsm)

              ;; Flush hook called, queue drained, and active FSM reset to nil
              (expect flush-called :to-be t)
              (with-current-buffer buf
                (expect macher-agent--active-fsm :to-be nil)
                (expect macher-agent--pending-instructions-queue :to-be nil)))
          (remove-hook 'macher-agent-task-flush-hook hook-fn)
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "resets macher-agent--active-fsm to nil on abort"
      (let ((buf (generate-new-buffer "test-abort-reset-buf")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (setq-local macher-agent--active-fsm 'mock-fsm))
              (spy-on 'gptel-abort)
              (macher-agent-bridge-abort buf)
              (with-current-buffer buf
                (expect macher-agent--active-fsm :to-be nil)))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "resets FSM context structure via macher-agent-bridge-reset-fsm-context"
      (let* ((buf (generate-new-buffer "test-bridge-reset-buf"))
             (old-ctx (make-macher-agent-context :project-root "/mock/old/"))
             (new-ctx (make-macher-agent-context :project-root "/mock/new/"))
             (fsm (gptel-make-fsm :info (list :buffer buf :macher-agent-context old-ctx))))
        (unwind-protect
            (with-current-buffer buf
              (setq-local macher-agent--active-fsm fsm)
              (macher-agent-bridge-reset-fsm-context new-ctx)
              (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be new-ctx))
          (when (buffer-live-p buf)
            (kill-buffer buf))))))

  (describe "Prompt Transformation and Inline Skill Parsing"
    (it "extracts and strips inline skill tags and resolves exclusive presets"
      (let* ((known '((elisp-expert . (:exclusive t :system "Elisp system"))
                      (helper . (:system "Helper system"))))
             (orig-buf (generate-new-buffer "test-inline-skills-buf")))
        (unwind-protect
            (with-current-buffer orig-buf
              (setq-local gptel--known-presets known)
              (insert "@elisp-expert @helper please analyze this code")
              (let* ((extraction (macher-agent--extract-inline-skills (point-min) orig-buf))
                     (matched (car extraction))
                     (used (cdr extraction)))
                (expect matched :to-equal '(elisp-expert helper))
                (expect used :to-be t)
                (expect (buffer-string) :to-equal "please analyze this code"))

              ;; Exclusive skill resolution
              (let ((resolved (macher-agent--transformer-resolve-skills '(helper) '(elisp-expert) known)))
                (expect resolved :to-equal '(elisp-expert))))
          (when (buffer-live-p orig-buf)
            (kill-buffer orig-buf)))))

    (it "detects skill command redirect when prompt contains only the inline skill tag"
      (let ((orig-buf (generate-new-buffer "test-redirect-buf")))
        (unwind-protect
            (with-current-buffer orig-buf
              (insert "   ")
              (expect (macher-agent--transformer-detect-redirect t (point-min) '(terminal)) :to-equal 'terminal)
              (erase-buffer)
              (insert "do some work")
              (expect (macher-agent--transformer-detect-redirect t (point-min) '(terminal)) :to-be nil))
          (when (buffer-live-p orig-buf)
            (kill-buffer orig-buf))))))

  (describe "Transmission Pipeline Reducer and Directives Compilation"
    (it "compiles base prompt, task submission directives, boot directives, and thought queue"
      (let* ((init-buf (generate-new-buffer "test-pipe-init-buf"))
             (subseq-buf (generate-new-buffer "test-pipe-subseq-buf"))
             (submit-tool (gptel-make-tool :name "submit_task_result"
                                           :description "Submit final answer"
                                           :args nil))
             (state (make-macher-agent-transmission-state
                     :base-prompt "Base System Prompt"
                     :target-buffer init-buf
                     :tools (list submit-tool))))
        (unwind-protect
            (progn
              (with-current-buffer init-buf
                (setq-local macher-agent--boot-directive "Boot setup directive.")
                (macher-agent-add-pending-instruction "Queued user thought"))

              ;; Pipeline step applications
              (setq state (macher-agent-pipe--init-core-directives state init-buf nil nil nil))
              (setq state (macher-agent-pipe--append-boot-directive state init-buf nil nil nil))
              (setq state (macher-agent-pipe--drain-thought-queue state init-buf nil nil nil))
              (setq state (macher-agent-pipe--compile-directives state init-buf nil nil nil))

              (let ((compiled (macher-agent-transmission-state-compiled-prompt state)))
                (expect compiled :to-match "Base System Prompt")
                (expect compiled :to-match "CRITICAL DIRECTIVE: You MUST use the `submit_task_result' tool")
                (expect compiled :to-match "Boot setup directive.")
                (expect compiled :to-match "Queued user thought"))

              ;; Subsequent turn skips boot directive when response property is present
              (with-current-buffer subseq-buf
                (setq-local macher-agent--boot-directive "Boot setup directive.")
                (insert "Previous response")
                (put-text-property (point-min) (point-max) 'gptel 'response))
              (let ((state2 (make-macher-agent-transmission-state :target-buffer subseq-buf)))
                (setq state2 (macher-agent-pipe--append-boot-directive state2 subseq-buf nil nil nil))
                (expect (macher-agent-transmission-state-directives state2) :to-be nil)))
          (when (buffer-live-p init-buf) (kill-buffer init-buf))
          (when (buffer-live-p subseq-buf) (kill-buffer subseq-buf))))))

  (describe "Tool Scope Enforcement and Deduplication"
    (it "enforces tool scope against active FSM authorized tools"
      (let* ((allowed-tool (gptel-make-tool :name "read_file" :description "Read file" :args nil))
             (fsm (gptel-make-fsm :info (list :tools (list allowed-tool)))))
        (expect (macher-agent--enforce-tool-scope "read_file" fsm) :to-be nil)
        (let ((block-res (macher-agent--enforce-tool-scope "unauthorized_tool" fsm)))
          (expect (plist-get block-res :block) :to-match "ERROR: Tool 'unauthorized_tool' is not accessible"))))

    (it "deduplicates excessive identical tool invocations in buffer context"
      (with-temp-buffer
        (let ((macher-agent-max-duplicate-tools 1))
          (insert "```tool read_file\n{\"path\": \"foo.el\"}\n```\n")
          (insert "```tool read_file\n{\"path\": \"foo.el\"}\n```\n")
          (put-text-property (point-min) (point-max) 'gptel 'response)
          (macher-agent-transformer-deduplicate-tools nil nil)
          (expect (buffer-string) :to-match "{\"status\": \"omitted\", \"reason\": \"duplicate\"}")))))

  (describe "Media Injection Lifecycle"
    (it "rejects invalid media formats and clears media queue upon injection"
      (let* ((mock-ctx (make-macher-agent-context :project-root "/mock/proj/"))
             (fsm (gptel-make-fsm :info (list :macher-agent-context mock-ctx))))
        (setf (macher-agent-context-media-queue mock-ctx) 12345)
        (expect (macher-agent--perform-pending-media-injection fsm)
                :to-throw 'error)

        (setf (macher-agent-context-media-queue mock-ctx) "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==")
        (macher-agent--perform-pending-media-injection fsm)
        (expect (macher-agent-context-media-queue mock-ctx) :to-be nil)

        ;; Media queue injection via plugins plist
        (setf (macher-agent-context-plugins mock-ctx)
              (list :pending-media "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="))
        (macher-agent--perform-pending-media-injection fsm)
        (expect (plist-get (macher-agent-context-plugins mock-ctx) :pending-media) :to-be nil))))

  (describe "Transmission Dispatch"
    (it "transmits request via macher-agent-gptel-transmit with system message and gptel-send"
      (let* ((buf (generate-new-buffer "test-transmit-buf"))
             (task-ctx (make-macher-agent-task-context
                        :target-buffer buf
                        :system-message "Transmitted System Message")))
        (unwind-protect
            (progn
              (macher-agent-gptel-transmit task-ctx nil)
              (expect 'gptel-send :to-have-been-called)
              (with-current-buffer buf
                (expect gptel-system-prompt :to-equal "Transmitted System Message")))
          (when (buffer-live-p buf)
            (kill-buffer buf))))))

  (describe "State Restoration and Session Tracking"
    (it "marks buffer as restored session when model is buffer-local"
      (let ((buf (generate-new-buffer "test-restore-buf")))
        (unwind-protect
            (with-current-buffer buf
              (setq-local gptel-model "mock-model")
              (macher-agent--restore-local-state)
              (expect macher-agent--is-restored-session :to-be t)
              ;; Setup buffer clears restored session flag and resets presets
              (setq-local macher-agent-presets '(test-preset))
              (macher-agent-setup-gptel-buffer)
              (expect macher-agent--is-restored-session :to-be nil)
              (expect macher-agent-presets :to-be nil))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "does not mark buffer as restored session when neither model nor backend is local"
      (let ((buf (generate-new-buffer "test-restore-buf-plain")))
        (unwind-protect
            (with-current-buffer buf
              (macher-agent--restore-local-state)
              (expect macher-agent--is-restored-session :to-be nil))
          (when (buffer-live-p buf)
            (kill-buffer buf))))))

  (describe "VFS Context Clearance"
    (it "clears persistent VFS context and resets FSM context without obsolete registries"
      (let* ((buf (generate-new-buffer "test-clear-ctx-buf"))
             (ws (make-macher-agent-workspace :project-root "/mock/clear/"))
             (mock-ctx (macher-agent--make-vfs-context :workspace ws :contents (list (make-macher-agent-vfs-entry :path "foo.el" :orig "data" :curr "data"))))
             (fsm (gptel-make-fsm :info (list :buffer buf :macher-agent-context mock-ctx))))
        (unwind-protect
            (with-current-buffer buf
              (setq-local macher-agent--persistent-context mock-ctx)
              (setq-local macher-agent--active-fsm fsm)
              (macher-agent-clear-context)
              (expect macher-agent--persistent-context :not :to-be nil)
              (expect (macher-agent--get-context-contents macher-agent--persistent-context) :to-be nil)
              (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be macher-agent--persistent-context))
          (when (buffer-live-p buf)
            (kill-buffer buf))))))

  (describe "Macher Tool Wrapping and Cons-Cell Context Coercion"
    (it "maps marshalled cons-cell contents back into macher-agent-vfs-entry structs"
      (let* ((buf (generate-new-buffer "test-wrap-tool-buf"))
             (ws (make-macher-agent-workspace :project-root "/mock/gptel/"))
             (init-entry (make-macher-agent-vfs-entry :path "init.el" :orig "orig" :curr "orig"))
             (agent-ctx (macher-agent--make-vfs-context :workspace ws :contents (list init-entry)))
             (orphaned-ctx (if (fboundp 'macher--make-context)
                               (macher--make-context :workspace (cons 'project "/mock/gptel/") :contents nil)
                             nil))
             (mock-tool (gptel-make-tool
                         :name "mock_macher_writer"
                         :category "macher"
                         :description "Writes virtual files via cons cells"
                         :args '((:name "path" :type string))
                         :function (lambda (m-ctx path)
                                     (when (fboundp 'macher-context-contents)
                                       (setf (macher-context-contents m-ctx)
                                             (list (cons path (cons "old text" "new text")))))
                                     "tool-result")))
             (macher-agent--wrapped-tools-hash (make-hash-table :test 'eq)))
        (unwind-protect
            (with-current-buffer buf
              (setq-local macher-agent--persistent-context agent-ctx)
              (macher-agent--wrap-single-tool mock-tool)
              (let ((res (funcall (gptel-tool-function mock-tool) orphaned-ctx "mapped-file.el")))
                (expect res :to-equal "tool-result")
                (let ((contents (macher-agent--get-context-contents agent-ctx)))
                  (expect (length contents) :to-equal 1)
                  (let ((entry (car contents)))
                    (expect (macher-agent-vfs-entry-p entry) :to-be t)
                    (expect (macher-agent-vfs-entry-path entry) :to-equal "mapped-file.el")
                    (expect (macher-agent-vfs-entry-orig entry) :to-equal "old text")
                    (expect (macher-agent-vfs-entry-curr entry) :to-equal "new text"))
                  (expect (macher-agent--get-context-dirty-p agent-ctx) :to-be t))))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "cleanly creates macher-context and updates contents via setf without invalid fboundp calls"
      (let ((ctx (macher--make-context :contents '(("file.el" . ("old" . "new"))))))
        (expect (macher-context-p ctx) :to-be t)
        (expect (macher-context-contents ctx) :to-equal '(("file.el" . ("old" . "new"))))
        (setf (macher-context-contents ctx) '(("file2.el" . ("a" . "b"))))
        (expect (macher-context-contents ctx) :to-equal '(("file2.el" . ("a" . "b"))))))

    (it "extracts tool names directly for tools, strings, and symbols"
      (let ((mock-tool (gptel-make-tool :name "my_tool" :description "desc")))
        (expect (macher-agent--extract-tool-name mock-tool) :to-equal "my_tool")
        (expect (macher-agent--extract-tool-name (cons "alias" mock-tool)) :to-equal "my_tool")
        (expect (macher-agent--extract-tool-name "str_tool") :to-equal "str_tool")
        (expect (macher-agent--extract-tool-name 'sym_tool) :to-equal "sym_tool")
        (expect (macher-agent--extract-tool-name 12345) :to-be nil))))

  (describe "PTC Tool UI Spoofing"
    (it "spoofs tool UI display in active FSM and invokes gptel--update-tool-call"
      (let* ((buf (generate-new-buffer "test-spoof-buf"))
             (fsm (gptel-make-fsm :info (list :buffer buf)))
             (update-call-invoked nil)
             (captured-tool-use nil))
        (unwind-protect
            (with-current-buffer buf
              (setq-local macher-agent--active-fsm fsm)
              (cl-letf (((symbol-function 'gptel--update-tool-call)
                         (lambda (called-fsm)
                           (setq update-call-invoked called-fsm)
                           (let* ((info (gptel-fsm-info called-fsm)))
                             (setq captured-tool-use (plist-get info :tool-use))))))
                (expect (macher-agent-gptel-spoof-tool-ui buf "my_special_tool") :to-be t)
                (expect update-call-invoked :to-be fsm)
                (expect (plist-get (car captured-tool-use) :name) :to-equal "PTC: my-special-tool")
                (let* ((info (gptel-fsm-info fsm))
                       (tool-use (plist-get info :tool-use)))
                  (expect tool-use :to-be nil))))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "spoofs tool UI via gptel--update-status when FSM is not active"
      (let* ((buf (generate-new-buffer "test-spoof-fallback-buf"))
             (status-msg nil))
        (unwind-protect
            (with-current-buffer buf
              (setq-local macher-agent--active-fsm nil)
              (cl-letf (((symbol-function 'gptel--update-status)
                         (lambda (msg &rest _)
                           (setq status-msg msg))))
                (expect (macher-agent-gptel-spoof-tool-ui (buffer-name buf) 'custom-tool) :to-be t)
                (expect status-msg :to-match "PTC: custom-tool")))
          (when (buffer-live-p buf)
            (kill-buffer buf))))))

  (describe "GPTEL Pre-Tool Audit Logging Delegation"
    (it "resolves context from FSM and delegates to macher-agent-log-tool-intent"
      (let* ((mock-ctx (make-macher-agent-context :id "ctx-pre-log" :project-root "/mock/log/"))
             (fsm (gptel-make-fsm :info (list :macher-agent-context mock-ctx)))
             (logged-args nil))
        (cl-letf (((symbol-function 'macher-agent-log-tool-intent)
                   (lambda (ctx type name args)
                     (setq logged-args (list ctx type name args)))))
          (macher-agent--log-gptel-pre-tool (list :name "search_files" :args '(:path "dir/")) fsm)
          (expect (nth 0 logged-args) :to-be mock-ctx)
          (expect (nth 1 logged-args) :to-equal "gptel-tool")
          (expect (nth 2 logged-args) :to-equal "search_files")
          (expect (nth 3 logged-args) :to-equal '(:path "dir/")))))

    (it "resolves context from buffer persistence when FSM is nil"
      (let* ((buf (generate-new-buffer "test-pre-log-buf"))
             (mock-ctx (make-macher-agent-context :id "ctx-buf-log" :project-root "/mock/buf-log/"))
             (logged-ctx nil))
        (unwind-protect
            (with-current-buffer buf
              (setq-local macher-agent--persistent-context mock-ctx)
              (cl-letf (((symbol-function 'macher-agent-log-tool-intent)
                         (lambda (ctx _type _name _args)
                           (setq logged-ctx ctx))))
                (macher-agent--log-gptel-pre-tool 'simple_tool nil :arg1 "val1")
                (expect logged-ctx :to-be mock-ctx)))
          (when (buffer-live-p buf)
            (kill-buffer buf))))))

  (describe "Prompt Assignment at Request Initiation"
    (it "assigns prompt explicitly to context prompt slot during macher-agent-gptel-transmit"
      (let* ((buf (generate-new-buffer "test-transmit-prompt-buf"))
             (mock-ctx (make-macher-agent-context :id "ctx-transmit-prompt" :project-root "/mock/prompt/"))
             (task-ctx (make-macher-agent-task-context
                        :target-buffer buf
                        :system-message "Explicit Task System Prompt")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (setq-local macher-agent--persistent-context mock-ctx))
              (expect (macher-agent-context-prompt mock-ctx) :to-be nil)
              (cl-letf (((symbol-function 'gptel-send) #'ignore))
                (macher-agent-gptel-transmit task-ctx nil))
              (expect (macher-agent-context-prompt mock-ctx) :to-equal "Explicit Task System Prompt"))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

(provide 'macher-agent-gptel-test)
;;; macher-agent-gptel-test.el ends here
