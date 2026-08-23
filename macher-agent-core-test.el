;;; macher-agent-core-test.el --- Core behaviour tests for macher-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; This test suite enforces the specification for macher-agent,
;; focusing on VFS optimistic concurrency, lexical state management,
;; sandbox isolation, and diff splitting behaviours.

;;; Code:

(require 'subr-x)
(require 'buttercup)
(require 'macher-agent-macher)
(require 'cl-lib)
(require 'macher-agent)

(defvar gptel--fsm)
(defvar macher-agent--active-fsm)
(defvar gptel--fsm-last)
(defvar macher--fsm-latest)
(defvar sandbox-root)

(describe "Macher-Agent Core Behaviours"
  (after-each
   (setq macher-agent--pause-auto-sync nil))

  (describe "1. VFS and Optimistic Concurrency"
    (it "asserts that a VFS write is rejected if the underlying file has drifted"
      (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
             (ctx (macher--make-context :workspace workspace :contents nil))
             (file-path "/mock/proj/test.el")
             (original-mtime '(25000 12345))
             (drifted-mtime '(25000 99999)))
        (unwind-protect
            (progn
              (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
              (puthash file-path original-mtime (macher-agent-workspace-mtime-tracker workspace))

              (spy-on 'file-attributes :and-call-fake
                      (lambda (&rest args)
                        (let ((file (car args)))
                          (if (string= file file-path)
                              `(t 1 1 1 ,drifted-mtime ,drifted-mtime ,drifted-mtime 100 "mode" t 1 1)
                            nil))))

              (let ((threw nil))
                (condition-case err
                    (macher-agent-vfs-write (macher-agent-workspace-vfs-buffers workspace) (macher-agent-workspace-mtime-tracker workspace) file-path "New content")
                  (error
                   (setq threw t)
                   (expect (cadr err) :to-equal "Your previous edits to test.el were discarded due to external file modifications.  Please re-read and re-apply")))
                (expect threw :to-be t)))
          (remhash (expand-file-name "/mock/proj/") macher-agent-active-workspaces))))

    (it "asserts that different agent sessions within the same workspace share uncommitted VFS state"
      (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
             (ctx-a (macher--make-context :workspace workspace :contents nil))
             (ctx-b (macher--make-context :workspace workspace :contents nil))
             (file-path "/mock/proj/shared.el"))
        (unwind-protect
            (progn
              (puthash (expand-file-name "/mock/proj/") ctx-a macher-agent-active-workspaces)

              (macher-agent-vfs-write (macher-agent-workspace-vfs-buffers workspace) (macher-agent-workspace-mtime-tracker workspace) file-path "Agent A changes")

              (let ((read-content (macher-agent-vfs-read (macher-agent-workspace-vfs-buffers workspace) nil file-path)))
                (expect read-content :to-equal "Agent A changes")))
          (remhash (expand-file-name "/mock/proj/") macher-agent-active-workspaces)))))

  (describe "2. Execution Environments (Sandbox)"
    (it "asserts that sandbox inflation overlays the uncommitted VFS changes"
      (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
             (ctx (macher--make-context :workspace workspace :contents nil)))
        (macher-agent--set-context-data ctx :sandbox-path "/tmp/macher-sandbox/")
        (unwind-protect
            (progn
              (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
              (puthash "/mock/proj/overlay.el" "VFS Overlay Content" (macher-agent-workspace-vfs-buffers workspace))

              (let ((written-to-sandbox nil))
                (spy-on 'file-in-directory-p :and-return-value t)
                (spy-on 'write-region :and-call-fake
                        (lambda (start end filename &rest _args)
                          (when (string-suffix-p "overlay.el" filename)
                            (setq written-to-sandbox
                                  (cond
                                   ((stringp start)
                                    (if (and (integerp end) (<= end (length start)))
                                        (substring-no-properties start 0 end)
                                      (substring-no-properties start)))
                                   ((and (or (integerp start) (markerp start))
                                         (or (integerp end) (markerp end)))
                                    (buffer-substring-no-properties start end))
                                   (t
                                    (buffer-substring-no-properties (point-min) (point-max))))))))

                (macher-agent-sandbox-inflate "/tmp/macher-sandbox/" (macher-agent-workspace-vfs-buffers workspace) (macher-agent-context-root ctx) (macher-agent--get-context-contents ctx))
                (expect written-to-sandbox :to-equal "VFS Overlay Content")))
          (remhash (expand-file-name "/mock/proj/") macher-agent-active-workspaces)))))

  (describe "3. Context and Isolation (Lexical Survival)"
    (it "asserts that lexical context survives async gptel callbacks without buffer bleeding"
      (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
             (ctx (macher--make-context :workspace workspace :contents nil))
             (fsm (gptel-make-fsm))
             (executed-workspace nil))

        (setf (gptel-fsm-info fsm) (list :macher-agent-context ctx))

        (with-temp-buffer
          (let ((_original-buffer (current-buffer)))
            (with-temp-buffer
              (let* ((info (macher-agent--extract-fsm-info fsm))
                     (fsm-ctx (plist-get info :macher-agent-context)))
                (when fsm-ctx
                  (setq executed-workspace (macher-agent--get-context-workspace fsm-ctx)))))
            (expect executed-workspace :to-equal workspace)))))

    (it "asserts that subagent context cloning properly isolates hash tables"
      (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
             (ctx (macher--make-context :workspace workspace :contents nil))
             (vfs-ht (make-hash-table :test 'equal))
             (mtime-ht (make-hash-table :test 'equal)))
        (puthash "test.el" "vfs-content" vfs-ht)
        (puthash "test.el" '(123 456) mtime-ht)
        (macher-agent--set-context-data ctx :vfs-buffers vfs-ht)
        (macher-agent--set-context-data ctx :mtime-tracker mtime-ht)

        (let* ((cloned-ctx (macher-agent--clone-context ctx))
               (cloned-vfs (macher-agent--get-context-data cloned-ctx :vfs-buffers))
               (cloned-mtime (macher-agent--get-context-data cloned-ctx :mtime-tracker)))

          ;; Verify they have the same initial content
          (expect (gethash "test.el" cloned-vfs) :to-equal "vfs-content")
          (expect (gethash "test.el" cloned-mtime) :to-equal '(123 456))

          ;; Mutate original and verify clone is isolated
          (puthash "test.el" "mutated-vfs-content" vfs-ht)
          (puthash "test.el" '(999 999) mtime-ht)
          (puthash "new.el" "new-content" vfs-ht)

          (expect (gethash "test.el" cloned-vfs) :to-equal "vfs-content")
          (expect (gethash "test.el" cloned-mtime) :to-equal '(123 456))
          (expect (gethash "new.el" cloned-vfs) :to-be nil))))))

(describe "4. Media Injection Isolation"
  (it "asserts that media injection strictly checks FSM properties"
    (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
           (ctx (macher--make-context :workspace workspace :contents nil))
           (fsm (gptel-make-fsm))
           (_gptel--fsm-last fsm))

      (setf (gptel-fsm-info fsm) (list :macher-agent-context ctx))
      (macher-agent--set-context-data ctx :pending-media (list (list "mockbase64" :mime "image/png")))

      (spy-on 'gptel--inject-media :and-return-value nil)
      (spy-on 'gptel--inject-prompt :and-return-value nil)

      (macher-agent--inject-media-fsm-logic fsm)

      (expect 'gptel--inject-media :to-have-been-called)
      (expect (macher-agent--get-context-data ctx :pending-media) :to-be nil))))

(describe "5. Diff Splitting Behaviour"
  (it "asserts that virtual buffer modifications are split from physical file modifications"
    (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
           (context (macher--make-context :workspace workspace
                                          :contents (list (macher-agent-vfs-make-entry "/mock/proj/disk-file.el" "old" "new")
                                                          (macher-agent-vfs-make-entry "*scratch*" "old" "new"))))
           (fsm (gptel-make-fsm))
           (macher--fsm-latest fsm))

      (spy-on 'macher-agent-resolve-context :and-return-value context)
      (setf (gptel-fsm-info fsm) (list :macher-agent-context context))
      (setf (macher-context-dirty-p context) t)

      (spy-on 'rename-buffer)
      (spy-on 'macher--get-buffer :and-return-value (list (get-buffer-create "*patch*")))
      (spy-on 'macher-agent--set-context-prompt :and-call-through)

      (let ((orig-called-with nil)
            (prompt-seen nil))
        (spy-on 'macher--build-patch :and-call-fake
                (lambda (ctx _fsm)
                  (push (macher-context-prompt ctx) prompt-seen)
                  (push (macher-context-contents ctx) orig-called-with)
                  (run-hooks 'macher-patch-ready-hook)))
        (setf (gptel-fsm-info fsm) (plist-put (gptel-fsm-info fsm) :prompt "Test Prompt Message"))
        (macher-agent-macher-build-patch-from-hook context)

        (expect (car prompt-seen) :to-equal "Test Prompt Message")
        (expect (length orig-called-with) :to-equal 2)
        (expect (car (car orig-called-with)) :to-equal '("*scratch*" "old" . "new"))
        (expect (car (cadr orig-called-with)) :to-equal '("/mock/proj/disk-file.el" "old" . "new"))))))

(describe "6. Sandbox Security and Path Traversal (Jailbreaks)"
  (it "completely neutralises absolute path injections, directory climbing, and home directory escapes"
    (let ((sandbox-root "/tmp/macher-sandbox/"))
      (expect (condition-case _ (macher-agent--resolve-safe-path "/etc/passwd" sandbox-root) (error 'trapped))
              :to-equal 'trapped)
      (expect (condition-case _ (macher-agent--resolve-safe-path "../../../../etc/passwd" sandbox-root) (error 'trapped))
              :to-equal 'trapped)
      (expect (condition-case _ (macher-agent--resolve-safe-path "~/.ssh/id_rsa" sandbox-root) (error 'trapped))
              :to-equal 'trapped))))

(describe "7. Agent Orchestration and Sub-agent Delegation"
  (it "dispatches point-to-point A2A payloads and invokes completion callback"
    (spy-on 'macher-agent-resolve-context :and-return-value nil)
    (let* ((callback-result nil)
           (sub-buf (get-buffer-create "sub-agent-buf"))
           (payloads (list (list :type 'SEND_MESSAGE
                                 :task-id "task-001"
                                 :message "Do something"
                                 :metadata (list :buffer_name "sub-agent-buf" :background t))))
           (callback (lambda (res) (setq callback-result res))))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'gptel-send)
                       (lambda ()
                         (let* ((task-id (bound-and-true-p macher-agent--current-task-id))
                                (cb (when task-id (gethash task-id macher-agent--pending-callbacks))))
                           (when cb
                             (funcall cb (list :status 'success :data "Done" :task-id task-id)))))))
              (macher-agent-a2a-dispatch payloads callback))
            (expect (length callback-result) :to-equal 1)
            (expect (plist-get (aref callback-result 0) :status) :to-be 'success)
            (with-current-buffer sub-buf
              (expect (bound-and-true-p macher-agent--ready-to-reap) :to-be nil)))
        (kill-buffer sub-buf))))

  (it "returns a controlled error state when dispatching to a missing or invalid sub-agent buffer"
    (let* ((callback-result nil)
           (payloads (list (list :type 'SEND_MESSAGE
                                 :task-id "task-err-001"
                                 :message "Do something"
                                 :metadata (list :buffer_name "non-existent-buffer"))))
           (callback (lambda (res) (setq callback-result res))))
      (macher-agent-a2a-dispatch payloads callback)
      (expect (length callback-result) :to-equal 1)
      (expect (plist-get (aref callback-result 0) :status) :to-be 'error)
      (expect (plist-get (aref callback-result 0) :error)
              :to-equal "ERROR: Sub-agent buffer 'non-existent-buffer' not found.")))

  (it "verifies macher-agent--is-background and macher-agent--is-ephemeral are permanent-local"
    (expect (get 'macher-agent--is-background 'permanent-local) :to-be t)
    (expect (get 'macher-agent--is-ephemeral 'permanent-local) :to-be t)
    (expect (get 'macher-agent--routing-stack 'permanent-local) :to-be t)
    (expect (get 'macher-agent--current-task-id 'permanent-local) :to-be t)
    (expect (get 'macher-agent--is-subagent 'permanent-local) :to-be t)
    (expect (get 'macher-agent--ready-to-reap 'permanent-local) :to-be t)
    (expect (get 'macher-agent-presets 'permanent-local) :to-be t)
    (expect (get 'macher-agent--active-ptc-primitives 'permanent-local) :to-be t)
    (expect (get 'macher-agent--suppress-patch 'permanent-local) :to-be t)
    (expect (get 'macher-agent--boot-directive 'permanent-local) :to-be t)
    (let ((buf1 (generate-new-buffer "core-bg-eph-1"))
          (buf2 (generate-new-buffer "core-bg-eph-2")))
      (unwind-protect
          (progn
            (with-current-buffer buf1
              (setq-local macher-agent--is-background t)
              (setq-local macher-agent--is-ephemeral t))
            (with-current-buffer buf2
              (expect (bound-and-true-p macher-agent--is-background) :to-be nil)
              (expect (bound-and-true-p macher-agent--is-ephemeral) :to-be nil)))
        (kill-buffer buf1)
        (kill-buffer buf2))))

  (it "normalises preset names correctly in macher-agent-core"
    (expect (macher-normalise-preset-name 'reviewer) :to-be 'reviewer)
    (expect (macher-normalise-preset-name '@reviewer) :to-be 'reviewer)
    (expect (macher-normalise-preset-name '@@reviewer) :to-be 'reviewer)
    (expect (macher-normalise-preset-name "reviewer") :to-be 'reviewer)
    (expect (macher-normalise-preset-name "@reviewer") :to-be 'reviewer)
    (expect (macher-normalise-preset-name "@@reviewer") :to-be 'reviewer)
    (expect (macher-normalise-preset-name '("reviewer")) :to-be 'reviewer)
    (expect (macher-normalise-preset-name '(@reviewer)) :to-be 'reviewer)
    (expect (macher-normalise-preset-name nil) :to-be nil)))

(describe "8. Prompt Transformer Pipeline and Media Watcher"
  (it "deduplicates tool usage blocks keeping the most recent occurrences"
    (with-temp-buffer
      (insert "User prompt\n")
      (let ((str1 "```tool (:name \"read_file\" :args (\"file.el\"))\nargs\n```\n"))
        (put-text-property 0 (length str1) 'gptel t str1)
        (insert str1))
      (insert "Intermediate response\n")
      (let ((str2 "```tool (:name \"read_file\" :args (\"file.el\"))\nargs\n```\n"))
        (put-text-property 0 (length str2) 'gptel t str2)
        (insert str2))
      (insert "Latest prompt\n")
      (let ((macher-agent-max-duplicate-tools 1))
        (macher-agent-transformer-deduplicate-tools nil nil))
      (expect (buffer-string) :to-match "{\"status\": \"omitted\", \"reason\": \"duplicate\"}")))

  (it "snips context history exceeding character limits cleanly at turn boundaries and protects frontmatter"
    (with-temp-buffer
      (insert "---\nkey: value\n---\nEarly user prompt content\n")
      (let ((resp "Previous response boundary\n"))
        (put-text-property 0 (length resp) 'gptel 'response resp)
        (insert resp))
      (insert "Latest user query content")
      (let ((macher-agent-max-context-chars '((nil . 25))))
        (macher-agent-memory-pipe--truncate-buffer nil (current-buffer) nil nil nil))
      (expect (buffer-string) :to-match "^---\nkey: value\n---")
      (expect (buffer-string) :to-match "Latest user query content")))

  (it "registers sync prompt transformer and transformers in setup-gptel-buffer"
    (let ((macher-agent-prompt-transformers '(t1 t2)))
      (spy-on 'macher-agent-resolve-context :and-return-value t)
      (with-temp-buffer
        (macher-agent-setup-gptel-buffer)
        (expect gptel-prompt-transform-functions
                :to-equal '(macher-agent--transform-inject-context macher-agent-sync-prompt-transformer t t1 t2)))))

  (it "triggers pending media injection on FSM updates"
    (let* ((ctx (macher--make-context :workspace nil :contents nil))
           (fsm (gptel-make-fsm)))
      (setf (gptel-fsm-info fsm) (list :macher-agent-context ctx))
      (macher-agent--set-context-data ctx :pending-media (list "data"))
      (spy-on 'macher-agent--perform-pending-media-injection)
      (macher-agent--inject-media-fsm-logic fsm)
      (expect 'macher-agent--perform-pending-media-injection :to-have-been-called-with fsm))))

(describe "9. Buffer Resolution and User Interface Delegation"
  (it "resolves buffer objects, string names, and file paths to buffer name strings"
    (let* ((mock-file "/mock/proj/dummy-file.el")
           (buf (get-buffer-create "test-resolve-buf-file")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (setq buffer-file-name (expand-file-name mock-file)))
            (expect (macher-agent--resolve-buffer-name buf) :to-equal "test-resolve-buf-file")
            (expect (macher-agent--resolve-buffer-name "test-resolve-buf-file") :to-equal "test-resolve-buf-file")
            (expect (macher-agent--resolve-buffer-name mock-file) :to-equal "test-resolve-buf-file")
            (expect (macher-agent--resolve-buffer-name "/tmp/nonexistent-xyz.el") :to-equal "/tmp/nonexistent-xyz.el"))
        (when (buffer-live-p buf) (kill-buffer buf)))))

  (it "invokes macher-agent-display-subagent-fn with target or current buffer"
    (let* ((buf (get-buffer-create "test-ui-show-buf"))
           (displayed-buf nil)
           (macher-agent-display-subagent-fn (lambda (b) (setq displayed-buf b))))
      (unwind-protect
          (progn
            (macher-agent-ui-show buf)
            (expect displayed-buf :to-be buf)
            (with-current-buffer buf
              (macher-agent-ui-show)
              (expect displayed-buf :to-be buf)))
        (kill-buffer buf)))))

(describe "10. Context Resolution and Prompt Injection"
  (it "injects originating buffer context into FSM info when available"
    (let* ((origin-buf (get-buffer-create "test-origin-buf"))
           (mock-ctx (macher--make-context :contents nil))
           (fsm (gptel-make-fsm :info (list :buffer origin-buf :model "test-model")))
           (called nil))
      (unwind-protect
          (progn
            (with-current-buffer origin-buf
              (setq-local macher-agent--persistent-context mock-ctx))
            (macher-agent--transform-inject-context (lambda () (setq called t)) fsm)
            (expect called :to-be t)
            (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be mock-ctx)
            (expect (plist-get (gptel-fsm-info fsm) :model) :to-equal "test-model"))
        (kill-buffer origin-buf))))

  (it "injects context into :macher--context and :macher-agent-context in fsm info"
    (let* ((mock-ctx (macher--make-context :contents nil))
           (fsm (gptel-make-fsm :info (list :model "test-model" :buffer nil))))
      (expect (macher-agent--inject-context-into-fsm-info mock-ctx fsm) :to-be t)
      (expect (plist-get (gptel-fsm-info fsm) :macher--context) :to-be mock-ctx)
      (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be mock-ctx)))

  (it "resets context across active FSM fallback variables"
    (let* ((old-ctx (macher--make-context :contents nil))
           (new-ctx (macher--make-context :contents nil))
           (fsm (gptel-make-fsm :info (list :macher-agent-context old-ctx :model "test-model")))
           (macher-agent--active-fsm fsm))
      (macher-agent-bridge-reset-fsm-context new-ctx)
      (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be new-ctx)))

  (it "synchronizes context and initializes skills with buffer presets"
    (let* ((orig-buf (get-buffer-create "test-ert-sync-ctx-orig"))
           (mock-ctx (macher--make-context :contents nil))
           (synced-ctx nil)
           (init-skills-ctx nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'macher-agent--auto-sync-context)
                       (lambda (ctx) (setq synced-ctx ctx)))
                      ((symbol-function 'macher-agent-initialize-skills)
                       (lambda (ctx) (setq init-skills-ctx ctx))))
              (with-current-buffer orig-buf
                (setq-local gptel-directives '((custom-preset . "Custom directive text")))
                (setq-local gptel-system-prompt "Custom directive text")
                (setq-local macher-agent-presets nil))
              (let ((res (macher-agent--transformer-sync-context mock-ctx orig-buf)))
                (expect res :to-be mock-ctx)
                (expect synced-ctx :to-be mock-ctx)
                (expect init-skills-ctx :to-be mock-ctx)
                (with-current-buffer orig-buf
                  (expect macher-agent-presets :to-equal '(custom-preset))))))
        (kill-buffer orig-buf))))

  (it "validates contexts correctly with macher-agent-valid-context-p"
    (let* ((mock-ctx (macher--make-context :contents nil))
           (ws (make-macher-agent-workspace :project-root "/tmp/proj")))
      (expect (macher-agent-valid-context-p mock-ctx) :to-be t)
      (expect (macher-agent-valid-context-p nil) :to-be nil)
      (expect (macher-agent-valid-context-p "string") :to-be nil)
      (expect (macher-agent-valid-context-p '(:foo "bar")) :to-be nil)
      (expect (macher-agent-valid-context-p ws) :to-be nil)))

  (it "extracts workspace ID from diverse formats with macher-agent-extract-workspace-id"
    (let* ((ws (make-macher-agent-workspace :project-root "/tmp/proj"))
           (ht (make-hash-table :test 'equal)))
      (puthash :workspace-id "/tmp/proj-ht" ht)
      (expect (macher-agent-extract-workspace-id "/tmp/str-proj") :to-equal "/tmp/str-proj")
      (expect (macher-agent-extract-workspace-id ws) :to-equal ws)
      (expect (macher-agent-extract-workspace-id '(:workspace-id "/tmp/plist-proj")) :to-equal "/tmp/plist-proj")
      (expect (macher-agent-extract-workspace-id '((:workspace-id . "/tmp/alist-proj"))) :to-equal "/tmp/alist-proj")
      (expect (macher-agent-extract-workspace-id ht) :to-equal "/tmp/proj-ht")
      (expect (macher-agent-extract-workspace-id nil) :to-be nil)))

  (it "executes flush hook and clears instructions on FSM completion"
    (let* ((target-buf (get-buffer-create "test-ert-flush-hook-buf"))
           (mock-ctx (macher--make-context :contents nil))
           (fsm (gptel-make-fsm :info (list :buffer target-buf :macher-agent-context mock-ctx) :state 'DONE))
           (flush-called nil)
           (hook-fn (lambda (&rest _) (setq flush-called t))))
      (unwind-protect
          (with-current-buffer target-buf
            (setq-local macher-agent--persistent-context mock-ctx)
            (setq-local macher-agent--pending-instructions-queue '("pending-1"))
            (add-hook 'macher-agent-task-flush-hook hook-fn)
            (macher-agent-gptel--trigger-flush fsm)
            (expect flush-called :to-be t)
            (expect macher-agent--pending-instructions-queue :to-be nil))
        (remove-hook 'macher-agent-task-flush-hook hook-fn)
        (when (buffer-live-p target-buf)
          (kill-buffer target-buf))))))

(describe "11. Centralised Universal Constants, Global State, and Utility Functions"
  (it "verifies global state and registries are initialised in core"
    (expect (hash-table-p macher-agent-active-workspaces) :to-be t)
    (expect (boundp 'macher-agent-context-mutated-hook) :to-be t)
    (expect (boundp 'macher-agent--allow-lazy-init) :to-be t)
    (expect (hash-table-p macher-agent-tools-registry) :to-be t)
    (expect (boundp 'macher-agent-global-skills-alist) :to-be t)
    (expect (alist-get nil macher-agent-max-context-chars) :to-equal 2000000))

  (it "resolves project root correctly with macher-agent-root"
    (let ((default-directory "/tmp/test-project/"))
      (expect (macher-agent-root) :to-equal (expand-file-name "/tmp/test-project/"))
      (expect (macher-agent-root "/tmp/custom-path/") :to-equal (expand-file-name "/tmp/custom-path/"))))

  (it "extracts raw tool names and coerces to canonical string names"
    (expect (macher-agent--extract-raw-tool-name "tool_str") :to-equal "tool_str")
    (expect (macher-agent--extract-raw-tool-name 'tool_sym) :to-equal "tool_sym")
    (expect (macher-agent--extract-raw-tool-name '(:name "tool_plist")) :to-equal "tool_plist")
    (expect (macher-agent-canonical-tool-name "my_tool") :to-equal "my_tool")
    (expect (macher-agent-canonical-tool-name 'my_tool) :to-equal "my_tool")
    (expect (macher-agent-canonical-tool-name '(:name "my_tool")) :to-equal "my_tool")
    (expect (macher-agent-canonical-tool-name nil) :to-be nil)))

(describe "12. Prompt Synchronization and Fallback Resolution"
  (it "resolves, synchronizes, and preserves prompts across direct slots, :data, cloning, hooks, and tools"
    (let* ((ctx-fallback (macher--make-context :prompt nil :data '(:prompt "fallback prompt message")))
           (ctx-direct (macher--make-context :prompt "direct prompt" :data '(:prompt "data prompt")))
           (ctx-sync (macher--make-context :prompt nil :data nil)))
      ;; Direct vs fallback prompt extraction & sync
      (expect (macher-agent--get-context-prompt ctx-fallback) :to-equal "fallback prompt message")
      (expect (macher-agent--get-context-prompt ctx-direct) :to-equal "direct prompt")
      (macher-agent--set-context-prompt ctx-sync "new synchronized prompt")
      (expect (macher-context-prompt ctx-sync) :to-equal "new synchronized prompt")
      (expect (macher-agent--get-context-data ctx-sync :prompt) :to-equal "new synchronized prompt")
      (expect (macher-agent--get-context-prompt ctx-sync) :to-equal "new synchronized prompt")

      ;; Clone preservation
      (let* ((orig (macher--make-context :workspace (make-macher-agent-workspace :project-root "/mock/proj")
                                         :prompt nil
                                         :data '(:prompt "cloned prompt text")))
             (cloned (macher-agent--clone-context orig)))
        (expect (macher-context-prompt cloned) :to-equal "cloned prompt text")
        (expect (macher-agent--get-context-prompt cloned) :to-equal "cloned prompt text"))

      ;; Hook preservation
      (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
             (context (macher--make-context :workspace workspace
                                            :contents (list (macher-agent-vfs-make-entry "/mock/proj/disk-file.el" "old" "new"))
                                            :prompt nil
                                            :data '(:prompt "hook patch prompt")))
             (fsm (gptel-make-fsm))
             (macher--fsm-latest fsm)
             (prompt-seen nil))
        (setf (gptel-fsm-info fsm) (list :prompt "Hook Prompt Without Recursion"))
        (setf (macher-context-dirty-p context) t)
        (spy-on 'rename-buffer)
        (spy-on 'macher--get-buffer :and-return-value (list (get-buffer-create "*patch*")))
        (spy-on 'macher--build-patch :and-call-fake
                (lambda (ctx _fsm)
                  (push (macher-context-prompt ctx) prompt-seen)
                  (run-hooks 'macher-patch-ready-hook)))
        (macher-agent-macher-build-patch-from-hook context)
        (expect (car prompt-seen) :to-equal "hook patch prompt")
        (expect (macher-context-prompt context) :to-equal "hook patch prompt")))))

(describe "13. Active FSM Fallback Precedence"
  (it "resolves active FSM according to strict fallback hierarchy"
    (let ((fsm-arg 'fsm-arg)
          (fsm-active 'fsm-active)
          (fsm-gptel 'fsm-gptel)
          (fsm-last 'fsm-last)
          (fsm-latest 'fsm-latest))
      ;; 1. Explicit argument overrides all
      (let ((macher-agent--active-fsm fsm-active)
            (gptel--fsm fsm-gptel)
            (gptel--fsm-last fsm-last)
            (macher--fsm-latest fsm-latest))
        (expect (macher-agent-get-active-fsm fsm-arg) :to-equal fsm-arg))
      ;; 2. macher-agent--active-fsm fallback
      (let ((macher-agent--active-fsm fsm-active)
            (gptel--fsm fsm-gptel))
        (expect (macher-agent-get-active-fsm nil) :to-equal fsm-active))
      ;; 3. gptel--fsm fallback
      (let ((macher-agent--active-fsm nil)
            (gptel--fsm fsm-gptel))
        (expect (macher-agent-get-active-fsm nil) :to-equal fsm-gptel))
      ;; 4. gptel--fsm-last fallback
      (let ((macher-agent--active-fsm nil)
            (gptel--fsm nil)
            (gptel--fsm-last fsm-last))
        (expect (macher-agent-get-active-fsm nil) :to-equal fsm-last))
      ;; 5. macher--fsm-latest fallback
      (let ((macher-agent--active-fsm nil)
            (gptel--fsm nil)
            (gptel--fsm-last nil)
            (macher--fsm-latest fsm-latest))
        (expect (macher-agent-get-active-fsm nil) :to-equal fsm-latest))
      ;; 6. Nil when none set
      (let ((macher-agent--active-fsm nil)
            (gptel--fsm nil)
            (gptel--fsm-last nil)
            (macher--fsm-latest nil))
        (expect (macher-agent-get-active-fsm) :to-be nil)))))

(provide 'macher-agent-core-test)
;;; macher-agent-core-test.el ends here
