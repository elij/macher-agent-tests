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

;; Dummy gptel structures for mocking
(cl-defstruct mock-gptel-fsm info state)

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
                            (expect threw :to-be t))))

                    (it "asserts that different agent sessions within the same workspace share uncommitted VFS state"
                        (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                               (ctx-a (macher--make-context :workspace workspace :contents nil))
                               (ctx-b (macher--make-context :workspace workspace :contents nil))
                               (file-path "/mock/proj/shared.el"))
                          (puthash (expand-file-name "/mock/proj/") ctx-a macher-agent-active-workspaces)

                          (macher-agent-vfs-write (macher-agent-workspace-vfs-buffers workspace) (macher-agent-workspace-mtime-tracker workspace) file-path "Agent A changes")

                          (let ((read-content (macher-agent-vfs-read (macher-agent-workspace-vfs-buffers workspace) nil file-path)))
                            (expect read-content :to-equal "Agent A changes")))))

          (describe "2. Execution Environments (Sandbox)"
                    (it "asserts that sandbox inflation overlays the uncommitted VFS changes"
                        (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                               (ctx (macher--make-context :workspace workspace :contents nil)))
                          (macher-agent--set-context-data ctx :sandbox-path "/tmp/macher-sandbox/")
                          (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)

                          (puthash "/mock/proj/overlay.el" "VFS Overlay Content" (macher-agent-workspace-vfs-buffers workspace))

                          (let ((written-to-sandbox nil))
                            (spy-on 'file-in-directory-p :and-return-value t)
                            (spy-on 'write-region :and-call-fake
                                    (lambda (start end filename &rest _args)
                                      (when (string-suffix-p "overlay.el" filename)
                                        (setq written-to-sandbox (substring-no-properties start end)))))

                            (macher-agent-sandbox-inflate "/tmp/macher-sandbox/" (macher-agent-workspace-vfs-buffers workspace) (macher-agent-context-root ctx) (macher-agent--get-context-contents ctx))

                            (expect written-to-sandbox :to-equal "VFS Overlay Content")))))

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

                (macher-agent--inject-media-fsm-advice (lambda (f) f) fsm)

                (expect 'gptel--inject-media :to-have-been-called)
                (expect (macher-agent--get-context-data ctx :pending-media) :to-be nil))))

(describe "5. Diff Splitting Behaviour"
          (it "asserts that virtual buffer modifications are split from physical file modifications"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (context (macher--make-context :workspace (cons 'project "/mock/proj/")
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

(describe "5. Sandbox Security and Path Traversal (Jailbreaks)"

          (before-each
           (setq sandbox-root "/tmp/macher-sandbox/"))

          (it "REGRESSION: completely neutralises absolute path injections"
              (let ((malicious-path "/etc/passwd")
                    (threw nil))
                (condition-case _err
                    (macher-agent--resolve-safe-path malicious-path sandbox-root)
                  (error (setq threw t)))
                (expect threw :to-be t)))

          (it "prevents relative path traversal (Directory Climbing)"
              (let ((malicious-path "../../../../etc/passwd")
                    (threw nil))
                (condition-case _err
                    (macher-agent--resolve-safe-path malicious-path sandbox-root)
                  (error (setq threw t)))
                (expect threw :to-be t)))

          (it "prevents tilde (~) home directory escapes"
              (let ((malicious-path "~/.ssh/id_rsa")
                    (threw nil))
                (condition-case _err
                    (macher-agent--resolve-safe-path malicious-path sandbox-root)
                  (error (setq threw t)))
                (expect threw :to-be t))))

(describe "6. Agent Orchestration and Sub-agent Delegation"

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

(describe "7. Prompt Transformer Pipeline and Media Watcher"
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

          (it "deduplicates Org-mode tool usage blocks and preserves distinct signatures"
              (with-temp-buffer
                (insert "User prompt\n")
                (let ((str1 "#+begin_src tool (:name \"cargo_test\" :args nil)\noutput1\n#+end_src\n"))
                  (put-text-property 0 (length str1) 'gptel-tool t str1)
                  (insert str1))
                (insert "Intermediate prompt\n")
                (let ((str2 "#+begin_src tool (:name \"cargo_test\" :args nil)\noutput2\n#+end_src\n"))
                  (put-text-property 0 (length str2) 'gptel-tool t str2)
                  (insert str2))
                (let ((str3 "#+begin_src tool (:name \"read_file\" :args (\"a.txt\"))\noutput3\n#+end_src\n"))
                  (put-text-property 0 (length str3) 'gptel-tool t str3)
                  (insert str3))
                (let ((macher-agent-max-duplicate-tools 1))
                  (macher-agent-transformer-deduplicate-tools nil nil))
                (expect (buffer-string) :to-match "{\"status\": \"omitted\", \"reason\": \"duplicate\"}")
                (expect (buffer-string) :to-match "output2")
                (expect (buffer-string) :to-match "output3")))

          (it "snips context history exceeding character limits cleanly at turn boundaries"
              (with-temp-buffer
                (insert "Early user prompt content\n")
                (let ((resp "Previous response boundary\n"))
                  (put-text-property 0 (length resp) 'gptel 'response resp)
                  (insert resp))
                (insert "Latest user query content")
                (let ((macher-agent-max-context-chars '((nil . 25))))
                  (macher-agent-memory-pipe--truncate-buffer nil (current-buffer) nil nil nil))
                (expect (buffer-string) :to-match "Latest user query content")))

          (it "protects frontmatter header when snipping context history"
              (with-temp-buffer
                (insert "---\nkey: value\n---\nEarly user prompt content\n")
                (let ((resp "Previous response boundary\n"))
                  (put-text-property 0 (length resp) 'gptel 'response resp)
                  (insert resp))
                (insert "Latest user query content")
                (let ((macher-agent-max-context-chars '((nil . 25))))
                  (macher-agent-memory-pipe--truncate-buffer nil (current-buffer) nil nil nil))
                (expect (buffer-string) :to-match "^---\nkey: value\n---")))

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
                (macher-agent--inject-media-fsm-advice (lambda (&rest _) nil) fsm 'WAIT)
                (expect 'macher-agent--perform-pending-media-injection :to-have-been-called-with fsm))))

(describe "8. Buffer Resolution and User Interface Delegation (Phase 4)"
          (describe "macher-agent--resolve-buffer-name"
                    (it "resolves buffer objects to buffer name string"
                        (let ((buf (get-buffer-create "test-resolve-buf-obj")))
                          (unwind-protect
                              (expect (macher-agent--resolve-buffer-name buf) :to-equal "test-resolve-buf-obj")
                            (kill-buffer buf))))

                    (it "resolves string buffer name when buffer exists"
                        (let ((buf (get-buffer-create "test-resolve-buf-str")))
                          (unwind-protect
                              (expect (macher-agent--resolve-buffer-name "test-resolve-buf-str") :to-equal "test-resolve-buf-str")
                            (kill-buffer buf))))

                    (it "resolves file paths to buffer names via native get-file-buffer"
                        (let* ((temp-file (make-temp-file "macher-resolve-test"))
                               (buf (find-file-noselect temp-file)))
                          (unwind-protect
                              (progn
                                (expect (macher-agent--resolve-buffer-name temp-file) :to-equal (buffer-name buf))
                                (expect (macher-agent--resolve-buffer-name (expand-file-name temp-file)) :to-equal (buffer-name buf)))
                            (when (buffer-live-p buf) (kill-buffer buf))
                            (when (file-exists-p temp-file) (delete-file temp-file)))))

                    (it "returns original name string when buffer or file buffer is unmapped"
                        (expect (macher-agent--resolve-buffer-name "/tmp/nonexistent-file-path-xyz.el")
                                :to-equal "/tmp/nonexistent-file-path-xyz.el")))

          (describe "macher-agent-ui-show"
                    (it "invokes macher-agent-display-subagent-fn with target buffer"
                        (let* ((buf (get-buffer-create "test-ui-show-buf"))
                               (displayed-buf nil)
                               (macher-agent-display-subagent-fn (lambda (b) (setq displayed-buf b))))
                          (unwind-protect
                              (progn
                                (macher-agent-ui-show buf)
                                (expect displayed-buf :to-be buf))
                            (kill-buffer buf))))

                    (it "defaults target buffer to current-buffer when omitted"
                        (let* ((displayed-buf nil)
                               (macher-agent-display-subagent-fn (lambda (b) (setq displayed-buf b))))
                          (with-temp-buffer
                            (let ((cur (current-buffer)))
                              (macher-agent-ui-show)
                              (expect displayed-buf :to-be cur)))))

                    (it "handles nil display function gracefully without throwing"
                        (let ((macher-agent-display-subagent-fn nil))
                          (expect (macher-agent-ui-show) :to-be nil)))

                    (it "confirms redundant macher-agent--show-ui function is removed"
                        (expect (fboundp 'macher-agent--show-ui) :to-be nil))))

(describe "9. Context Resolution and Prompt Injection"
          (it "verifies macher-agent--resolve-context is deleted and unmapped"
              (expect (fboundp 'macher-agent--resolve-context) :to-be nil))

          (it "verifies macher-agent--get-active-context is deleted and unmapped"
              (expect (fboundp 'macher-agent--get-active-context) :to-be nil))

          (describe "macher-agent--transform-inject-context"
                    (it "injects context from live originating buffer into FSM info"
                        (let* ((origin-buf (get-buffer-create "test-origin-buf"))
                               (mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :buffer origin-buf :model "test-model")))
                               (called nil))
                          (unwind-protect
                              (progn
                                (with-current-buffer origin-buf
                                  (setq-local macher-agent--persistent-context mock-ctx))
                                (macher-agent--transform-inject-context
                                 (lambda () (setq called t))
                                 fsm)
                                (expect called :to-be t)
                                (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be mock-ctx)
                                (expect (plist-get (gptel-fsm-info fsm) :model) :to-equal "test-model"))
                            (kill-buffer origin-buf))))

                    (it "does not inject context if originating buffer context is nil"
                        (let* ((origin-buf (get-buffer-create "test-origin-buf-no-ctx"))
                               (fsm (gptel-make-fsm :info (list :buffer origin-buf :model "test-model")))
                               (called nil))
                          (unwind-protect
                              (progn
                                (with-current-buffer origin-buf
                                  (setq-local macher-agent--persistent-context nil))
                                (macher-agent--transform-inject-context
                                 (lambda () (setq called t))
                                 fsm)
                                (expect called :to-be t)
                                (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be nil))
                            (kill-buffer origin-buf))))

                    (it "does not inject context if originating buffer is dead"
                        (let* ((origin-buf (get-buffer-create "test-origin-buf-dead"))
                               (mock-ctx (macher--make-context :contents nil))
                               (fsm nil)
                               (called nil))
                          (with-current-buffer origin-buf
                            (setq-local macher-agent--persistent-context mock-ctx))
                          (setq fsm (gptel-make-fsm :info (list :buffer origin-buf)))
                          (kill-buffer origin-buf)
                          (macher-agent--transform-inject-context
                           (lambda () (setq called t))
                           fsm)
                          (expect called :to-be t)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be nil)))

                    (it "executes callback when fsm is nil"
                        (let ((called nil))
                          (macher-agent--transform-inject-context
                           (lambda () (setq called t))
                           nil)
                          (expect called :to-be t)))

                    (it "executes callback when fsm has no buffer in info"
                        (let* ((fsm (gptel-make-fsm :info (list :model "test-model")))
                               (called nil))
                          (macher-agent--transform-inject-context
                           (lambda () (setq called t))
                           fsm)
                          (expect called :to-be t)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be nil)))

                    (it "updates existing context in FSM info"
                        (let* ((origin-buf (get-buffer-create "test-origin-buf-update"))
                               (old-ctx (macher--make-context :contents nil))
                               (new-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :buffer origin-buf :macher-agent-context old-ctx :model "test-model")))
                               (called nil))
                          (unwind-protect
                              (progn
                                (with-current-buffer origin-buf
                                  (setq-local macher-agent--persistent-context new-ctx))
                                (macher-agent--transform-inject-context
                                 (lambda () (setq called t))
                                 fsm)
                                (expect called :to-be t)
                                (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be new-ctx)
                                (expect (plist-get (gptel-fsm-info fsm) :model) :to-equal "test-model"))
                            (kill-buffer origin-buf))))

                    (it "handles nil async-fn gracefully"
                        (let* ((origin-buf (get-buffer-create "test-origin-buf-nil-fn"))
                               (mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :buffer origin-buf :model "test-model"))))
                          (unwind-protect
                              (progn
                                (with-current-buffer origin-buf
                                  (setq-local macher-agent--persistent-context mock-ctx))
                                (macher-agent--transform-inject-context nil fsm)
                                (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be mock-ctx))
                            (kill-buffer origin-buf)))))

          (describe "macher-agent--inject-context-into-fsm-info"
                    (it "injects context into :macher--context and :macher-agent-context and returns t"
                        (let* ((mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :model "test-model" :buffer nil))))
                          (expect (macher-agent--inject-context-into-fsm-info mock-ctx fsm) :to-be t)
                          (expect (plist-get (gptel-fsm-info fsm) :macher--context) :to-be mock-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be mock-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :model) :to-equal "test-model")))

                    (it "defaults to gptel--fsm when fsm argument is omitted"
                        (let* ((mock-ctx (macher--make-context :contents nil))
                               (gptel--fsm (gptel-make-fsm :info (list :model "test-model"))))
                          (expect (macher-agent--inject-context-into-fsm-info mock-ctx) :to-be t)
                          (expect (plist-get (gptel-fsm-info gptel--fsm) :macher--context) :to-be mock-ctx)
                          (expect (plist-get (gptel-fsm-info gptel--fsm) :macher-agent-context) :to-be mock-ctx)))

                    (it "injects context safely when fsm info is initially nil"
                        (let* ((mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info nil)))
                          (expect (macher-agent--inject-context-into-fsm-info mock-ctx fsm) :to-be t)
                          (expect (plist-get (gptel-fsm-info fsm) :macher--context) :to-be mock-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be mock-ctx)))

                    (it "returns nil when no fsm is available"
                        (let ((gptel--fsm nil)
                              (mock-ctx (macher--make-context :contents nil)))
                          (expect (macher-agent--inject-context-into-fsm-info mock-ctx nil) :to-be nil))))

          (describe "macher-agent-bridge-reset-fsm-context"
                    (it "resets context when active FSM is bound via macher-agent--active-fsm"
                        (let* ((old-ctx (macher--make-context :contents nil))
                               (new-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :macher-agent-context old-ctx :model "test-model")))
                               (macher-agent--active-fsm fsm)
                               (gptel--fsm nil)
                               (gptel--fsm-last nil)
                               (macher--fsm-latest nil))
                          (macher-agent-bridge-reset-fsm-context new-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher--context) :to-be new-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-context) :to-be new-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be new-ctx)))

                    (it "resets context when active FSM is bound via gptel--fsm fallback"
                        (let* ((old-ctx (macher--make-context :contents nil))
                               (new-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :macher--context old-ctx)))
                               (macher-agent--active-fsm nil)
                               (gptel--fsm fsm)
                               (gptel--fsm-last nil)
                               (macher--fsm-latest nil))
                          (macher-agent-bridge-reset-fsm-context new-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher--context) :to-be new-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-context) :to-be new-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be new-ctx)))

                    (it "resets context when active FSM is bound via gptel--fsm-last fallback"
                        (let* ((old-ctx (macher--make-context :contents nil))
                               (new-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :macher-context old-ctx)))
                               (macher-agent--active-fsm nil)
                               (gptel--fsm nil)
                               (gptel--fsm-last fsm)
                               (macher--fsm-latest nil))
                          (macher-agent-bridge-reset-fsm-context new-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher--context) :to-be new-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-context) :to-be new-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be new-ctx)))

                    (it "resets context when active FSM is bound via macher--fsm-latest fallback"
                        (let* ((old-ctx (macher--make-context :contents nil))
                               (new-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :macher-agent-context old-ctx)))
                               (macher-agent--active-fsm nil)
                               (gptel--fsm nil)
                               (gptel--fsm-last nil)
                               (macher--fsm-latest fsm))
                          (macher-agent-bridge-reset-fsm-context new-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher--context) :to-be new-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-context) :to-be new-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be new-ctx)))

                    (it "does nothing when no FSM is active"
                        (let ((new-ctx (macher--make-context :contents nil))
                              (macher-agent--active-fsm nil)
                              (gptel--fsm nil)
                              (gptel--fsm-last nil)
                              (macher--fsm-latest nil))
                          (expect (macher-agent-bridge-reset-fsm-context new-ctx) :to-be nil)))

                    (it "does not update FSM info when no context property is present"
                        (let* ((new-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :model "test-model")))
                               (macher-agent--active-fsm fsm)
                               (gptel--fsm nil)
                               (gptel--fsm-last nil)
                               (macher--fsm-latest nil))
                          (macher-agent-bridge-reset-fsm-context new-ctx)
                          (expect (plist-get (gptel-fsm-info fsm) :macher--context) :to-be nil)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-context) :to-be nil)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be nil)
                          (expect (plist-get (gptel-fsm-info fsm) :model) :to-equal "test-model"))))

          (describe "macher-agent--transformer-sync-context"
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

                    (it "returns nil when context is nil"
                        (let ((orig-buf (get-buffer-create "test-ert-sync-ctx-nil")))
                          (unwind-protect
                              (progn
                                (with-current-buffer orig-buf
                                  (setq-local gptel-directives nil)
                                  (setq-local macher-agent-presets nil))
                                (let ((res (macher-agent--transformer-sync-context nil orig-buf)))
                                  (expect res :to-be nil)))
                            (kill-buffer orig-buf)))))

          (describe "macher-agent-sync-prompt-transformer"
                    (it "resolves context and syncs"
                        (let* ((orig-buf (get-buffer-create "test-ert-sync-prompt-orig"))
                               (temp-buf (get-buffer-create "test-ert-sync-prompt-temp"))
                               (mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :buffer orig-buf :macher-agent-context mock-ctx)))
                               (passed-ctx nil)
                               (passed-buf nil))
                          (unwind-protect
                              (progn
                                (cl-letf (((symbol-function 'macher-agent--transformer-sync-context)
                                           (lambda (ctx buf)
                                             (setq passed-ctx ctx)
                                             (setq passed-buf buf)
                                             ctx)))
                                  (with-current-buffer temp-buf
                                    (macher-agent-sync-prompt-transformer nil fsm))
                                  (expect passed-ctx :to-be mock-ctx)
                                  (expect passed-buf :to-be orig-buf)))
                            (kill-buffer orig-buf)
                            (kill-buffer temp-buf)))))

          (describe "macher-agent completion and flush hook triggering"
                    (it "verifies macher-agent--get-buffer-persistent-context is deleted"
                        (expect (fboundp 'macher-agent--get-buffer-persistent-context) :to-be nil))

                    (it "verifies macher-agent--a2a-callback is deleted"
                        (expect (boundp 'macher-agent--a2a-callback) :to-be nil)
                        (expect (fboundp 'macher-agent--a2a-callback) :to-be nil))

                    (it "verifies obsolete functions are deleted"
                        (expect (fboundp 'macher-agent--trigger-patch-on-complete) :to-be nil)
                        (expect (fboundp 'macher-agent--process-completed-fsm-buffer) :to-be nil)
                        (expect (fboundp 'macher-agent--get-workspace-root) :to-be nil)
                        (expect (fboundp 'macher-agent--pure-virtual-entry-p) :to-be nil))

                    (it "validates contexts correctly with macher-agent-valid-context-p"
                        (let* ((mock-ctx (macher--make-context :contents nil))
                               (ws (make-macher-agent-workspace :project-root "/tmp/proj"))
                               (max-lisp-eval-depth 300))
                          (expect (macher-agent-valid-context-p mock-ctx) :to-be t)
                          (expect (macher-agent-valid-context-p nil) :to-be nil)
                          (expect (macher-agent-valid-context-p "string") :to-be nil)
                          (expect (macher-agent-valid-context-p '(:foo "bar")) :to-be nil)
                          (expect (macher-agent-valid-context-p ws) :to-be nil)
                          (expect (macher-agent-valid-context-p [1 2 3]) :to-be nil)))

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
                               (fsm (gptel-make-fsm :info (list :buffer target-buf) :state 'DONE))
                               (flush-called nil)
                               (hook-fn (lambda () (setq flush-called t))))
                          (unwind-protect
                              (with-current-buffer target-buf
                                (setq-local macher-agent--pending-instructions-queue '("pending-1"))
                                (add-hook 'macher-agent-task-flush-hook hook-fn)
                                (macher-agent-gptel--trigger-flush fsm)
                                (expect flush-called :to-be t)
                                (expect macher-agent--pending-instructions-queue :to-be nil)
                                (remove-hook 'macher-agent-task-flush-hook hook-fn))
                            (kill-buffer target-buf)))))

          (describe "10. Source File Syntax and Parsing Integrity"
                    (it "parses all main source files without end-of-file or syntax errors"
                        (let* ((root (locate-dominating-file default-directory "macher-agent-core.el"))
                               (source-files '("macher-agent-core.el"
                                               "macher-agent-macher.el"
                                               "macher-agent-vfs.el"
                                               "macher-agent-sandbox.el"
                                               "macher-agent-presets.el"
                                               "macher-agent-gptel.el"
                                               "macher-agent-zero-mem.el"
                                               "macher-agent-orchestration.el"
                                               "macher-agent-tools.el"
                                               "macher-agent-api.el"
                                               "macher-agent.el")))
                          (dolist (file source-files)
                            (let* ((full-path (expand-file-name file (or root default-directory))))
                              (when (file-exists-p full-path)
                                (with-temp-buffer
                                  (insert-file-contents full-path)
                                  (goto-char (point-min))
                                  (let ((forms-read 0))
                                    (condition-case err
                                        (while t
                                          (read (current-buffer))
                                          (setq forms-read (1+ forms-read)))
                                      (end-of-file
                                       (expect forms-read :to-be-greater-than 0))
                                      (error
                                       (error "Failed parsing %s: %S" file err))))))))))

                    (it "verifies defun, let, if, and cond structures in macher-agent-core, macher, and vfs"
                        (let* ((root (locate-dominating-file default-directory "macher-agent-core.el"))
                               (target-files '("macher-agent-core.el"
                                               "macher-agent-macher.el"
                                               "macher-agent-vfs.el"))
                               (validate-node
                                (lambda (node self-fn)
                                  (when (consp node)
                                    (let ((head (car node)))
                                      (cond
                                       ((memq head '(defun cl-defun defmacro))
                                        (expect (length node) :to-be-greater-than 2)
                                        (expect (symbolp (nth 1 node)) :to-be t)
                                        (expect (listp (nth 2 node)) :to-be t))
                                       ((memq head '(let let*))
                                        (expect (length node) :to-be-greater-than 1)
                                        (expect (listp (nth 1 node)) :to-be t))
                                       ((eq head 'if)
                                        (expect (length node) :to-be-greater-than 2))
                                       ((eq head 'cond)
                                        (let ((clauses (cdr node)))
                                          (while (consp clauses)
                                            (expect (consp (car clauses)) :to-be t)
                                            (setq clauses (cdr clauses))))))
                                      (let ((curr node))
                                        (while (consp curr)
                                          (funcall self-fn (car curr) self-fn)
                                          (setq curr (cdr curr)))
                                        (when curr
                                          (funcall self-fn curr self-fn))))))))
                          (dolist (file target-files)
                            (let ((full-path (expand-file-name file (or root default-directory))))
                              (when (file-exists-p full-path)
                                (with-temp-buffer
                                  (insert-file-contents full-path)
                                  (goto-char (point-min))
                                  (condition-case err
                                      (while t
                                        (let ((form (read (current-buffer))))
                                          (funcall validate-node form validate-node)))
                                    (end-of-file nil)
                                    (error (error "Structural error in %s: %S" file err))))))))))

          (describe "11. Centralised Universal Constants, Global State, and Utility Functions"
                    (it "verifies macher-agent-active-workspaces is initialised in core"
                        (expect (boundp 'macher-agent-active-workspaces) :to-be t)
                        (expect (hash-table-p macher-agent-active-workspaces) :to-be t)
                        (expect (hash-table-test macher-agent-active-workspaces) :to-equal 'equal))

                    (it "verifies macher-agent-context-mutated-hook is defined in core"
                        (expect (boundp 'macher-agent-context-mutated-hook) :to-be t))

                    (it "verifies macher-agent--allow-lazy-init is defined in core"
                        (expect (boundp 'macher-agent--allow-lazy-init) :to-be t))

                    (it "verifies macher-agent-tools-registry is initialised in core"
                        (expect (boundp 'macher-agent-tools-registry) :to-be t)
                        (expect (hash-table-p macher-agent-tools-registry) :to-be t)
                        (expect (hash-table-test macher-agent-tools-registry) :to-equal 'equal))

                    (it "verifies macher-agent-global-skills-alist is defined in core"
                        (expect (boundp 'macher-agent-global-skills-alist) :to-be t))

                    (it "verifies macher-agent-max-context-chars is defined in core with default value"
                        (expect (boundp 'macher-agent-max-context-chars) :to-be t)
                        (expect (alist-get nil macher-agent-max-context-chars) :to-equal 2000000))

                    (it "resolves project root correctly with macher-agent-root"
                        (let ((default-directory "/tmp/test-project/"))
                          (expect (macher-agent-root) :to-equal (expand-file-name "/tmp/test-project/"))
                          (expect (macher-agent-root "/tmp/custom-path/") :to-equal (expand-file-name "/tmp/custom-path/"))))

                    (it "validates tools correctly with macher-tool-valid-p"
                        (expect (macher-tool-valid-p nil) :to-be nil)
                        (expect (macher-tool-valid-p "not-a-tool") :to-be nil)
                        (expect (macher-tool-valid-p '(not a tool)) :to-be nil))

                    (it "extracts raw tool names correctly with macher-agent--extract-raw-tool-name"
                        (expect (macher-agent--extract-raw-tool-name "tool_str") :to-equal "tool_str")
                        (expect (macher-agent--extract-raw-tool-name 'tool_sym) :to-equal "tool_sym")
                        (expect (macher-agent--extract-raw-tool-name '(:name "tool_plist")) :to-equal "tool_plist")
                        (expect (macher-agent--extract-raw-tool-name '(:function (:name "tool_fn_plist"))) :to-equal "tool_fn_plist")
                        (expect (macher-agent--extract-raw-tool-name '(:function tool_fn_sym)) :to-equal 'tool_fn_sym)
                        (expect (macher-agent--extract-raw-tool-name 12345) :to-equal 12345))

                    (it "coerces tools to canonical string names with macher-agent-canonical-tool-name"
                        (expect (macher-agent-canonical-tool-name "my_tool") :to-equal "my_tool")
                        (expect (macher-agent-canonical-tool-name 'my_tool) :to-equal "my_tool")
                        (expect (macher-agent-canonical-tool-name '(:name "my_tool")) :to-equal "my_tool")
                        (expect (macher-agent-canonical-tool-name '(:function my_tool)) :to-equal "my_tool")
                        (expect (macher-agent-canonical-tool-name nil) :to-be nil))

                    (it "verifies that macher-agent-core.el is the sole canonical definition site"
                        (let* ((root (locate-dominating-file default-directory "macher-agent-core.el"))
                               (read-file-forms
                                (lambda (file)
                                  (let ((forms nil)
                                        (full-path (expand-file-name file (or root default-directory))))
                                    (with-temp-buffer
                                      (insert-file-contents full-path)
                                      (goto-char (point-min))
                                      (condition-case nil
                                          (while t
                                            (push (read (current-buffer)) forms))
                                        (end-of-file (nreverse forms)))))))
                               (core-forms (funcall read-file-forms "macher-agent-core.el"))
                               (macher-forms (funcall read-file-forms "macher-agent-macher.el"))
                               (presets-forms (funcall read-file-forms "macher-agent-presets.el"))
                               (gptel-forms (funcall read-file-forms "macher-agent-gptel.el"))
                               (vfs-forms (funcall read-file-forms "macher-agent-vfs.el"))
                               (defines-sym-p
                                (lambda (forms sym)
                                  (cl-some (lambda (form)
                                             (and (consp form)
                                                  (memq (car form) '(defvar defcustom defun defsubst cl-defun))
                                                  (eq (cadr form) sym)
                                                  ;; defvar with an init-value is a definition
                                                  (or (not (eq (car form) 'defvar))
                                                      (> (length form) 2))))
                                           forms))))
                          ;; Core defines all of them
                          (expect (funcall defines-sym-p core-forms 'macher-agent-active-workspaces) :to-be t)
                          (expect (funcall defines-sym-p core-forms 'macher-agent-context-mutated-hook) :to-be t)
                          (expect (funcall defines-sym-p core-forms 'macher-agent--allow-lazy-init) :to-be t)
                          (expect (funcall defines-sym-p core-forms 'macher-agent-tools-registry) :to-be t)
                          (expect (funcall defines-sym-p core-forms 'macher-agent-global-skills-alist) :to-be t)
                          (expect (funcall defines-sym-p core-forms 'macher-tool-valid-p) :to-be t)
                          (expect (funcall defines-sym-p core-forms 'macher-agent--extract-raw-tool-name) :to-be t)
                          (expect (funcall defines-sym-p core-forms 'macher-agent-canonical-tool-name) :to-be t)
                          (expect (funcall defines-sym-p core-forms 'macher-agent-max-context-chars) :to-be t)
                          (expect (funcall defines-sym-p core-forms 'macher-agent-root) :to-be t)
                          ;; Removed from macher.el
                          (expect (funcall defines-sym-p macher-forms 'macher-agent-active-workspaces) :to-be nil)
                          (expect (funcall defines-sym-p macher-forms 'macher-agent-context-mutated-hook) :to-be nil)
                          (expect (funcall defines-sym-p macher-forms 'macher-agent--allow-lazy-init) :to-be nil)
                          ;; Removed from presets.el
                          (expect (funcall defines-sym-p presets-forms 'macher-agent-tools-registry) :to-be nil)
                          (expect (funcall defines-sym-p presets-forms 'macher-agent-global-skills-alist) :to-be nil)
                          (expect (funcall defines-sym-p presets-forms 'macher-tool-valid-p) :to-be nil)
                          (expect (funcall defines-sym-p presets-forms 'macher-agent--extract-raw-tool-name) :to-be nil)
                          (expect (funcall defines-sym-p presets-forms 'macher-agent-canonical-tool-name) :to-be nil)
                          ;; Removed from gptel.el
                          (expect (funcall defines-sym-p gptel-forms 'macher-agent-max-context-chars) :to-be nil)
                          (expect (funcall defines-sym-p gptel-forms 'macher-agent-active-workspaces) :to-be nil)
                          ;; Removed from vfs.el
                          (expect (funcall defines-sym-p vfs-forms 'macher-agent-root) :to-be nil)
                          (expect (funcall defines-sym-p vfs-forms 'macher-agent-context-mutated-hook) :to-be nil)))))

(describe "12. Prompt Synchronization and Fallback Resolution"
          (it "resolves prompt from :data :prompt when direct slot is nil"
              (let ((ctx (macher--make-context :prompt nil :data '(:prompt "fallback prompt message"))))
                (expect (macher-agent--get-context-prompt ctx) :to-equal "fallback prompt message")
                (expect (macher-agent--get-context-prompt ctx) :to-equal "fallback prompt message")))

          (it "resolves prompt from direct slot when direct slot is present"
              (let ((ctx (macher--make-context :prompt "direct prompt" :data '(:prompt "data prompt"))))
                (expect (macher-agent--get-context-prompt ctx) :to-equal "direct prompt")))

          (it "synchronizes both direct slot and :data :prompt when setting prompt"
              (let ((ctx (macher--make-context :prompt nil :data nil)))
                (macher-agent--set-context-prompt ctx "new synchronized prompt")
                (expect (macher-context-prompt ctx) :to-equal "new synchronized prompt")
                (expect (macher-agent--get-context-data ctx :prompt) :to-equal "new synchronized prompt")
                (expect (macher-agent--get-context-prompt ctx) :to-equal "new synchronized prompt")))

          (it "clones context preserving prompt in both direct and data slots"
              (let* ((orig (macher--make-context :workspace (make-macher-agent-workspace :project-root "/mock/proj")
                                                 :prompt nil
                                                 :data '(:prompt "cloned prompt text")))
                     (cloned (macher-agent--clone-context orig)))
                (expect (macher-context-prompt cloned) :to-equal "cloned prompt text")
                (expect (macher-agent--get-context-data cloned :prompt) :to-equal "cloned prompt text")
                (expect (macher-agent--get-context-prompt cloned) :to-equal "cloned prompt text")))

          (it "prepares patch contexts preserving prompt from :data :prompt in both slots"
              (let* ((ws (cons 'project "/mock/proj/"))
                     (ctx (macher--make-context :workspace ws
                                                :contents (list (macher-agent-vfs-make-entry "/mock/proj/file.el" "a" "b"))
                                                :prompt nil
                                                :data '(:prompt "patch preparation prompt")))
                     (res (macher-agent--prepare-patch-contexts ctx nil "/mock/proj/"))
                     (p-ctx (nth 1 res)))
                (expect p-ctx :not :to-be nil)
                (expect (macher-context-prompt p-ctx) :to-equal "patch preparation prompt")
                (expect (macher-agent--get-context-data p-ctx :prompt) :to-equal "patch preparation prompt")
                (expect (macher-agent--get-context-prompt p-ctx) :to-equal "patch preparation prompt")))

          (it "preserves prompt in macher-agent-macher-build-patch-from-hook when context only has :data :prompt"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (context (macher--make-context :workspace (cons 'project "/mock/proj/")
                                                    :contents (list (macher-agent-vfs-make-entry "/mock/proj/disk-file.el" "old" "new"))
                                                    :prompt nil
                                                    :data '(:prompt "hook patch prompt")))
                     (fsm (gptel-make-fsm))
                     (macher--fsm-latest fsm)
                     (prompt-seen nil))
                (spy-on 'rename-buffer)
                (spy-on 'macher--get-buffer :and-return-value (list (get-buffer-create "*patch*")))
                (spy-on 'macher--build-patch :and-call-fake
                        (lambda (ctx _fsm)
                          (push (macher-context-prompt ctx) prompt-seen)
                          (run-hooks 'macher-patch-ready-hook)))
                (setf (macher-context-dirty-p context) t)
                (macher-agent-macher-build-patch-from-hook context)
                (expect (car prompt-seen) :to-equal "hook patch prompt")))

          (it "transfers user prompt from orphaned context with :data :prompt via wrapped tool"
              (let* ((orphaned-ctx (macher--make-context :prompt nil :data '(:prompt "orphaned data prompt")))
                     (agent-ctx (macher--make-context :prompt nil :data nil))
                     (called-with-ctx nil)
                     (mock-tool (gptel-make-tool
                                 :name "mock_tool"
                                 :function (lambda (ctx cb &rest _args)
                                             (setq called-with-ctx ctx)
                                             (funcall cb "done"))
                                 :description "Mock Tool")))
                (spy-on 'macher-agent-resolve-context :and-return-value agent-ctx)
                (macher-agent--wrap-single-tool mock-tool)
                (funcall (gptel-tool-function mock-tool) orphaned-ctx (lambda (_res) nil))
                (expect (macher-context-prompt agent-ctx) :to-equal "orphaned data prompt")
                (expect (macher-agent--get-context-data agent-ctx :prompt) :to-equal "orphaned data prompt")
                (expect (macher-agent--get-context-prompt agent-ctx) :to-equal "orphaned data prompt")))

          (it "synchronizes prompt in both slots during vfs-handle-flush before running hooks"
              (let* ((ctx (macher--make-context :workspace (make-macher-agent-workspace :project-root "/mock/proj")
                                                :contents (list (macher-agent-vfs-make-entry "/mock/proj/f.el" "1" "2"))
                                                :prompt nil
                                                :data '(:prompt "vfs flush prompt")
                                                :dirty-p t))
                     (hook-prompt nil))
                (let ((macher-agent-vfs-flush-hook
                       (list (lambda (c)
                               (setq hook-prompt (macher-context-prompt c))))))
                  (macher-agent-vfs-handle-flush ctx)
                  (expect hook-prompt :to-equal "vfs flush prompt")
                  (expect (macher-context-prompt ctx) :to-equal "vfs flush prompt")
                  (expect (macher-agent--get-context-data ctx :prompt) :to-equal "vfs flush prompt"))))

          (it "allows setting prompt cleanly via macher-agent--set-context-prompt"
              (let ((ctx (macher--make-context :prompt nil :data nil)))
                (macher-agent--set-context-prompt ctx "public alias prompt")
                (expect (macher-context-prompt ctx) :to-equal "public alias prompt")
                (expect (macher-agent--get-context-prompt ctx) :to-equal "public alias prompt")))

          (it "executes macher-agent-macher-build-patch-from-hook without infinite recursion when setting prompt"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (context (macher--make-context :workspace (cons 'project "/mock/proj/")
                                                    :contents (list (macher-agent-vfs-make-entry "/mock/proj/file.el" "a" "b"))))
                     (fsm (gptel-make-fsm))
                     (macher--fsm-latest fsm))
                (setf (gptel-fsm-info fsm) (list :prompt "Hook Prompt Without Recursion"))
                (setf (macher-context-dirty-p context) t)
                (spy-on 'rename-buffer)
                (spy-on 'macher--get-buffer :and-return-value (list (get-buffer-create "*patch*")))
                (spy-on 'macher--build-patch :and-return-value nil)
                (macher-agent-macher-build-patch-from-hook context)
                (expect (macher-context-prompt context) :to-equal "Hook Prompt Without Recursion"))))

(describe "13. Active FSM Fallback Precedence"
          (it "prefers explicit current-fsm argument over all fallback variables"
              (let ((fsm-arg 'fsm-arg)
                    (macher-agent--active-fsm 'fsm-active)
                    (gptel--fsm 'fsm-gptel)
                    (gptel--fsm-last 'fsm-last)
                    (macher--fsm-latest 'fsm-latest))
                (expect (macher-agent-get-active-fsm fsm-arg) :to-equal 'fsm-arg)))

          (it "falls back to macher-agent--active-fsm when current-fsm is nil"
              (let ((macher-agent--active-fsm 'fsm-active)
                    (gptel--fsm 'fsm-gptel)
                    (gptel--fsm-last 'fsm-last)
                    (macher--fsm-latest 'fsm-latest))
                (expect (macher-agent-get-active-fsm nil) :to-equal 'fsm-active)
                (expect (macher-agent-get-active-fsm) :to-equal 'fsm-active)))

          (it "falls back to gptel--fsm when macher-agent--active-fsm is nil"
              (let ((macher-agent--active-fsm nil)
                    (gptel--fsm 'fsm-gptel)
                    (gptel--fsm-last 'fsm-last)
                    (macher--fsm-latest 'fsm-latest))
                (expect (macher-agent-get-active-fsm nil) :to-equal 'fsm-gptel)
                (expect (macher-agent-get-active-fsm) :to-equal 'fsm-gptel)))

          (it "falls back to gptel--fsm-last when gptel--fsm is nil"
              (let ((macher-agent--active-fsm nil)
                    (gptel--fsm nil)
                    (gptel--fsm-last 'fsm-last)
                    (macher--fsm-latest 'fsm-latest))
                (expect (macher-agent-get-active-fsm nil) :to-equal 'fsm-last)
                (expect (macher-agent-get-active-fsm) :to-equal 'fsm-last)))

          (it "falls back to macher--fsm-latest when gptel--fsm-last is nil"
              (let ((macher-agent--active-fsm nil)
                    (gptel--fsm nil)
                    (gptel--fsm-last nil)
                    (macher--fsm-latest 'fsm-latest))
                (expect (macher-agent-get-active-fsm nil) :to-equal 'fsm-latest)
                (expect (macher-agent-get-active-fsm) :to-equal 'fsm-latest)))

          (it "returns nil when none of the FSM sources are set"
              (let ((macher-agent--active-fsm nil)
                    (gptel--fsm nil)
                    (gptel--fsm-last nil)
                    (macher--fsm-latest nil))
                (expect (macher-agent-get-active-fsm) :to-be nil)
                (expect (macher-agent-get-active-fsm nil) :to-be nil))))

(provide 'macher-agent-core-test)
;;; macher-agent-core-test.el ends here

