;;; macher-agent-core-test.el --- Core behaviour tests for macher-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; This test suite enforces the specification for macher-agent,
;; focusing on primary agent context envelopes, lexical state management,
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
(defvar sandbox-root)

(describe "1. VFS and Optimistic Concurrency"
          (after-each
           (setq macher-agent--pause-auto-sync nil))

          (it "asserts that a VFS write warns if the underlying file has drifted"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher-agent--make-context :project-root "/mock/proj/"))
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
                                      (list t 1 1 1 drifted-mtime drifted-mtime drifted-mtime 100 "mode" t 1 1)
                                    nil))))

                      (spy-on 'display-warning)

                      (macher-agent-vfs-write (macher-agent-workspace-vfs-buffers workspace)
                                              (macher-agent-workspace-mtime-tracker workspace)
                                              file-path
                                              "New content")

                      (expect 'display-warning :to-have-been-called-with
                              'macher-agent
                              "Your previous edits to test.el were discarded due to external file modifications.  Please re-read and re-apply"
                              :warning))
                  (remhash (expand-file-name "/mock/proj/") macher-agent-active-workspaces)))))

(describe "2. Execution Environments (Sandbox)"
          (it "asserts that sandbox inflation overlays the uncommitted VFS changes"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher-agent--make-context :project-root "/mock/proj/")))
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

                        (macher-agent-vfs-scratch-inflate "/tmp/macher-sandbox/" (macher-agent-workspace-vfs-buffers workspace) (macher-agent-context-root ctx) (macher-agent--get-context-contents ctx))
                        (expect written-to-sandbox :to-equal "VFS Overlay Content")))
                  (remhash (expand-file-name "/mock/proj/") macher-agent-active-workspaces)))))

(describe "3. Context and Isolation (Lexical Survival)"
          (it "asserts that lexical context survives async gptel callbacks without buffer bleeding"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (fsm (gptel-make-fsm))
                     (executed-root nil))

                (setf (gptel-fsm-info fsm) (list :macher-agent-context ctx))

                (with-temp-buffer
                  (let ((_original-buffer (current-buffer)))
                    (with-temp-buffer
                      (let* ((info (macher-agent--extract-fsm-info fsm))
                             (fsm-ctx (plist-get info :macher-agent-context)))
                        (when fsm-ctx
                          (setq executed-root (macher-agent--get-context-root fsm-ctx)))))
                    (expect executed-root :to-equal (file-truename (expand-file-name "/mock/proj/")))))))

          (it "asserts that subagent context cloning properly isolates hash tables"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (vfs-ht (make-hash-table :test 'equal))
                     (mtime-ht (make-hash-table :test 'equal)))
                (puthash "test.el" "vfs-content" vfs-ht)
                (puthash "test.el" '(123 456) mtime-ht)
                (macher-agent--set-context-data ctx :vfs-buffers vfs-ht)
                (macher-agent--set-context-data ctx :mtime-tracker mtime-ht)

                (let* ((cloned-ctx (macher-agent--copy-context ctx))
                       (cloned-vfs (copy-hash-table (macher-agent--get-context-data ctx :vfs-buffers)))
                       (cloned-mtime (copy-hash-table (macher-agent--get-context-data ctx :mtime-tracker))))
                  (macher-agent--set-context-data cloned-ctx :vfs-buffers cloned-vfs)
                  (macher-agent--set-context-data cloned-ctx :mtime-tracker cloned-mtime)

                  ;; Verify they have the same initial content
                  (expect (gethash "test.el" cloned-vfs) :to-equal "vfs-content")
                  (expect (gethash "test.el" cloned-mtime) :to-equal '(123 456))

                  ;; Mutate original and verify clone is isolated
                  (puthash "test.el" "mutated-vfs-content" vfs-ht)
                  (puthash "test.el" '(999 999) mtime-ht)
                  (puthash "new.el" "new-content" vfs-ht)

                  (expect (gethash "test.el" cloned-vfs) :to-equal "vfs-content")
                  (expect (gethash "test.el" cloned-mtime) :to-equal '(123 456))
                  (expect (gethash "new.el" cloned-vfs) :to-be nil)))))

(describe "4. Media Injection Isolation"
          (it "asserts that media injection strictly checks FSM properties"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (fsm (gptel-make-fsm))
                     (mock-backend (gptel-make-openai "MockBackend"))
                     (mock-data (list :messages [])))

                (setf (gptel-fsm-info fsm)
                      (list :macher-agent-context ctx
                            :backend mock-backend
                            :data mock-data))
                (macher-agent--set-context-data ctx :pending-media "mockbase64")

                (spy-on 'gptel--inject-media :and-return-value nil)
                (spy-on 'gptel--inject-prompt :and-return-value nil)

                (macher-agent--inject-media-fsm-logic fsm)

                (expect 'gptel--inject-media :to-have-been-called)
                (expect 'gptel--inject-prompt :to-have-been-called)
                (expect (macher-agent--get-context-data ctx :pending-media) :to-be nil)))

          (it "processes context media-queue through FSM media injection and clears queue"
              (let* ((ctx (macher-agent--make-context :project-root "/mock/proj/"
                                                      :media-queue '("data:image/png;base64,mockdata")))
                     (fsm (gptel-make-fsm))
                     (mock-backend (gptel-make-openai "MockBackend"))
                     (mock-data (list :messages [])))

                (setf (gptel-fsm-info fsm)
                      (list :macher-agent-context ctx
                            :backend mock-backend
                            :data mock-data))

                (spy-on 'gptel--inject-media :and-return-value nil)
                (spy-on 'gptel--inject-prompt :and-return-value nil)

                (macher-agent--inject-media-fsm-logic fsm)

                (expect 'gptel--inject-media :to-have-been-called)
                (expect 'gptel--inject-prompt :to-have-been-called)
                (expect (macher-agent-context-media-queue ctx) :to-be nil)
                (expect (macher-agent--get-context-data ctx :pending-media) :to-be nil))))

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
              (let* ((ctx (macher-agent--make-context :project-root "/mock/proj/"))
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
                      (expect (macher-agent--resolve-buffer-name "/tmp/nonexistent-xyz.el") :to-equal "/tmp/nonexistent-xyz.el")))
                (when (buffer-live-p buf) (kill-buffer buf))))

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
                     (mock-ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (fsm (gptel-make-fsm :info (list :buffer origin-buf :model "test-model")))
                     (called nil))
                (unwind-protect
                    (progn
                      (with-current-buffer origin-buf
                        (setq-local macher-agent--persistent-context mock-ctx))
                      (macher-agent--transform-inject-context (lambda () (setq called t)) fsm)
                      (expect called :to-be t)
                      (with-current-buffer origin-buf
                        (expect (bound-and-true-p macher-agent--active-fsm) :to-be fsm))
                      (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be mock-ctx)
                      (expect (plist-get (gptel-fsm-info fsm) :model) :to-equal "test-model"))
                  (kill-buffer origin-buf))))

          (it "extracts context from FSM using macher-agent--extract-fsm-context"
              (let* ((mock-ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (fsm-agent (gptel-make-fsm :info (list :macher-agent-context mock-ctx)))
                     (fsm-ctx (gptel-make-fsm :info (list :context mock-ctx)))
                     (fsm-legacy (gptel-make-fsm :info (list :macher--context mock-ctx)))
                     (fsm-none (gptel-make-fsm :info nil)))
                (expect (macher-agent--extract-fsm-context nil) :to-be nil)
                (expect (macher-agent--extract-fsm-context mock-ctx) :to-be mock-ctx)
                (expect (macher-agent--extract-fsm-context fsm-agent) :to-be mock-ctx)
                (expect (macher-agent--extract-fsm-context fsm-ctx) :to-be mock-ctx)
                (expect (macher-agent--extract-fsm-context fsm-legacy) :to-be nil)
                (expect (macher-agent--extract-fsm-context fsm-none) :to-be nil)))

          (it "injects context into :macher-agent-context in fsm info"
              (let* ((mock-ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (fsm (gptel-make-fsm :info (list :model "test-model" :buffer nil))))
                (expect (macher-agent--inject-context-into-fsm-info mock-ctx fsm) :to-be t)
                (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be mock-ctx)))

          (it "resets context across active FSM fallback variables"
              (let* ((old-ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (new-ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (fsm (gptel-make-fsm :info (list :macher-agent-context old-ctx :model "test-model")))
                     (macher-agent--active-fsm fsm))
                (macher-agent-bridge-reset-fsm-context new-ctx)
                (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be new-ctx)))

          (it "synchronizes context with buffer presets"
              (let* ((orig-buf (get-buffer-create "test-ert-sync-ctx-orig"))
                     (mock-ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (synced-ctx nil))
                (unwind-protect
                    (progn
                      (cl-letf (((symbol-function 'macher-agent--auto-sync-context)
                                 (lambda (ctx) (setq synced-ctx ctx))))
                        (with-current-buffer orig-buf
                          (setq-local gptel-directives '((custom-preset . "Custom directive text")))
                          (setq-local gptel-system-prompt "Custom directive text")
                          (setq-local macher-agent-presets '(base-preset)))
                        (let ((res (macher-agent--transformer-sync-context mock-ctx orig-buf)))
                          (expect res :to-be mock-ctx)
                          (expect synced-ctx :to-be mock-ctx)
                          (with-current-buffer orig-buf
                            (expect macher-agent-presets :to-equal '(base-preset))))))
                  (kill-buffer orig-buf))))

          (it "extracts target buffer and context from FSM safely"
              (let* ((target-buf (get-buffer-create "test-fsm-target-ctx-buf"))
                     (mock-ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (fsm1 (gptel-make-fsm :info (list :buffer target-buf :macher-agent-context mock-ctx)))
                     (fsm2 (gptel-make-fsm :info (list :buffer target-buf :context mock-ctx)))
                     (fsm3 (gptel-make-fsm :info (list :buffer target-buf)))
                     (fsm-empty (gptel-make-fsm :info nil)))
                (unwind-protect
                    (progn
                      (expect (macher-agent-gptel--fsm-target-buffer nil) :to-be nil)
                      (expect (macher-agent-gptel--fsm-target-buffer fsm-empty) :to-be nil)
                      (expect (macher-agent-gptel--fsm-target-buffer fsm1) :to-be target-buf)

                      (expect (macher-agent-gptel--fsm-context nil) :to-be nil)
                      (expect (macher-agent-gptel--fsm-context fsm1) :to-be mock-ctx)
                      (expect (macher-agent-gptel--fsm-context fsm2) :to-be mock-ctx)

                      (with-current-buffer target-buf
                        (setq-local macher-agent--persistent-context mock-ctx))
                      (expect (macher-agent-gptel--fsm-context fsm3) :to-be mock-ctx))
                  (when (buffer-live-p target-buf)
                    (kill-buffer target-buf)))))

          (it "validates contexts correctly with macher-agent-valid-context-p"
              (let* ((mock-ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (ws (make-macher-agent-workspace :project-root "/tmp/proj")))
                (expect (macher-agent-valid-context-p mock-ctx) :to-be t)
                (expect (macher-agent-valid-context-p nil) :to-be nil)
                (expect (macher-agent-valid-context-p "string") :to-be nil)
                (expect (macher-agent-valid-context-p '(:foo "bar")) :to-be nil)
                (expect (macher-agent-valid-context-p ws) :to-be nil)))

          (it "extracts workspace ID from diverse formats with macher-agent-extract-workspace-id"
              (let* ((ws (make-macher-agent-workspace :project-root "/tmp/proj")))
                (expect (macher-agent-extract-workspace-id "/tmp/str-proj") :to-equal "/tmp/str-proj")
                (expect (macher-agent-extract-workspace-id '(project . "/tmp/proj")) :to-equal '(project . "/tmp/proj"))
                (expect (macher-agent-extract-workspace-id '(agent . "/tmp/proj")) :to-equal '(agent . "/tmp/proj"))
                (expect (macher-agent-extract-workspace-id ws) :to-equal ws)
                (expect (macher-agent-extract-workspace-id '(:workspace-id "/tmp/plist-proj")) :to-equal "/tmp/plist-proj")
                (expect (macher-agent-extract-workspace-id '(:workspace "/tmp/ws-proj")) :to-equal "/tmp/ws-proj")
                (expect (macher-agent-extract-workspace-id '(:target-workspace "/tmp/t-proj")) :to-equal "/tmp/t-proj")
                (expect (macher-agent-extract-workspace-id '(:shared-state (:workspace-id "/tmp/shared-proj"))) :to-equal "/tmp/shared-proj")
                (expect (macher-agent-extract-workspace-id nil) :to-be nil)))

          (it "executes flush hook and clears instructions on FSM completion"
              (let* ((target-buf (get-buffer-create "test-ert-flush-hook-buf"))
                     (mock-ctx (macher-agent--make-context :project-root "/mock/proj/"))
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
          (it "resolves, synchronizes, and preserves prompts across direct slots, plugins, and copiers"
              (let* ((ctx-fallback (macher-agent--make-context :plugins '(:prompt "fallback prompt message")))
                     (ctx-direct (macher-agent--make-context :plugins '(:prompt "data prompt")))
                     (ctx-sync (macher-agent--make-context)))
                ;; Direct vs fallback prompt extraction & sync
                (expect (macher-agent--get-context-prompt ctx-fallback) :to-equal "fallback prompt message")
                (expect (macher-agent--get-context-prompt ctx-direct) :to-equal "data prompt")
                (macher-agent--set-context-prompt ctx-sync "new synchronized prompt")
                (expect (macher-agent--get-context-data ctx-sync :prompt) :to-equal "new synchronized prompt")
                (expect (macher-agent--get-context-prompt ctx-sync) :to-equal "new synchronized prompt")

                ;; Clone preservation
                (let* ((orig (macher-agent--make-context :project-root "/mock/proj"
                                                         :plugins '(:prompt "cloned prompt text")))
                       (cloned (macher-agent--copy-context orig)))
                  (expect (macher-agent--get-context-prompt cloned) :to-equal "cloned prompt text")))))

(describe "13. Active FSM Fallback Precedence"
          (it "resolves active FSM according to strict fallback hierarchy"
              (let ((fsm-arg 'fsm-arg)
                    (fsm-active 'fsm-active)
                    (fsm-gptel 'fsm-gptel)
                    (fsm-last 'fsm-last))
                ;; 1. Explicit argument overrides all
                (let ((macher-agent--active-fsm fsm-active)
                      (gptel--fsm fsm-gptel)
                      (gptel--fsm-last fsm-last))
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
                ;; 5. Nil when none set
                (let ((macher-agent--active-fsm nil)
                      (gptel--fsm nil)
                      (gptel--fsm-last nil))
                  (expect (macher-agent-get-active-fsm) :to-be nil)))))

(describe "14. `macher-agent-context` Struct Definition, Slots, Predicate, and Copier"
          (it "initialises macher-agent-context with defaults and constructor arguments"
              (let ((ctx-default (macher-agent--make-context))
                    (buf (get-buffer-create "test-origin-buf-ctx")))
                (unwind-protect
                    (let ((ctx (macher-agent--make-context
                                :id "ctx-test-101"
                                :project-root "/mock/agent-proj/"
                                :origin-buffer buf
                                :tools '(tool-a tool-b)
                                :skills '(skill-a skill-b)
                                :media-queue '("img1.png" "img2.png")
                                :subagents '("subagent-1")
                                :plugins '(:key1 "val1" :key2 42))))
                      ;; Defaults
                      (expect (macher-agent-context-p ctx-default) :to-be t)
                      (expect (fboundp 'macher-agent-context-zero-mem) :to-be nil)
                      (expect (macher-agent-context-id ctx-default) :to-be nil)
                      (expect (macher-agent-context-project-root ctx-default) :to-be nil)
                      (expect (macher-agent-context-origin-buffer ctx-default) :to-be nil)
                      (expect (macher-agent-context-tools ctx-default) :to-be nil)
                      (expect (macher-agent-context-skills ctx-default) :to-be nil)
                      (expect (macher-agent-context-media-queue ctx-default) :to-be nil)
                      (expect (macher-agent-context-subagents ctx-default) :to-be nil)
                      (expect (macher-agent-context-plugins ctx-default) :to-be nil)

                      ;; Initialized struct
                      (expect (macher-agent-context-p ctx) :to-be t)
                      (expect (macher-agent-context-id ctx) :to-equal "ctx-test-101")
                      (expect (macher-agent-context-project-root ctx) :to-equal "/mock/agent-proj/")
                      (expect (macher-agent-context-origin-buffer ctx) :to-be buf)
                      (expect (macher-agent-context-tools ctx) :to-equal '(tool-a tool-b))
                      (expect (macher-agent-context-skills ctx) :to-equal '(skill-a skill-b))
                      (expect (macher-agent-context-media-queue ctx) :to-equal '("img1.png" "img2.png"))
                      (expect (macher-agent-context-subagents ctx) :to-equal '("subagent-1"))
                      (expect (macher-agent-context-plugins ctx) :to-equal '(:key1 "val1" :key2 42)))
                  (when (buffer-live-p buf) (kill-buffer buf)))))

          (it "allows mutating slots via setf on macher-agent-context"
              (let ((ctx (macher-agent--make-context))
                    (buf (get-buffer-create "test-mut-buf")))
                (unwind-protect
                    (progn
                      (setf (macher-agent-context-id ctx) "new-ctx-id")
                      (expect (macher-agent-context-id ctx) :to-equal "new-ctx-id")

                      (setf (macher-agent-context-project-root ctx) "/updated/root/")
                      (expect (macher-agent-context-project-root ctx) :to-equal "/updated/root/")

                      (setf (macher-agent-context-origin-buffer ctx) buf)
                      (expect (macher-agent-context-origin-buffer ctx) :to-be buf)

                      (setf (macher-agent-context-tools ctx) '(tool-x))
                      (expect (macher-agent-context-tools ctx) :to-equal '(tool-x))

                      (setf (macher-agent-context-skills ctx) '(skill-y))
                      (expect (macher-agent-context-skills ctx) :to-equal '(skill-y))

                      (setf (macher-agent-context-media-queue ctx) '("pic.jpg"))
                      (expect (macher-agent-context-media-queue ctx) :to-equal '("pic.jpg"))

                      (setf (macher-agent-context-subagents ctx) '("sub-1" "sub-2"))
                      (expect (macher-agent-context-subagents ctx) :to-equal '("sub-1" "sub-2"))

                      (setf (macher-agent-context-plugins ctx) '(:plug "enabled"))
                      (expect (macher-agent-context-plugins ctx) :to-equal '(:plug "enabled")))
                  (when (buffer-live-p buf) (kill-buffer buf)))))

          (it "clones contexts using macher-agent--copy-context with slot isolation"
              (let* ((buf (get-buffer-create "test-copy-buf"))
                     (orig (macher-agent--make-context
                            :id "orig-id"
                            :project-root "/orig/proj/"
                            :origin-buffer buf
                            :tools '(orig-tool)
                            :skills '(orig-skill)
                            :media-queue '("orig.png")
                            :subagents '("sub-orig")
                            :plugins '(:flag t :value 100))))
                (unwind-protect
                    (let ((copy (macher-agent--copy-context orig)))
                      (expect (macher-agent-context-p copy) :to-be t)
                      (expect (eq orig copy) :to-be nil)
                      (expect (macher-agent-context-id copy) :to-equal "orig-id")
                      (expect (macher-agent-context-project-root copy) :to-equal "/orig/proj/")
                      (expect (macher-agent-context-origin-buffer copy) :to-be buf)
                      (expect (macher-agent-context-tools copy) :to-equal '(orig-tool))
                      (expect (macher-agent-context-skills copy) :to-equal '(orig-skill))
                      (expect (macher-agent-context-media-queue copy) :to-equal '("orig.png"))
                      (expect (macher-agent-context-subagents copy) :to-equal '("sub-orig"))
                      (expect (macher-agent-context-plugins copy) :to-equal '(:flag t :value 100))

                      ;; Verify mutating copy does not affect orig
                      (setf (macher-agent-context-id copy) "copy-id")
                      (setf (macher-agent-context-project-root copy) "/copy/proj/")
                      (setf (macher-agent-context-tools copy) '(copy-tool))
                      (expect (macher-agent-context-id orig) :to-equal "orig-id")
                      (expect (macher-agent-context-project-root orig) :to-equal "/orig/proj/")
                      (expect (macher-agent-context-tools orig) :to-equal '(orig-tool)))
                  (when (buffer-live-p buf) (kill-buffer buf))))))

(describe "15. Struct Prompt and Data Direct Accessors and Slot Mapping"
          (it "maps dedicated slots and arbitrary keys cleanly in macher-agent--get-context-data and macher-agent--set-context-data"
              (let ((ctx (macher-agent--make-context
                          :id "slot-id"
                          :project-root "/mock/slot-proj/"
                          :tools '(slot-tool)
                          :skills '(slot-skill)
                          :media-queue '("slot-media.png")
                          :subagents '("sub-1")
                          :plugins '(:custom-key "custom-val"))))
                ;; Read dedicated slots via get-context-data
                (expect (macher-agent--get-context-data ctx :id) :to-equal "slot-id")
                (expect (macher-agent--get-context-data ctx :project-root) :to-equal "/mock/slot-proj/")
                (expect (macher-agent--get-context-data ctx :tools) :to-equal '(slot-tool))
                (expect (macher-agent--get-context-data ctx :skills) :to-equal '(slot-skill))
                (expect (macher-agent--get-context-data ctx :media-queue) :to-equal '("slot-media.png"))
                (expect (macher-agent--get-context-data ctx :pending-media) :to-equal '("slot-media.png"))
                (expect (macher-agent--get-context-data ctx :subagents) :to-equal '("sub-1"))
                (expect (macher-agent--get-context-data ctx :custom-key) :to-equal "custom-val")
                (expect (macher-agent--get-context-data ctx :nonexistent "def") :to-equal "def")

                ;; Write dedicated slots via set-context-data
                (macher-agent--set-context-data ctx :id "new-slot-id")
                (expect (macher-agent-context-id ctx) :to-equal "new-slot-id")

                (macher-agent--set-context-data ctx :tools '(new-tool))
                (expect (macher-agent-context-tools ctx) :to-equal '(new-tool))

                (macher-agent--set-context-data ctx :media-queue '("new-media.png"))
                (expect (macher-agent-context-media-queue ctx) :to-equal '("new-media.png"))

                ;; Write arbitrary keys into plugins
                (macher-agent--set-context-data ctx :new-plugin-key "plugin-val")
                (expect (macher-agent--get-context-data ctx :new-plugin-key) :to-equal "plugin-val")
                (expect (plist-get (macher-agent-context-plugins ctx) :new-plugin-key) :to-equal "plugin-val")
                (expect (plist-get (macher-agent-context-plugins ctx) :custom-key) :to-equal "custom-val")))

          (it "safely accesses and synchronizes prompt on macher-agent-context"
              (let ((ctx (macher-agent--make-context :project-root "/mock/proj/" :plugins '(:prompt "init prompt"))))
                (expect (macher-agent--get-context-prompt ctx) :to-equal "init prompt")
                (expect (macher-agent-context-prompt ctx) :to-equal "init prompt")
                (macher-agent--set-context-prompt ctx "updated agent prompt")
                (expect (macher-agent--get-context-prompt ctx) :to-equal "updated agent prompt")
                (expect (macher-agent-context-prompt ctx) :to-equal "updated agent prompt")
                (expect (macher-agent--get-context-data ctx :prompt) :to-equal "updated agent prompt")
                (expect (plist-get (macher-agent-context-plugins ctx) :prompt) :to-equal "updated agent prompt")
                (setf (macher-agent-context-prompt ctx) "setf prompt")
                (expect (macher-agent-context-prompt ctx) :to-equal "setf prompt")
                (set-macher-agent-context-prompt ctx "setter prompt")
                (expect (macher-agent-context-prompt ctx) :to-equal "setter prompt")))

          (it "retrieves tagged workspace structure from macher-agent-context via macher-agent--get-context-workspace"
              (let ((ctx (macher-agent--make-context :project-root "/mock/proj/")))
                (expect (macher-agent--get-context-workspace ctx)
                        :to-equal (cons 'project (expand-file-name "/mock/proj/"))))))

(describe "16. Workspace Root Resolution, Context Lookup, and Context For Buffer"
          (it "resolves project root path strings purely from diverse workspace formats"
              (let ((path-str "/mock/proj/path/")
                    (proj-cons (cons 'project "/mock/proj/path/"))
                    (agent-cons (cons 'agent "/mock/proj/path/"))
                    (dir-cons (cons 'directory "/mock/proj/path/"))
                    (nested-proj (list (cons 'project "/mock/proj/path/")))
                    (nested-agent (list (cons 'agent "/mock/proj/path/")))
                    (nested-transient (cons 'project (cons 'transient "/mock/proj/path/")))
                    (list-transient (list (cons 'transient "/mock/proj/path/"))))
                (expect (macher-agent-workspace-project-root path-str)
                        :to-equal (file-truename (expand-file-name "/mock/proj/path/")))
                (expect (macher-agent-workspace-project-root proj-cons)
                        :to-equal (file-truename (expand-file-name "/mock/proj/path/")))
                (expect (macher-agent-workspace-project-root agent-cons)
                        :to-equal (file-truename (expand-file-name "/mock/proj/path/")))
                (expect (macher-agent-workspace-project-root dir-cons)
                        :to-equal (file-truename (expand-file-name "/mock/proj/path/")))
                (expect (macher-agent-workspace-project-root nested-proj)
                        :to-equal (file-truename (expand-file-name "/mock/proj/path/")))
                (expect (macher-agent-workspace-project-root nested-agent)
                        :to-equal (file-truename (expand-file-name "/mock/proj/path/")))
                (expect (macher-agent-workspace-project-root nested-transient)
                        :to-equal (file-truename (expand-file-name "/mock/proj/path/")))
                (expect (macher-agent-workspace-project-root list-transient)
                        :to-equal (file-truename (expand-file-name "/mock/proj/path/")))
                (cl-letf (((symbol-function 'project-current)
                           (lambda (&rest _) (cons 'transient "/mock/proj/path/")))
                          ((symbol-function 'project-root)
                           (lambda (p) (if (consp p) (cdr p) p))))
                  (expect (macher-agent-workspace-project-root (list "/mock/proj/path/"))
                          :to-equal (file-truename (expand-file-name "/mock/proj/path/"))))
                (expect (macher-agent-workspace-project-root nil) :to-be nil)))

          (it "retrieves display names and context roots based on project roots"
              (let ((ws-str "/mock/sample-proj")
                    (ctx-struct (macher-agent--make-context :project-root "/mock/sample-proj")))
                (expect (macher-agent--get-name ws-str) :to-equal "Agent: sample-proj")
                (expect (macher-agent--get-context-root ws-str)
                        :to-equal (file-truename (expand-file-name "/mock/sample-proj")))
                (expect (macher-agent--get-context-root ctx-struct)
                        :to-equal (file-truename (expand-file-name "/mock/sample-proj")))
                (expect (macher-agent-context-root ctx-struct)
                        :to-equal (file-truename (expand-file-name "/mock/sample-proj")))))

          (it "resolves contexts deterministically via macher-agent-context-lookup"
              (let* ((mock-dir (file-truename (expand-file-name "/mock/lookup-proj/")))
                     (sub-dir (file-truename (expand-file-name "/mock/lookup-proj/subdir/nested/")))
                     (ctx (macher-agent--make-context :project-root mock-dir))
                     (buf (get-buffer-create "test-lookup-buf")))
                (unwind-protect
                    (progn
                      ;; 1. Direct context resolution
                      (expect (macher-agent-context-lookup ctx) :to-be ctx)

                      ;; 2. Buffer-local persistent context resolution
                      (with-current-buffer buf
                        (setq-local macher-agent--persistent-context ctx))
                      (expect (macher-agent-context-lookup buf) :to-be ctx)

                      ;; 3. Workspace hash table lookup & ancestor climbing
                      (puthash (directory-file-name mock-dir) ctx macher-agent-active-workspaces)
                      (expect (macher-agent-context-lookup mock-dir) :to-be ctx)
                      (expect (macher-agent-context-lookup sub-dir) :to-be ctx)
                      (expect (macher-agent-context-lookup "/nonexistent/path/xyz") :to-be nil))
                  (remhash (directory-file-name mock-dir) macher-agent-active-workspaces)
                  (when (buffer-live-p buf) (kill-buffer buf)))))

          (it "builds tagged Agent-to-Agent transit payloads"
              (let ((valid-payload (macher-agent-make-a2a-payload
                                    :transit-type :root-to-subagent
                                    :task-id "task-core-001"
                                    :message "Execute instruction")))
                (expect (plist-get valid-payload :schema-version) :to-equal :a2a-v1)
                (expect (plist-get valid-payload :transit-type) :to-equal :root-to-subagent)
                (expect (plist-get valid-payload :task-id) :to-equal "task-core-001")
                (expect (plist-get valid-payload :message) :to-equal "Execute instruction")
                (let ((debug-on-error nil))
                  (expect (macher-agent-make-a2a-payload :transit-type :invalid-transit) :to-throw))))

          (it "registers, unregisters, and runs priority-ordered pipeline steps"
              (let ((pipeline-id 'test-core-pipeline)
                    (step1 (lambda (val) (concat val "->step1")))
                    (step2 (lambda (val) (concat val "->step2")))
                    (step3 (lambda (val) (concat val "->step3"))))
                (unwind-protect
                    (progn
                      (macher-agent-register-pipeline-step pipeline-id step2 20)
                      (macher-agent-register-pipeline-step pipeline-id step1 10)
                      (macher-agent-register-pipeline-step pipeline-id step3 30)

                      (expect (macher-agent-get-pipeline-steps pipeline-id)
                              :to-equal (list step1 step2 step3))

                      (expect (macher-agent-run-pipeline pipeline-id "init")
                              :to-equal "init->step1->step2->step3")

                      (macher-agent-unregister-pipeline-step pipeline-id step2)
                      (expect (macher-agent-get-pipeline-steps pipeline-id)
                              :to-equal (list step1 step3))
                      (expect (macher-agent-run-pipeline pipeline-id "init")
                              :to-equal "init->step1->step3"))
                  (remhash pipeline-id macher-agent-pipeline-registry))))

          (it "removes completed child entries from ownership and registry tables"
              (let ((parent-buf-name "test-parent-buf")
                    (child-buf (generate-new-buffer "test-child-buf")))
                (unwind-protect
                    (progn
                      (puthash parent-buf-name (list "test-child-buf" "other-child" 'sym-child) macher-agent--a2a-ownership)
                      (setq macher-agent-active-subagents (list "test-child-buf" "keep-me" 'sym-keep (cons "other" "/tmp")))
                      (macher-agent--remove-active-subagent-registries "test-child-buf" child-buf)
                      (expect (gethash parent-buf-name macher-agent--a2a-ownership) :to-equal '("other-child" sym-child))
                      (expect macher-agent-active-subagents :to-equal '("keep-me" sym-keep ("other" . "/tmp"))))
                  (remhash parent-buf-name macher-agent--a2a-ownership)
                  (when (buffer-live-p child-buf)
                    (kill-buffer child-buf))))))

(describe "17. gptel-fsm Generalized Variable (setf) Setters and Safe Mutators"
          (it "provides macher-agent--set-fsm-info and macher-agent--set-fsm-handlers safe wrappers"
              (let ((fsm (gptel-make-fsm :info '(:a 1) :handlers '((H1 . (f1))))))
                (expect (macher-agent--set-fsm-info fsm '(:a 2 :b 3)) :to-equal '(:a 2 :b 3))
                (expect (gptel-fsm-info fsm) :to-equal '(:a 2 :b 3))

                (expect (macher-agent--set-fsm-handlers fsm '((H2 . (f2)))) :to-equal '((H2 . (f2))))
                (expect (gptel-fsm-handlers fsm) :to-equal '((H2 . (f2))))

                ;; Gracefully handles nil FSM
                (expect (macher-agent--set-fsm-info nil '(:a 1)) :to-be nil)
                (expect (macher-agent--set-fsm-handlers nil '((H . (f)))) :to-be nil))))

(describe "18. Core Flush Hooks and VFS Independence"
          (it "verifies macher-agent-core.el defines task flush hook and runner without requiring macher-agent-vfs"
              (let* ((core-file (or (locate-library "macher-agent-core.el")
                                    (expand-file-name "macher-agent-core.el" default-directory)))
                     (core-content (with-temp-buffer
                                     (insert-file-contents core-file)
                                     (buffer-string))))
                ;; macher-agent-core.el has no (require 'macher-agent-vfs)
                (expect (string-match-p "(require 'macher-agent-vfs" core-content) :to-be nil)
                ;; Core defines task flush hook and runner
                (expect (boundp 'macher-agent-task-flush-hook) :to-be t)
                (expect (fboundp 'macher-agent-run-task-flush-hook) :to-be t)))

          (it "dispatches task flush hooks for 0-argument and 1-argument functions"
              (let* ((mock-ctx (macher-agent--make-context :project-root "/mock/core-flush/"))
                     (zero-arg-called nil)
                     (one-arg-called nil)
                     (zero-fn (lambda () (setq zero-arg-called t)))
                     (one-fn (lambda (ctx) (setq one-arg-called ctx))))
                (let ((macher-agent-task-flush-hook (list zero-fn one-fn)))
                  (macher-agent-run-task-flush-hook mock-ctx)
                  (expect zero-arg-called :to-be t)
                  (expect one-arg-called :to-equal mock-ctx))

                ;; Test with nil context
                (setq zero-arg-called nil
                      one-arg-called 'not-called)
                (let ((macher-agent-task-flush-hook (list zero-fn one-fn)))
                  (macher-agent-run-task-flush-hook nil)
                  (expect zero-arg-called :to-be t)
                  (expect one-arg-called :to-be nil)))))

(provide 'macher-agent-core-test)
;;; macher-agent-core-test.el ends here
