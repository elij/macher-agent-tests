;;; macher-agent-core-test.el --- Core behaviour tests for macher-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; This test suite enforces the specification for macher-agent,
;; focusing on primary agent context envelopes, lexical state management,
;; sandbox isolation, and diff splitting behaviours.

;;; Code:

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

(require 'subr-x)
(require 'buttercup)
(require 'cl-lib)
(require 'macher-agent-test-setup)
(require 'macher-agent)
(require 'macher-agent-macher nil t)
(require 'macher-agent-vfs)

(defvar gptel--fsm)
(defvar macher-agent--active-fsm)
(defvar gptel--fsm-last)
(defvar sandbox-root)

(describe "1. VFS and Optimistic Concurrency"
          (after-each
           (setq macher-agent--pause-auto-sync nil))

          (it "asserts that a VFS write warns if the underlying file has drifted"
              (let* ((ctx (make-macher-agent-context :project-root "/mock/proj/"))
                     (file-path "/mock/proj/test.el")
                     (original-mtime '(25000 12345))
                     (drifted-mtime '(25000 99999)))
                (unwind-protect
                    (progn
                      (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                      (puthash file-path original-mtime (macher-agent-workspace-mtime-tracker ctx))

                      (spy-on 'file-attributes :and-call-fake
                              (lambda (&rest args)
                                (let ((file (car args)))
                                  (if (string= file file-path)
                                      (list t 1 1 1 drifted-mtime drifted-mtime drifted-mtime 100 "mode" t 1 1)
                                    nil))))

                      (spy-on 'display-warning)

                      (macher-agent-vfs-write (macher-agent-workspace-vfs-buffers ctx)
                                              (macher-agent-workspace-mtime-tracker ctx)
                                              file-path
                                              "New content")

                      (expect 'display-warning :to-have-been-called-with
                              'macher-agent
                              "Your previous edits to test.el were discarded due to external file modifications.  Please re-read and re-apply"
                              :warning))
                  (remhash (expand-file-name "/mock/proj/") macher-agent-active-workspaces)))))

(describe "2. Execution Environments (Sandbox)"
          (it "asserts that sandbox inflation overlays the uncommitted VFS changes"
              (let* ((ctx (make-macher-agent-context :project-root "/mock/proj/")))
                (setf (macher-agent-context-plugins ctx) (list :sandbox-path "/tmp/macher-sandbox/"))
                (unwind-protect
                    (progn
                      (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                      (puthash "/mock/proj/overlay.el" "VFS Overlay Content" (macher-agent-workspace-vfs-buffers ctx))
                      
                      (let ((written-to-sandbox nil))
                        (spy-on 'file-directory-p :and-return-value t)
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
                        
                        (macher-agent-vfs-scratch-inflate "/tmp/macher-sandbox/" (macher-agent-workspace-vfs-buffers ctx) (macher-agent-context-root ctx) (macher-agent--get-context-contents ctx))
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
                (setf (macher-agent-context-plugins ctx)
                      (list :vfs-buffers vfs-ht :mtime-tracker mtime-ht))

                (let* ((cloned-ctx (macher-agent--copy-context ctx))
                       (cloned-vfs (copy-hash-table (plist-get (macher-agent-context-plugins ctx) :vfs-buffers)))
                       (cloned-mtime (copy-hash-table (plist-get (macher-agent-context-plugins ctx) :mtime-tracker))))
                  (setf (macher-agent-context-plugins cloned-ctx)
                        (list :vfs-buffers cloned-vfs :mtime-tracker cloned-mtime))

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
                (setf (macher-agent-context-plugins ctx) (list :pending-media "mockbase64"))

                (spy-on 'gptel--inject-media :and-return-value nil)
                (spy-on 'gptel--inject-prompt :and-return-value nil)

                (macher-agent--inject-media-fsm-logic fsm)

                (expect 'gptel--inject-media :to-have-been-called)
                (expect 'gptel--inject-prompt :to-have-been-called)
                (expect (plist-get (macher-agent-context-plugins ctx) :pending-media) :to-be nil)))

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
                (expect (plist-get (macher-agent-context-plugins ctx) :pending-media) :to-be nil))))

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
              (let* ((callback-result nil)
                     (sub-buf (get-buffer-create "sub-agent-buf"))
                     (payloads (list (macher-agent-make-a2a-payload
                                      :type 'SEND_MESSAGE
                                      :task-id "task-001"
                                      :payload "Do something"
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
                (let ((macher-agent-max-context-chars '((nil . 25)))
                      (state (make-macher-agent-transmission-state :target-buffer (current-buffer))))
                  (macher-agent-memory-pipe--truncate-buffer state))
                (expect (buffer-string) :to-match "^---\nkey: value\n---")
                (expect (buffer-string) :to-match "Latest user query content")))

          (it "triggers pending media injection on FSM updates"
              (let* ((ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (fsm (gptel-make-fsm)))
                (setf (gptel-fsm-info fsm) (list :macher-agent-context ctx))
                (setf (macher-agent-context-plugins ctx) (list :pending-media (list "data")))
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
                      (expect (macher-agent--resolve-buffer-name mock-file) :to-equal "/mock/proj/dummy-file.el")
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
                        (macher-agent-ui-show buf)
                        (expect displayed-buf :to-be buf)))
                  (kill-buffer buf)))))

(describe "10. Context Resolution and Typed Contracts"
          (it "extracts persistent context from live buffer objects and buffer names via macher-agent-context-from-buffer"
              (let ((buf (get-buffer-create "test-context-from-buf"))
                    (ctx (macher-agent--make-context :id "buf-ctx")))
                (unwind-protect
                    (progn
                      (with-current-buffer buf
                        (setq-local macher-agent--persistent-context ctx))
                      (expect (macher-agent-context-from-buffer buf) :to-be ctx)
                      (expect (macher-agent-context-from-buffer "test-context-from-buf") :to-be ctx))
                  (when (buffer-live-p buf) (kill-buffer buf)))))

          (it "returns nil from macher-agent-context-from-buffer for dead buffers or unset context"
              (let ((buf (get-buffer-create "test-context-dead-buf"))
                    (ctx (macher-agent--make-context :id "dead-buf-ctx")))
                (with-current-buffer buf
                  (setq-local macher-agent--persistent-context ctx))
                (kill-buffer buf)
                (expect (macher-agent-context-from-buffer buf) :to-be nil)
                (expect (macher-agent-context-from-buffer "non-existent-buf-xyz") :to-be nil)
                (with-temp-buffer
                  (expect (macher-agent-context-from-buffer (current-buffer)) :to-be nil))))

          (it "extracts context directly or from transit payload slots via macher-agent-context-from-payload"
              (let* ((ctx (macher-agent--make-context :id "direct-ctx"))
                     (buf (get-buffer-create "test-payload-target-buf"))
                     (target-ctx (macher-agent--make-context :id "target-ctx"))
                     (parent-ctx (macher-agent--make-context :id "parent-ctx"))
                     (child-ctx (macher-agent--make-context :id "child-ctx"))
                     (shared-ctx (macher-agent--make-context :id "shared-ctx"))
                     (buf-ctx (macher-agent--make-context :id "buf-ctx")))
                (unwind-protect
                    (progn
                      (with-current-buffer buf
                        (setq-local macher-agent--persistent-context buf-ctx))
                      ;; 1. Target context in payload
                      (expect (macher-agent-context-from-payload
                               (make-macher-agent-transit-payload :target-context target-ctx))
                              :to-be target-ctx)
                      ;; 2. Parent context in payload
                      (expect (macher-agent-context-from-payload
                               (make-macher-agent-transit-payload :parent-context parent-ctx))
                              :to-be parent-ctx)
                      ;; 3. Child context in payload
                      (expect (macher-agent-context-from-payload
                               (make-macher-agent-transit-payload :child-context child-ctx))
                              :to-be child-ctx)
                      ;; 4. Shared state plist
                      (expect (macher-agent-context-from-payload
                               (make-macher-agent-transit-payload :shared-state (list :target-context shared-ctx)))
                              :to-be shared-ctx))
                  (when (buffer-live-p buf) (kill-buffer buf)))))

          (it "signals an error on invalid or unresolvable payloads in macher-agent-context-from-payload"
              (expect (macher-agent-context-from-payload (make-macher-agent-transit-payload)) :to-be nil)
              (expect (macher-agent-context-from-payload "invalid-payload") :to-throw 'wrong-type-argument)
              (expect (macher-agent-context-from-payload nil) :to-throw 'wrong-type-argument))

          (it "verifies strict isolation: polymorphic probing and FSM extract functions are removed"
              (expect (fboundp 'macher-agent-resolve-context) :to-be nil)
              (expect (fboundp 'macher-agent--extract-fsm-context) :to-be nil)
              (expect (fboundp 'macher-agent--inject-context-into-fsm-info) :to-be nil)
              (expect (fboundp 'macher-agent-ctx-pipe--fsm) :to-be nil))

          (it "injects originating buffer context into active-fsm binding when available"
              (let* ((origin-buf (get-buffer-create "test-origin-buf"))
                     (mock-ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (fsm (gptel-make-fsm :info (list :buffer origin-buf :model "test-model")))
                     (called nil))
                (unwind-protect
                    (progn
                      (with-current-buffer origin-buf
                        (setq-local macher-agent--persistent-context mock-ctx))
                      (macher-agent--transform-inject-context fsm (lambda () (setq called t)))
                      (expect called :to-be t)
                      (with-current-buffer origin-buf
                        (expect (bound-and-true-p macher-agent--active-fsm) :to-be fsm))
                      (expect (plist-get (gptel-fsm-info fsm) :model) :to-equal "test-model"))
                  (kill-buffer origin-buf))))

          (it "binds macher-agent--active-fsm locally even when parent fsm is let-bound"
              (let* ((origin-buf (get-buffer-create "test-origin-buf-letbound"))
                     (mock-ctx (macher-agent--make-context :project-root "/mock/proj/"))
                     (fsm (gptel-make-fsm :info (list :buffer origin-buf :model "test-model")))
                     (parent-fsm (gptel-make-fsm :info (list :model "parent-model")))
                     (called nil))
                (unwind-protect
                    (progn
                      (with-current-buffer origin-buf
                        (setq-local macher-agent--persistent-context mock-ctx))
                      (let ((macher-agent--active-fsm parent-fsm))
                        (macher-agent--transform-inject-context fsm (lambda () (setq called t))))
                      (expect called :to-be t)
                      (with-current-buffer origin-buf
                        (expect (bound-and-true-p macher-agent--active-fsm) :to-be fsm)))
                  (kill-buffer origin-buf))))

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

                      (with-temp-buffer
                        (expect (macher-agent-gptel-context-from-fsm nil) :to-be nil))
                      (expect (macher-agent-gptel-context-from-fsm fsm1) :to-be mock-ctx)
                      (expect (macher-agent-gptel-context-from-fsm fsm2) :to-be mock-ctx)

                      (with-current-buffer target-buf
                        (setq-local macher-agent--persistent-context mock-ctx))
                      (expect (macher-agent-gptel-context-from-fsm fsm3) :to-be mock-ctx))
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
              (expect (macher-agent-extract-workspace-id '(:workspace-id "/tmp/plist-proj")) :to-equal "/tmp/plist-proj")
              (expect (macher-agent-extract-workspace-id '(:workspace "/tmp/ws-proj")) :to-be nil)
              (expect (macher-agent-extract-workspace-id nil) :to-be nil)
              (expect (macher-agent-extract-workspace-id "/tmp/str-proj") :to-throw 'wrong-type-argument))

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
                (expect (macher-agent-root default-directory) :to-equal (expand-file-name "/tmp/test-project/"))
                (expect (macher-agent-root "/tmp/custom-path/") :to-equal (expand-file-name "/tmp/custom-path/"))))

          (it "extracts raw tool names and coerces to canonical string names"
              (expect (macher-agent--extract-raw-tool-name "tool_str") :to-equal "tool_str")
              (expect (macher-agent--extract-raw-tool-name 'tool_sym) :to-equal "tool_sym")
              (expect (macher-agent--extract-raw-tool-name '(:name "tool_plist")) :to-equal "tool_plist")
              (expect (macher-agent-canonical-tool-name "my_tool") :to-equal "my_tool")
              (expect (macher-agent-canonical-tool-name 'my_tool) :to-equal "my_tool")
              (expect (macher-agent-canonical-tool-name '(:name "my_tool")) :to-equal "my_tool")
              (expect (macher-agent-canonical-tool-name nil) :to-be nil)))

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
                ;; 4. gptel--fsm-last is ignored (no legacy lookup)
                (let ((macher-agent--active-fsm nil)
                      (gptel--fsm nil)
                      (gptel--fsm-last fsm-last))
                  (expect (macher-agent-get-active-fsm nil) :to-be nil))
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
                                :prompt "test prompt"
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
                      (expect (macher-agent-context-prompt ctx-default) :to-be nil)
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
                      (expect (macher-agent-context-prompt ctx) :to-equal "test prompt")
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

                      (setf (macher-agent-context-prompt ctx) "updated prompt")
                      (expect (macher-agent-context-prompt ctx) :to-equal "updated prompt")

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
                            :prompt "orig prompt"
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
                      (expect (macher-agent-context-prompt copy) :to-equal "orig prompt")
                      (expect (macher-agent-context-tools copy) :to-equal '(orig-tool))
                      (expect (macher-agent-context-skills copy) :to-equal '(orig-skill))
                      (expect (macher-agent-context-media-queue copy) :to-equal '("orig.png"))
                      (expect (macher-agent-context-subagents copy) :to-equal '("sub-orig"))
                      (expect (macher-agent-context-plugins copy) :to-equal '(:flag t :value 100))

                      ;; Verify mutating copy does not affect orig
                      (setf (macher-agent-context-id copy) "copy-id")
                      (setf (macher-agent-context-project-root copy) "/copy/proj/")
                      (setf (macher-agent-context-prompt copy) "copy prompt")
                      (setf (macher-agent-context-tools copy) '(copy-tool))
                      (expect (macher-agent-context-id orig) :to-equal "orig-id")
                      (expect (macher-agent-context-project-root orig) :to-equal "/orig/proj/")
                      (expect (macher-agent-context-prompt orig) :to-equal "orig prompt")
                      (expect (macher-agent-context-tools orig) :to-equal '(orig-tool)))
                  (when (buffer-live-p buf) (kill-buffer buf))))))

(describe "15. Struct Direct Accessors and Prompt Operations"
          (it "accesses dedicated slots and plugin properties directly on macher-agent-context"
              (let ((ctx (macher-agent--make-context
                          :id "slot-id"
                          :project-root "/mock/slot-proj/"
                          :tools '(slot-tool)
                          :skills '(slot-skill)
                          :media-queue '("slot-media.png")
                          :subagents '("sub-1")
                          :plugins '(:custom-key "custom-val"))))
                ;; Read dedicated slots directly
                (expect (macher-agent-context-id ctx) :to-equal "slot-id")
                (expect (macher-agent-context-project-root ctx) :to-equal "/mock/slot-proj/")
                (expect (macher-agent-context-tools ctx) :to-equal '(slot-tool))
                (expect (macher-agent-context-skills ctx) :to-equal '(slot-skill))
                (expect (macher-agent-context-media-queue ctx) :to-equal '("slot-media.png"))
                (expect (macher-agent-context-subagents ctx) :to-equal '("sub-1"))
                (expect (plist-get (macher-agent-context-plugins ctx) :custom-key) :to-equal "custom-val")

                ;; Write dedicated slots directly
                (setf (macher-agent-context-id ctx) "new-slot-id")
                (expect (macher-agent-context-id ctx) :to-equal "new-slot-id")

                (setf (macher-agent-context-tools ctx) '(new-tool))
                (expect (macher-agent-context-tools ctx) :to-equal '(new-tool))

                (setf (macher-agent-context-media-queue ctx) '("new-media.png"))
                (expect (macher-agent-context-media-queue ctx) :to-equal '("new-media.png"))

                ;; Write plugin properties directly
                (setf (macher-agent-context-plugins ctx)
                      (plist-put (copy-sequence (macher-agent-context-plugins ctx)) :new-plugin-key "plugin-val"))
                (expect (plist-get (macher-agent-context-plugins ctx) :new-plugin-key) :to-equal "plugin-val")
                (expect (plist-get (macher-agent-context-plugins ctx) :custom-key) :to-equal "custom-val")))

          (it "safely accesses and mutates prompt on macher-agent-context"
              (let ((ctx (macher-agent--make-context :project-root "/mock/proj/" :prompt "init prompt")))
                (expect (macher-agent-context-prompt ctx) :to-equal "init prompt")
                (set-macher-agent-context-prompt ctx "updated agent prompt")
                (expect (macher-agent-context-prompt ctx) :to-equal "updated agent prompt")
                (setf (macher-agent-context-prompt ctx) "setf prompt")
                (expect (macher-agent-context-prompt ctx) :to-equal "setf prompt"))))

(describe "16. Workspace Root Resolution, Context Lookup, and Context For Buffer"
          (it "resolves project root path strings purely from diverse workspace formats"
              (let ((ctx (make-macher-agent-context :project-root "/mock/proj/path/")))
                (expect (macher-agent-workspace-project-root ctx)
                        :to-equal "/mock/proj/path/")
                (expect (macher-agent-workspace-project-root "/mock/proj/path/")
                        :to-throw 'wrong-type-argument)
                (expect (macher-agent-workspace-project-root nil)
                        :to-throw 'wrong-type-argument)))

          (it "retrieves display names and context roots based on project roots"
              (let ((ws-str "/mock/sample-proj")
                    (ctx-struct (macher-agent--make-context :project-root "/mock/sample-proj")))
                (expect (macher-agent--get-name ws-str) :to-equal "Agent: Workspace")
                (expect (macher-agent--get-name ctx-struct) :to-equal "Agent: sample-proj")
                (expect (macher-agent--get-context-root ws-str) :to-throw 'wrong-type-argument)
                (expect (macher-agent--get-context-root ctx-struct)
                        :to-equal (file-truename (expand-file-name "/mock/sample-proj")))
                (expect (macher-agent-context-root ctx-struct)
                        :to-equal (file-truename (expand-file-name "/mock/sample-proj")))))

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

          (it "dispatches task flush hooks with context to hook functions"
              (let* ((mock-ctx (macher-agent--make-context :project-root "/mock/core-flush/"))
                     (zero-arg-called nil)
                     (one-arg-called nil)
                     (fn1 (lambda (_ctx) (setq zero-arg-called t)))
                     (fn2 (lambda (ctx) (setq one-arg-called ctx))))
                (let ((macher-agent-task-flush-hook (list fn1 fn2)))
                  (macher-agent-run-task-flush-hook mock-ctx)
                  (expect zero-arg-called :to-be t)
                  (expect one-arg-called :to-equal mock-ctx))

                ;; Test with nil context
                (setq zero-arg-called nil
                      one-arg-called 'not-called)
                (let ((macher-agent-task-flush-hook (list fn1 fn2)))
                  (macher-agent-run-task-flush-hook nil)
                  (expect zero-arg-called :to-be t)
                  (expect one-arg-called :to-be nil)))))

(describe "19. Core cl-defstruct Contracts and Accessors"
          (it "constructs, inspects, and copies macher-agent-vfs-entry structs"
              (let ((entry (make-macher-agent-vfs-entry :path "foo.el" :orig "orig-text" :curr "curr-text")))
                (expect (macher-agent-vfs-entry-p entry) :to-be t)
                (expect (macher-agent-vfs-entry-path entry) :to-equal "foo.el")
                (expect (macher-agent-vfs-entry-orig entry) :to-equal "orig-text")
                (expect (macher-agent-vfs-entry-curr entry) :to-equal "curr-text")
                (let ((cloned (copy-macher-agent-vfs-entry entry)))
                  (expect (macher-agent-vfs-entry-p cloned) :to-be t)
                  (expect (macher-agent-vfs-entry-path cloned) :to-equal "foo.el")
                  (setf (macher-agent-vfs-entry-curr cloned) "mutated")
                  (expect (macher-agent-vfs-entry-curr cloned) :to-equal "mutated")
                  (expect (macher-agent-vfs-entry-curr entry) :to-equal "curr-text"))))

          (it "constructs, inspects, and copies macher-agent-transit-payload structs"
              (let ((payload (make-macher-agent-transit-payload
                              :schema-version :a2a-v1
                              :transit-type :subagent-to-parent
                              :type 'ARTIFACT_UPDATE
                              :task-id "task-struct-01"
                              :target-buffer "*target*"
                              :target-context 'mock-ctx
                              :parent-context 'mock-parent
                              :child-context 'mock-child
                              :shared-state '(:state "active")
                              :payload "payload data"
                              :metadata '(:meta "data"))))
                (expect (macher-agent-transit-payload-p payload) :to-be t)
                (expect (macher-agent-transit-payload-schema-version payload) :to-equal :a2a-v1)
                (expect (macher-agent-transit-payload-transit-type payload) :to-equal :subagent-to-parent)
                (expect (macher-agent-transit-payload-type payload) :to-equal 'ARTIFACT_UPDATE)
                (expect (macher-agent-transit-payload-task-id payload) :to-equal "task-struct-01")
                (expect (macher-agent-transit-payload-target-buffer payload) :to-equal "*target*")
                (expect (macher-agent-transit-payload-target-context payload) :to-equal 'mock-ctx)
                (expect (macher-agent-transit-payload-parent-context payload) :to-equal 'mock-parent)
                (expect (macher-agent-transit-payload-child-context payload) :to-equal 'mock-child)
                (expect (macher-agent-transit-payload-shared-state payload) :to-equal '(:state "active"))
                (expect (macher-agent-transit-payload-payload payload) :to-equal "payload data")
                (expect (macher-agent-transit-payload-metadata payload) :to-equal '(:meta "data"))))

          (it "constructs, inspects, and copies macher-agent-tool-call structs"
              (let ((tc (make-macher-agent-tool-call
                         :name 'read_file
                         :args '(:path "main.py")
                         :status 'success
                         :result "content of main.py")))
                (expect (macher-agent-tool-call-p tc) :to-be t)
                (expect (macher-agent-tool-call-name tc) :to-equal 'read_file)
                (expect (macher-agent-tool-call-args tc) :to-equal '(:path "main.py"))
                (expect (macher-agent-tool-call-status tc) :to-equal 'success)
                (expect (macher-agent-tool-call-result tc) :to-equal "content of main.py")))

          (it "constructs, inspects, and copies macher-agent-sandbox-state structs"
              (let ((sb (make-macher-agent-sandbox-state
                         :env '((x . 10) (y . 20))
                         :is-star t
                         :interrupt 'tool-call)))
                (expect (macher-agent-sandbox-state-p sb) :to-be t)
                (expect (macher-agent-sandbox-state-env sb) :to-equal '((x . 10) (y . 20)))
                (expect (macher-agent-sandbox-state-is-star sb) :to-be t)
                (expect (macher-agent-sandbox-state-interrupt sb) :to-equal 'tool-call)))

          (it "constructs, inspects, and copies macher-agent-task-context structs in core"
              (let ((tc (make-macher-agent-task-context
                         :workspace "/mock/task-ws"
                         :target-buffer (current-buffer)
                         :skill-sym 'test-skill
                         :system-message "Test system prompt")))
                (expect (macher-agent-task-context-p tc) :to-be t)
                (expect (macher-agent-task-context-workspace tc) :to-equal "/mock/task-ws")
                (expect (macher-agent-task-context-target-buffer tc) :to-equal (current-buffer))
                (expect (macher-agent-task-context-skill-sym tc) :to-equal 'test-skill)
                (expect (macher-agent-task-context-system-message tc) :to-equal "Test system prompt")
                (let ((cloned (copy-macher-agent-task-context tc)))
                  (expect (macher-agent-task-context-p cloned) :to-be t)
                  (expect (macher-agent-task-context-workspace cloned) :to-equal "/mock/task-ws")
                  (expect (macher-agent-task-context-skill-sym cloned) :to-equal 'test-skill)))))

(describe "20. Dual Virtual File System Instances"
          (it "partitions mixed context entries into separate physical and buffer VFS instances"
              (let* ((mock-dir (expand-file-name "/mock/dual-vfs-proj/"))
                     (buf (get-buffer-create "*dual-vfs-virtual-buf*"))
                     (e-file (make-macher-agent-vfs-entry :path (expand-file-name "main.el" mock-dir)
                                                          :orig "file-orig" :curr "file-curr"))
                     (e-buf (make-macher-agent-vfs-entry :path "*dual-vfs-virtual-buf*"
                                                         :orig "buf-orig" :curr "buf-curr"))
                     (ctx (macher-agent--make-context :project-root mock-dir)))
                (unwind-protect
                    (progn
                      (macher-agent--set-context-contents ctx (list e-file e-buf))
                      (let* ((partitioned (macher-agent--partition-vfs-entries
                                           (macher-agent--get-context-contents ctx) mock-dir))
                             (virtual-list (car partitioned))
                             (physical-list (cdr partitioned)))
                        ;; Non-file-backed buffer partition
                        (expect (length virtual-list) :to-equal 1)
                        (expect (macher-agent-vfs-entry-path (car virtual-list)) :to-equal "*dual-vfs-virtual-buf*")
                        ;; Physical file partition
                        (expect (length physical-list) :to-equal 1)
                        (expect (macher-agent-vfs-entry-path (car physical-list)) :to-equal (expand-file-name "main.el" mock-dir)))
                      ;; Splitting context produces dual distinct instances
                      (let* ((split (macher-agent--split-context ctx))
                             (file-ctx (car split))
                             (buf-ctx (cdr split)))
                        (expect (macher-agent-context-p file-ctx) :to-be t)
                        (expect (macher-agent-context-p buf-ctx) :to-be t)
                        (expect (eq file-ctx buf-ctx) :to-be nil)
                        (expect (length (macher-agent--get-context-contents file-ctx)) :to-equal 1)
                        (expect (length (macher-agent--get-context-contents buf-ctx)) :to-equal 1)
                        (expect (macher-agent-vfs-entry-path (car (macher-agent--get-context-contents file-ctx)))
                                :to-equal (expand-file-name "main.el" mock-dir))
                        (expect (macher-agent-vfs-entry-path (car (macher-agent--get-context-contents buf-ctx)))
                                :to-equal "*dual-vfs-virtual-buf*")))
                  (when (buffer-live-p buf) (kill-buffer buf)))))

          (it "deep-copies hash tables in macher-agent--split-context so split contexts do not mutate each other"
              (let* ((vfs-ht (make-hash-table :test 'equal))
                     (mtime-ht (make-hash-table :test 'equal))
                     (ctx (macher-agent--make-context :project-root "/mock/deep-copy-proj/")))
                (puthash "key1" "val1" vfs-ht)
                (puthash "key1" '(100 200) mtime-ht)
                (setf (macher-agent-context-plugins ctx)
                      (list :vfs-buffers vfs-ht :mtime-tracker mtime-ht))
                (let* ((split (macher-agent--split-context ctx))
                       (file-ctx (car split))
                       (buf-ctx (cdr split))
                       (file-vfs (plist-get (macher-agent-context-plugins file-ctx) :vfs-buffers))
                       (buf-vfs (plist-get (macher-agent-context-plugins buf-ctx) :vfs-buffers))
                       (file-mtime (plist-get (macher-agent-context-plugins file-ctx) :mtime-tracker))
                       (buf-mtime (plist-get (macher-agent-context-plugins buf-ctx) :mtime-tracker)))
                  ;; Ensure hash tables are distinct objects
                  (expect (eq file-vfs buf-vfs) :to-be nil)
                  (expect (eq file-vfs vfs-ht) :to-be nil)
                  (expect (eq buf-vfs vfs-ht) :to-be nil)
                  (expect (eq file-mtime buf-mtime) :to-be nil)
                  (expect (eq file-mtime mtime-ht) :to-be nil)
                  ;; Mutating file-vfs does not affect buf-vfs or original
                  (puthash "file-only" "file-val" file-vfs)
                  (expect (gethash "file-only" file-vfs) :to-equal "file-val")
                  (expect (gethash "file-only" buf-vfs) :to-be nil)
                  (expect (gethash "file-only" vfs-ht) :to-be nil)))))

(provide 'macher-agent-core-test)
;;; macher-agent-core-test.el ends here
