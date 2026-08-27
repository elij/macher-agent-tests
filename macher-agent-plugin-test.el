;;; macher-agent-plugin-test.el --- Decoupled Plugin Model Tests -*- lexical-binding: t; -*-

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
(require 'macher-agent-core)
(require 'macher-agent-vfs)
(require 'macher-agent-presets)
(require 'macher-agent-sandbox)
(require 'macher-agent-gptel)
(require 'macher-agent-zero-mem)
(require 'macher-agent-orchestration)

(describe "Decoupled Plugin Model Architecture"
          (macher-agent-test-setup-before-each)
          (before-each
           (macher-agent-install))
          (after-each
           (when (fboundp 'macher-agent-zero-mem-uninstall)
             (macher-agent-zero-mem-uninstall))
           (setq macher-agent-search-backend-function #'macher-agent-search-glob))
          (after-all
           (when (fboundp 'macher-agent-zero-mem-uninstall)
             (macher-agent-zero-mem-uninstall))
           (setq macher-agent-search-backend-function #'macher-agent-search-glob))

          (describe "1. Core Routing and Pipeline Registry"
                    (it "initialises macher-agent-pipeline-registry as a hash table"
                        (expect (hash-table-p macher-agent-pipeline-registry) :to-be t))

                    (it "registers pipeline steps in strict priority depth order and deduplicates"
                        (let ((pipeline-name (make-symbol "test-pipeline-priority")))
                          (macher-agent-register-pipeline-step pipeline-name #'ignore 90)
                          (macher-agent-register-pipeline-step pipeline-name #'identity 10)
                          (macher-agent-register-pipeline-step pipeline-name #'car 50)
                          (expect (macher-agent-get-pipeline-steps pipeline-name)
                                  :to-equal (list #'identity #'car #'ignore))
                          (macher-agent-register-pipeline-step pipeline-name #'ignore 20)
                          (expect (macher-agent-get-pipeline-steps pipeline-name)
                                  :to-equal (list #'identity #'ignore #'car))))

                    (it "stores symbol keys exclusively and avoids duplicate string keys in pipeline registry"
                        (let ((sym-pipe (intern "test-sym-pipeline")))
                          (macher-agent-register-pipeline-step (symbol-name sym-pipe) #'identity 25)
                          (expect (gethash sym-pipe macher-agent-pipeline-registry) :not :to-be nil)
                          (expect (gethash (symbol-name sym-pipe) macher-agent-pipeline-registry) :to-be nil)))

                    (it "bridges user interface buffers to state machine with buffer-local macher-agent-fsm-id"
                        (with-temp-buffer
                          (setq-local macher-agent-fsm-id "fsm-session-12345")
                          (expect (local-variable-p 'macher-agent-fsm-id) :to-be t)
                          (expect macher-agent-fsm-id :to-equal "fsm-session-12345"))
                        (with-temp-buffer
                          (expect macher-agent-fsm-id :to-be nil)))

                    (it "resolves callback collisions in dispatch using macher-agent--generate-uuid"
                        (let* ((existing-id "task-collision-id")
                               (cb-called nil)
                               (dispatched-id nil))
                          (puthash existing-id (lambda (_msg) (setq cb-called t)) macher-agent--pending-callbacks)
                          (spy-on 'macher-agent-a2a-pipe--validate-routing :and-call-fake
                                  (lambda (state)
                                    (let ((msg (plist-get state :a2a-msg)))
                                      (setq dispatched-id (plist-get msg :task-id)))
                                    state))
                          (macher-agent-a2a-dispatch
                           (list (list :type 'USER_DIRECTIVE
                                       :task-id existing-id
                                       :message "Test message"
                                       :metadata nil))
                           nil)
                          (expect dispatched-id :not :to-equal existing-id)
                          (expect (string-prefix-p "task-" dispatched-id) :to-be t)
                          (remhash existing-id macher-agent--pending-callbacks)))

                    (it "resolves callback collisions in bind closure step using macher-agent--generate-uuid"
                        (let* ((colliding-id "bind-collision-id")
                               (initial-state (list :a2a-msg (list :task-id colliding-id :metadata nil)
                                                    :shared-state (list :results (make-hash-table :test 'equal)
                                                                        :total 1
                                                                        :final-callback nil
                                                                        :parent-buffer (current-buffer)
                                                                        :parent-fsm nil
                                                                        :original-payloads nil)
                                                    :child-buf (generate-new-buffer "test-bind-child"))))
                          (puthash colliding-id #'ignore macher-agent--pending-callbacks)
                          (unwind-protect
                              (let ((res-state (macher-agent-a2a-pipe--bind-closure initial-state)))
                                (let ((assigned-id (plist-get (plist-get res-state :a2a-msg) :task-id)))
                                  (expect assigned-id :not :to-equal colliding-id)
                                  (expect (gethash assigned-id macher-agent--pending-callbacks) :not :to-be nil)))
                            (remhash colliding-id macher-agent--pending-callbacks)
                            (kill-buffer (plist-get initial-state :child-buf)))))

                    (it "extracts parent-buffer from plist shared state in macher-agent-a2a-pipe--acquire-target"
                        (let* ((mock-dir (make-temp-file "macher-a2a-acquire-target-test" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (parent-ctx (macher-agent--make-vfs-context :workspace workspace :contents nil))
                               (parent-buf (generate-new-buffer "test-a2a-parent-buf"))
                               (child-buf (generate-new-buffer "test-a2a-child-buf")))
                          (unwind-protect
                              (progn
                                (with-current-buffer parent-buf
                                  (setq-local macher-agent--persistent-context parent-ctx))
                                (let* ((state-plist (list :a2a-msg (list :task-id "task-p1" :metadata (list :buffer_name "test-a2a-child-buf"))
                                                          :target "test-a2a-child-buf"
                                                          :target-name "test-a2a-child-buf"
                                                          :shared-state (list :parent-buffer parent-buf)))
                                       (res-plist (macher-agent-a2a-pipe--acquire-target state-plist)))
                                  (expect (plist-get res-plist :child-buf) :to-be child-buf)
                                  (with-current-buffer child-buf
                                    (expect (macher-agent-valid-context-p macher-agent--persistent-context) :to-be-truthy)
                                    (expect (macher-agent--get-context-workspace macher-agent--persistent-context) :to-equal workspace))))
                            (kill-buffer parent-buf)
                            (kill-buffer child-buf)
                            (delete-directory mock-dir t)))))

          (describe "2. Storage and Virtual File System"
                    (it "executes within Virtual File System awareness scope using macher-agent-with-vfs-scope"
                        (let* ((mock-dir (make-temp-file "macher-vfs-scope-test" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (ctx (macher-agent--make-vfs-context :workspace workspace :contents nil))
                               (executed-dir nil)
                               (executed-ctx nil)
                               (eval-count 0)
                               (getter (lambda ()
                                         (setq eval-count (1+ eval-count))
                                         ctx)))
                          (unwind-protect
                              (progn
                                (macher-agent-with-vfs-scope ctx
                                  (setq executed-dir default-directory)
                                  (setq executed-ctx macher-agent--persistent-context))
                                (expect (file-name-as-directory (file-truename executed-dir)) :to-equal (file-name-as-directory (file-truename mock-dir)))
                                (expect executed-ctx :to-be ctx)
                                (macher-agent-with-vfs-scope (funcall getter)
                                  (expect default-directory :not :to-be nil))
                                (expect eval-count :to-equal 1))
                            (delete-directory mock-dir t))))

                    (it "merges payload diffs, handles deletions, and extracts context via macher-agent-vfs--merge-payload"
                        (let* ((mock-dir (make-temp-file "macher-vfs-merge-test" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (ctx (macher-agent--make-vfs-context :workspace workspace :contents nil))
                               (ambient-ctx (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/unrelated") :contents nil))
                               (target-buf (generate-new-buffer "*macher-vfs-merge-target*")))
                          (unwind-protect
                              (progn
                                ;; 1. Merge diffs
                                (let* ((state (list :status 'initial
                                                    :context ctx
                                                    :diff (list (cons "file1.txt" (cons "old" "new content")))
                                                    :data "test result data"))
                                       (merged (macher-agent-vfs--merge-payload state)))
                                  (expect (plist-get merged :data) :to-equal "test result data")
                                  (expect (plist-get merged :status) :to-equal 'initial)
                                  (expect (macher-agent--read-context-file ctx "file1.txt") :to-equal "new content"))
                                ;; 2. Handle deletions
                                (macher-agent--update-context-file ctx "deleted-file.txt" "original text")
                                (macher-agent-vfs--merge-payload (list :context ctx :diff (list (cons "deleted-file.txt" (cons "original text" nil)))))
                                (expect (macher-agent--read-context-file ctx "deleted-file.txt") :to-be nil)
                                ;; 3. Workspace-id and shared-state context extraction
                                (puthash (expand-file-name mock-dir) ctx macher-agent-active-workspaces)
                                (let ((macher-agent--persistent-context ambient-ctx))
                                  (macher-agent-vfs--merge-payload (list :workspace-id mock-dir :diff (list (cons "scoped-file.txt" (cons nil "target payload")))))
                                  (expect (macher-agent--read-context-file ctx "scoped-file.txt") :to-equal "target payload"))
                                (macher-agent-vfs--merge-payload (list :shared-state (list :context ctx) :diff (list (cons "shared-file.txt" (cons nil "shared content")))))
                                (expect (macher-agent--read-context-file ctx "shared-file.txt") :to-equal "shared content")
                                ;; 4. Target buffer persistent context update
                                (let ((res (macher-agent-vfs--merge-payload (list :parent-context ctx :target-buffer target-buf :diff (list (cons "merged-doc.txt" (cons nil "fresh content")))))))
                                  (expect (plist-get res :target-context) :to-be ctx)
                                  (expect (macher-agent--read-context-file ctx "merged-doc.txt") :to-equal "fresh content")
                                  (with-current-buffer target-buf
                                    (expect macher-agent--persistent-context :to-be ctx))))
                            (remhash (expand-file-name mock-dir) macher-agent-active-workspaces)
                            (when (buffer-live-p target-buf) (kill-buffer target-buf))
                            (delete-directory mock-dir t))))

                    (it "registers macher-agent-vfs--merge-payload via macher-agent-vfs-install"
                        (clrhash macher-agent-pipeline-registry)
                        (setq macher-agent-task-flush-hook nil)
                        (setq macher-agent-vfs-flush-hook nil)
                        (macher-agent-vfs-install)
                        (let ((merge-steps (macher-agent-get-pipeline-steps 'payload-merge)))
                          (expect (member #'macher-agent-vfs--merge-payload merge-steps) :to-be-truthy))
                        (let* ((entries (gethash 'payload-merge macher-agent-pipeline-registry))
                               (entry (cl-find #'macher-agent-vfs--merge-payload entries
                                               :key (lambda (e) (plist-get e :step)))))
                          (expect (plist-get entry :priority) :to-equal 10))
                        (expect (member #'macher-agent-vfs-handle-flush macher-agent-task-flush-hook) :to-be-truthy)
                        (expect (member #'macher-agent-vfs-build-patch-from-hook macher-agent-vfs-flush-hook) :to-be-truthy))

                    (it "composes artifact payload with diff when context has modified files and unmodified when clean via macher-agent-prepare-upstream-payloads"
                        (let* ((mock-modified (cons "file1.txt" (cons "original" "modified")))
                               (mock-clean (cons "file1.txt" (cons "same" "same")))
                               (ctx-mod (macher-agent--make-vfs-context :contents (list mock-modified)))
                               (ctx-clean (macher-agent--make-vfs-context :contents (list mock-clean)))
                               (payload (list :status 'success :data "Done" :buffer-name "test-buf")))
                          (let* ((macher-agent--persistent-context ctx-mod)
                                 (composed (macher-agent-prepare-upstream-payloads payload)))
                            (expect (plist-get composed :diff) :not :to-be nil)
                            (expect (length (plist-get composed :diff)) :to-equal 1)
                            (expect (car (car (plist-get composed :diff))) :to-equal "file1.txt"))
                          (let* ((macher-agent--persistent-context ctx-clean)
                                 (composed (macher-agent-prepare-upstream-payloads payload)))
                            (expect (plist-get composed :diff) :to-be nil)
                            (expect (plist-get composed :data) :to-equal "Done"))))

                    (it "resolves context comprehensively across transit keys, states, and buffers"
                        (let* ((mock-dir (make-temp-file "macher-transit-test" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (ctx (macher-agent--make-vfs-context :workspace workspace :contents nil))
                               (buf (generate-new-buffer "*macher-buf-fallback-test*")))
                          (unwind-protect
                              (progn
                                (expect (macher-agent-resolve-from-transit-payload ctx) :to-be ctx)
                                (dolist (key '(:target-context :parent-context :context))
                                  (let* ((payload (list key ctx :data "sample"))
                                         (resolved (macher-agent-resolve-from-transit-payload payload)))
                                    (expect resolved :to-be ctx)
                                    (let* ((pipe-state (list :input payload :resolved nil))
                                           (res-state (macher-agent-resolve-from-transit-payload pipe-state)))
                                      (expect (plist-get res-state :resolved) :to-be ctx))))
                                ;; Raw payloads with :resolved
                                (expect (macher-agent-resolve-from-transit-payload (list :context ctx :resolved t)) :to-be ctx)
                                (expect (macher-agent-resolve-from-transit-payload (list :resolved "done" :target-context ctx)) :to-be ctx)
                                ;; Shared state plists
                                (expect (macher-agent-resolve-from-transit-payload (list :shared-state (list :context ctx))) :to-be ctx)
                                ;; Buffer fallbacks
                                (with-current-buffer buf
                                  (setq-local macher-agent--persistent-context ctx))
                                (expect (macher-agent-resolve-from-transit-payload (list :buffer-name (buffer-name buf))) :to-be ctx)
                                (expect (macher-agent-resolve-from-transit-payload (list :target-buffer buf)) :to-be ctx)
                                ;; Invalid inputs reject with an error signal
                                (expect (macher-agent-resolve-from-transit-payload '(:context "invalid-string")) :to-throw 'error)
                                (expect (macher-agent-resolve-from-transit-payload '(project . "/some/path")) :to-throw 'error)
                                (expect (macher-agent-resolve-from-transit-payload 12345) :to-throw 'error)
                                (expect (macher-agent-resolve-from-transit-payload "string-input") :to-throw 'error))
                            (when (buffer-live-p buf) (kill-buffer buf))
                            (delete-directory mock-dir t))))

                    (it "extracts context from FSM using macher-agent--extract-fsm-context"
                        (let* ((mock-dir (make-temp-file "macher-fsm-ctx-test" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (ctx (macher-agent--make-vfs-context :workspace workspace :contents nil)))
                          (unwind-protect
                              (progn
                                (expect (macher-agent--extract-fsm-context nil) :to-be nil)
                                (expect (macher-agent--extract-fsm-context ctx) :to-be ctx)
                                (cl-letf (((symbol-function 'macher-agent--extract-fsm-info)
                                           (lambda (_fsm) (list :target-context ctx))))
                                  (expect (macher-agent--extract-fsm-context 'mock-fsm) :to-be ctx))
                                (cl-letf (((symbol-function 'macher-agent--extract-fsm-info)
                                           (lambda (_fsm) (list :shared-state (list :context ctx)))))
                                  (expect (macher-agent--extract-fsm-context 'mock-fsm-pipeline) :to-be ctx))
                                (cl-letf (((symbol-function 'macher-agent--extract-fsm-info)
                                           (lambda (_fsm) (list :context ctx))))
                                  (expect (macher-agent--extract-fsm-context 'mock-fsm-err) :to-be ctx)))
                            (delete-directory mock-dir t)))))

          (describe "3. Programmatic Tool Calling (PTC)"
                    (it "assesses primitives and injects ptc_execution tool when primitives are active"
                        (let* ((state (list :tools (list 'search_in_workspace)
                                            :ptc-primitives (list 'spawn-subagent)))
                               (updated (macher-agent-ptc--inject-tool state nil)))
                          (let ((tool-names (mapcar (lambda (tl)
                                                      (if (symbolp tl) (symbol-name tl) (gptel-tool-name tl)))
                                                    (plist-get updated :tools))))
                            (expect (member "ptc_execution" tool-names) :to-be-truthy)))
                        (let* ((state (list :tools (list 'search_in_workspace)
                                            :ptc-primitives nil))
                               (macher-agent--active-ptc-primitives nil)
                               (updated (macher-agent-ptc--inject-tool state nil)))
                          (expect (plist-get updated :tools) :to-equal (list 'search_in_workspace))))

                    (it "registers PTC steps to preset-composition and transmission pipelines via sandbox-install"
                        (clrhash macher-agent-pipeline-registry)
                        (macher-agent-sandbox-install)
                        (expect (member #'macher-agent-ptc--inject-tool (macher-agent-get-pipeline-steps 'preset-composition)) :to-be-truthy)
                        (expect (member #'macher-agent-sandbox-append-ptc-directive (macher-agent-get-pipeline-steps 'transmission)) :to-be-truthy)))

          (describe "4. Memory and Context Truncation (Zero-Mem)"
                    (it "calculates limits using context horizon in macher-agent-memory-pipe--inject-tool"
                        (with-temp-buffer
                          (insert (make-string 3000 ?a))
                          (let* ((macher-agent-max-context-chars '((nil . 500)))
                                 (state (make-macher-agent-transmission-state :target-buffer (current-buffer)
                                                                              :tools nil))
                                 (updated (macher-agent-memory-pipe--inject-tool state (current-buffer) nil nil nil)))
                            (expect (member "search_conversation_history"
                                            (mapcar #'macher-agent-canonical-tool-name
                                                    (macher-agent-transmission-state-tools updated)))
                                    :to-be-truthy)))
                        (with-temp-buffer
                          (insert "short content")
                          (let* ((macher-agent-max-context-chars '((nil . 50000)))
                                 (state (make-macher-agent-transmission-state :target-buffer (current-buffer)
                                                                              :tools nil))
                                 (updated (macher-agent-memory-pipe--inject-tool state (current-buffer) nil nil nil)))
                            (expect (member "search_conversation_history"
                                            (mapcar #'macher-agent-canonical-tool-name
                                                    (macher-agent-transmission-state-tools updated)))
                                    :to-be nil))))

                    (it "installs and uninstalls memory pipeline steps via zero-mem-install and zero-mem-uninstall"
                        (let ((saved-registry (copy-hash-table macher-agent-pipeline-registry)))
                          (unwind-protect
                              (progn
                                (clrhash macher-agent-pipeline-registry)
                                (macher-agent-zero-mem-install)
                                (let ((steps (macher-agent-get-pipeline-steps 'transmission)))
                                  (expect (member #'macher-agent-memory-pipe--inject-tool steps) :to-be-truthy)
                                  (expect (member #'macher-agent-memory-pipe--truncate-buffer steps) :to-be-truthy)
                                  (expect (member #'macher-agent-memory-pipe--inject-directive steps) :to-be-truthy))
                                (expect (default-value 'macher-agent-search-backend-function) :to-equal #'macher-agent-memory-search-zero-mem)
                                (expect (member #'macher-agent-memory--persist-interaction macher-agent-task-flush-hook) :to-be-truthy)
                                (macher-agent-zero-mem-uninstall)
                                (let ((steps (macher-agent-get-pipeline-steps 'transmission)))
                                  (expect (member #'macher-agent-memory-pipe--inject-tool steps) :to-be nil)
                                  (expect (member #'macher-agent-memory-pipe--truncate-buffer steps) :to-be nil)
                                  (expect (member #'macher-agent-memory-pipe--inject-directive steps) :to-be nil))
                                (expect (member #'macher-agent-memory--persist-interaction macher-agent-task-flush-hook) :to-be nil)
                                (expect (default-value 'macher-agent-search-backend-function) :to-equal #'macher-agent-search-glob))
                            (setq macher-agent-pipeline-registry saved-registry))))

                    (it "persists conversation history and aggregates results without premature task flush"
                        (with-temp-buffer
                          (insert "User query regarding system architecture.\nAssistant reply explaining decoupled plugins.\n")
                          (let ((graph (macher-agent-memory--persist-interaction (current-buffer))))
                            (expect graph :not :to-be nil)
                            (expect (gethash (buffer-name (current-buffer)) macher-agent-memory-vector-storage) :to-be graph)))
                        (let* ((hook-called nil)
                               (hook-fn (lambda () (setq hook-called t)))
                               (callback-result nil)
                               (final-cb (lambda (res) (setq callback-result res)))
                               (parent-buf (generate-new-buffer "test-parent-flush-buf"))
                               (results-ht (make-hash-table :test 'equal))
                               (task-id "flush-test-task")
                               (msg-body (list :status 'success :data "done")))
                          (add-hook 'macher-agent-task-flush-hook hook-fn)
                          (unwind-protect
                              (progn
                                (macher-agent--aggregate-a2a-results
                                 task-id msg-body results-ht 1 (list (list :task-id task-id)) final-cb parent-buf nil)
                                (expect hook-called :to-be nil)
                                (expect callback-result :to-equal (vector msg-body)))
                            (remove-hook 'macher-agent-task-flush-hook hook-fn)
                            (kill-buffer parent-buf)))))

          (describe "5. Pipeline Registry Runtime Dispatching"
                    (it "executes dynamically registered steps during macher-agent-compose-payload without duplicate tail loop"
                        (let* ((call-count 0)
                               (dynamic-step (lambda (state &optional _item)
                                               (setq call-count (1+ call-count))
                                               (plist-put (copy-sequence state) :dynamic-flag t))))
                          (macher-agent-register-pipeline-step 'preset-composition dynamic-step 80)
                          (unwind-protect
                              (let ((res (macher-agent-compose-payload (list :known-presets nil) '(single-preset))))
                                (expect call-count :to-equal 1)
                                (expect (plist-get res :dynamic-flag) :to-be t))
                            (let ((entries (gethash 'preset-composition macher-agent-pipeline-registry)))
                              (puthash 'preset-composition
                                       (cl-remove-if (lambda (e) (equal (plist-get e :step) dynamic-step)) entries)
                                       macher-agent-pipeline-registry)))))

                    (it "executes dynamic preset-composition steps exactly once across multiple presets"
                        (let* ((call-count 0)
                               (dynamic-step (lambda (payload &optional _item)
                                               (setq call-count (1+ call-count))
                                               (plist-put (copy-sequence payload) :dynamic-flag t))))
                          (macher-agent-register-pipeline-step 'preset-composition dynamic-step 80)
                          (unwind-protect
                              (let ((res (macher-agent-compose-payload (list :known-presets nil) '(preset-one preset-two preset-three))))
                                (expect call-count :to-equal 1)
                                (expect (plist-get res :dynamic-flag) :to-be t))
                            (let ((entries (gethash 'preset-composition macher-agent-pipeline-registry)))
                              (puthash 'preset-composition
                                       (cl-remove-if (lambda (e) (equal (plist-get e :step) dynamic-step)) entries)
                                       macher-agent-pipeline-registry)))))

                    (it "executes dynamically registered steps during macher-agent--compile-transmission-payload"
                        (with-temp-buffer
                          (let* ((dyn-tx-called nil)
                                 (dyn-tx-step (lambda (state &optional _buf _presets _skills _redir)
                                                (setq dyn-tx-called t)
                                                state)))
                            (macher-agent-register-pipeline-step 'transmission dyn-tx-step 85)
                            (unwind-protect
                                (progn
                                  (macher-agent--compile-transmission-payload (current-buffer) nil nil nil)
                                  (expect dyn-tx-called :to-be t))
                              (let ((entries (gethash 'transmission macher-agent-pipeline-registry)))
                                (puthash 'transmission
                                         (cl-remove-if (lambda (e) (equal (plist-get e :step) dyn-tx-step)) entries)
                                         macher-agent-pipeline-registry))))))

                    (it "executes dynamically registered steps during payload-merge in bind closure"
                        (let* ((mock-dir (make-temp-file "macher-merge-dyn-test" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (parent-ctx (macher-agent--make-vfs-context :workspace workspace :contents nil))
                               (parent-buf (generate-new-buffer "test-parent-merge-buf"))
                               (child-buf (generate-new-buffer "test-child-merge-buf"))
                               (dyn-merge-called nil)
                               (dyn-merge-step (lambda (state)
                                                 (setq dyn-merge-called t)
                                                 (plist-put (copy-sequence state) :reduced t))))
                          (macher-agent-register-pipeline-step 'payload-merge dyn-merge-step 15)
                          (unwind-protect
                              (progn
                                (with-current-buffer parent-buf
                                  (setq macher-agent--persistent-context parent-ctx))
                                (let* ((task-id "task-merge-dynamic")
                                       (results (make-hash-table :test 'equal))
                                       (shared-state (list :results results
                                                           :total 1
                                                           :final-callback nil
                                                           :parent-buffer parent-buf
                                                           :parent-fsm nil
                                                           :original-payloads nil))
                                       (state (list :a2a-msg (list :type 'SEND_MESSAGE :task-id task-id)
                                                    :child-buf child-buf
                                                    :shared-state shared-state))
                                       (res-state (macher-agent-a2a-pipe--bind-closure state))
                                       (cb (gethash task-id macher-agent--pending-callbacks)))
                                  (expect cb :not :to-be nil)
                                  (funcall cb (list :type 'ARTIFACT_UPDATE
                                                    :task-id task-id
                                                    :message (list :status 'success :data "done" :diff nil)))
                                  (expect dyn-merge-called :to-be t)))
                            (let ((entries (gethash 'payload-merge macher-agent-pipeline-registry)))
                              (puthash 'payload-merge
                                       (cl-remove-if (lambda (e) (equal (plist-get e :step) dyn-merge-step)) entries)
                                       macher-agent-pipeline-registry))
                            (kill-buffer parent-buf)
                            (kill-buffer child-buf)
                            (delete-directory mock-dir t))))

                    (it "correctly merges child diffs into parent context even when child buffer is live"
                        (let* ((mock-dir (make-temp-file "macher-child-merge-test" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (parent-ctx (macher-agent--make-vfs-context :workspace workspace :contents nil))
                               (child-ctx (macher-agent--make-vfs-context :workspace workspace
                                                                          :contents (list (macher-agent-vfs-make-entry "merged-file.el" "initial" "initial"))))
                               (parent-buf (generate-new-buffer "test-parent-live-buf"))
                               (child-buf (generate-new-buffer "test-child-live-buf"))
                               (task-id "task-live-merge"))
                          (unwind-protect
                              (progn
                                (with-current-buffer parent-buf
                                  (setq macher-agent--persistent-context parent-ctx))
                                (with-current-buffer child-buf
                                  (setq macher-agent--persistent-context child-ctx))
                                (let* ((results (make-hash-table :test 'equal))
                                       (shared-state (list :results results
                                                           :total 1
                                                           :final-callback nil
                                                           :parent-buffer parent-buf
                                                           :parent-fsm nil
                                                           :original-payloads nil))
                                       (state (list :a2a-msg (list :type 'SEND_MESSAGE :task-id task-id)
                                                    :child-buf child-buf
                                                    :shared-state shared-state))
                                       (res-state (macher-agent-a2a-pipe--bind-closure state))
                                       (cb (gethash task-id macher-agent--pending-callbacks)))
                                  (expect cb :not :to-be nil)
                                  (macher-agent--update-context-file child-ctx "merged-file.el" "updated-content")
                                  (let ((composed (macher-agent-prepare-upstream-payloads
                                                   (list :target-context child-ctx :data "done" :status 'success :buffer-name (buffer-name child-buf)))))
                                    (funcall cb (list :type 'ARTIFACT_UPDATE
                                                      :task-id task-id
                                                      :message composed)))
                                  (expect (macher-agent--read-context-file parent-ctx "merged-file.el")
                                          :to-equal "updated-content")
                                  (expect (macher-agent--read-context-file child-ctx "merged-file.el")
                                          :to-equal "updated-content")))
                            (kill-buffer parent-buf)
                            (kill-buffer child-buf)
                            (delete-directory mock-dir t))))

                    (it "chains multiple reducer steps sequentially in payload-merge pipeline"
                        (let* ((results-tbl (make-hash-table :test 'equal))
                               (step-one (lambda (msg)
                                           (plist-put (copy-sequence msg) :step-one-flag t)))
                               (step-two (lambda (msg)
                                           (plist-put (copy-sequence msg) :step-two-flag t)))
                               (parent-buf (generate-new-buffer "test-chain-parent"))
                               (child-buf (generate-new-buffer "test-chain-child"))
                               (task-id "task-chain-merge")
                               (shared-state (list :results results-tbl
                                                   :total 1
                                                   :final-callback nil
                                                   :parent-buffer parent-buf
                                                   :parent-fsm nil
                                                   :original-payloads nil))
                               (initial-state (list :a2a-msg (list :type 'SEND_MESSAGE :task-id task-id)
                                                    :child-buf child-buf
                                                    :shared-state shared-state)))
                          (macher-agent-register-pipeline-step 'payload-merge step-one 12)
                          (macher-agent-register-pipeline-step 'payload-merge step-two 14)
                          (unwind-protect
                              (progn
                                (macher-agent-a2a-pipe--bind-closure initial-state)
                                (let ((cb (gethash task-id macher-agent--pending-callbacks)))
                                  (funcall cb (list :type 'ARTIFACT_UPDATE
                                                    :task-id task-id
                                                    :message (list :status 'success
                                                                   :data "chain-result"
                                                                   :diff nil)))
                                  (let ((stored (gethash task-id results-tbl)))
                                    (expect (plist-get stored :step-one-flag) :to-be t)
                                    (expect (plist-get stored :step-two-flag) :to-be t))))
                            (let ((entries (gethash 'payload-merge macher-agent-pipeline-registry)))
                              (puthash 'payload-merge
                                       (cl-remove-if (lambda (e)
                                                       (or (equal (plist-get e :step) step-one)
                                                           (equal (plist-get e :step) step-two)))
                                                     entries)
                                       macher-agent-pipeline-registry))
                            (kill-buffer parent-buf)
                            (kill-buffer child-buf))))

                    (it "installs all plugin hooks and pipeline steps via macher-agent-install"
                        (clrhash macher-agent-pipeline-registry)
                        (setq macher-agent-task-flush-hook nil)
                        (setq macher-agent-vfs-flush-hook nil)
                        (macher-agent-install)
                        (expect (member #'macher-agent-vfs--merge-payload (macher-agent-get-pipeline-steps 'payload-merge)) :to-be-truthy)
                        (expect (member #'macher-agent-ptc--inject-tool (macher-agent-get-pipeline-steps 'preset-composition)) :to-be-truthy)
                        (expect (member #'macher-agent-sandbox-append-ptc-directive (macher-agent-get-pipeline-steps 'transmission)) :to-be-truthy)
                        (expect (member #'macher-agent-memory-pipe--inject-tool (macher-agent-get-pipeline-steps 'transmission)) :to-be-truthy)
                        (expect (member #'macher-agent-memory-pipe--truncate-buffer (macher-agent-get-pipeline-steps 'transmission)) :to-be-truthy)
                        (expect (member #'macher-agent-memory-pipe--inject-directive (macher-agent-get-pipeline-steps 'transmission)) :to-be-truthy)
                        (expect (member #'macher-agent-ctx-pipe--explicit (macher-agent-get-pipeline-steps 'context-resolution)) :to-be-truthy)
                        (expect (member #'macher-agent-resolve-from-transit-payload (macher-agent-get-pipeline-steps 'context-resolution)) :to-be-truthy)
                        (expect (member #'macher-agent-memory--persist-interaction macher-agent-task-flush-hook) :to-be-truthy)
                        (expect (member #'macher-agent-vfs-handle-flush macher-agent-task-flush-hook) :to-be-truthy)
                        (expect (member #'macher-agent-vfs-build-patch-from-hook macher-agent-vfs-flush-hook) :to-be-truthy)
                        (expect (member #'macher-agent--mutation-dispatcher macher-agent-context-mutated-hook) :to-be-truthy))

                    (it "invokes Level 3 bridge and Level 1 plugin loaders dynamically when bound"
                        (let ((level3-called nil)
                              (sandbox-called nil)
                              (zero-mem-called nil)
                              (vfs-called nil))
                          (cl-letf (((symbol-function 'macher-agent-context-resolution-install) (lambda () (setq level3-called t)))
                                    ((symbol-function 'macher-agent-sandbox-install) (lambda () (setq sandbox-called t)))
                                    ((symbol-function 'macher-agent-zero-mem-install) (lambda () (setq zero-mem-called t)))
                                    ((symbol-function 'macher-agent-vfs-install) (lambda () (setq vfs-called t))))
                            (macher-agent-install)
                            (expect level3-called :to-be t)
                            (expect sandbox-called :to-be t)
                            (expect zero-mem-called :to-be t)
                            (expect vfs-called :to-be t))))

                    (it "gracefully handles unbound plugin install functions during macher-agent-install"
                        (cl-letf (((symbol-function 'macher-agent-context-resolution-install) nil)
                                  ((symbol-function 'macher-agent-sandbox-install) nil)
                                  ((symbol-function 'macher-agent-zero-mem-install) nil)
                                  ((symbol-function 'macher-agent-vfs-install) nil))
                          (fmakunbound 'macher-agent-context-resolution-install)
                          (fmakunbound 'macher-agent-sandbox-install)
                          (fmakunbound 'macher-agent-zero-mem-install)
                          (fmakunbound 'macher-agent-vfs-install)
                          (expect (macher-agent-install) :not :to-throw)))))

(provide 'macher-agent-plugin-test)
;;; macher-agent-plugin-test.el ends here
