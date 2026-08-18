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

  (describe "1. Core Routing and Pipeline Registry"
    (it "initialises macher-agent-pipeline-registry as a hash table"
      (expect (hash-table-p macher-agent-pipeline-registry) :to-be t))

    (it "registers pipeline steps in strict priority depth order"
      (let ((pipeline-name (make-symbol "test-pipeline-priority")))
        (macher-agent-register-pipeline-step pipeline-name #'ignore 90)
        (macher-agent-register-pipeline-step pipeline-name #'identity 10)
        (macher-agent-register-pipeline-step pipeline-name #'car 50)
        (let ((steps (macher-agent-get-pipeline-steps pipeline-name)))
          (expect steps :to-equal (list #'identity #'car #'ignore)))))

    (it "updates existing step priority without creating duplicates"
      (let ((pipeline-name (make-symbol "test-pipeline-dedup")))
        (macher-agent-register-pipeline-step pipeline-name #'ignore 90)
        (macher-agent-register-pipeline-step pipeline-name #'identity 50)
        (macher-agent-register-pipeline-step pipeline-name #'ignore 20)
        (let ((steps (macher-agent-get-pipeline-steps pipeline-name)))
          (expect steps :to-equal (list #'ignore #'identity)))))

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
                                                      :parent-buf (current-buffer)
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
          (kill-buffer (plist-get initial-state :child-buf))))))

  (describe "2. Storage and Virtual File System"
    (it "executes within Virtual File System awareness scope using macher-agent-with-vfs-scope"
      (let* ((mock-dir (make-temp-file "macher-vfs-scope-test" t))
             (workspace (make-macher-agent-workspace :project-root mock-dir))
             (ctx (macher--make-context :workspace workspace :contents nil))
             (executed-dir nil)
             (executed-ctx nil))
        (unwind-protect
            (progn
              (macher-agent-with-vfs-scope ctx
                (setq executed-dir default-directory)
                (setq executed-ctx macher-agent--persistent-context))
              (expect (file-name-as-directory (file-truename executed-dir)) :to-equal (file-name-as-directory (file-truename mock-dir)))
              (expect executed-ctx :to-be ctx))
          (delete-directory mock-dir t))))

    (it "evaluates context expression only once in macher-agent-with-vfs-scope"
      (let* ((mock-dir (make-temp-file "macher-vfs-eval-test" t))
             (workspace (make-macher-agent-workspace :project-root mock-dir))
             (ctx (macher--make-context :workspace workspace :contents nil))
             (eval-count 0)
             (getter (lambda ()
                       (setq eval-count (1+ eval-count))
                       ctx)))
        (unwind-protect
            (progn
              (macher-agent-with-vfs-scope (funcall getter)
                (expect default-directory :not :to-be nil))
              (expect eval-count :to-equal 1))
          (delete-directory mock-dir t))))

    (it "merges payload diffs and updates state with macher-agent-vfs--merge-payload"
      (let* ((mock-dir (make-temp-file "macher-vfs-merge-test" t))
             (workspace (make-macher-agent-workspace :project-root mock-dir))
             (ctx (macher--make-context :workspace workspace :contents nil))
             (initial-state (list :status 'initial
                                  :context ctx
                                  :diff (list (cons "file1.txt" (cons "old" "new content")))
                                  :data "test result data")))
        (unwind-protect
            (let ((merged (macher-agent-vfs--merge-payload initial-state)))
              (expect (plist-get merged :data) :to-equal "test result data")
              (expect (plist-get merged :status) :to-equal 'initial)
              (let ((read-val (macher-agent--read-context-file ctx "file1.txt")))
                (expect read-val :to-equal "new content")))
          (delete-directory mock-dir t))))

    (it "allows nil content to trigger file deletion updates in macher-agent-vfs--merge-payload"
      (let* ((mock-dir (make-temp-file "macher-vfs-delete-test" t))
             (workspace (make-macher-agent-workspace :project-root mock-dir))
             (ctx (macher--make-context :workspace workspace :contents nil))
             (state (list :context ctx
                          :diff (list (cons "deleted-file.txt" (cons "original text" nil))))))
        (unwind-protect
            (progn
              (macher-agent--update-context-file ctx "deleted-file.txt" "original text")
              (expect (macher-agent--read-context-file ctx "deleted-file.txt") :to-equal "original text")
              (macher-agent-vfs--merge-payload state)
              (expect (macher-agent--read-context-file ctx "deleted-file.txt") :to-be nil))
          (delete-directory mock-dir t))))

    (it "extracts target context from workspace-id without relying on ambient buffer-local context"
      (let* ((mock-dir (make-temp-file "macher-vfs-ws-id-test" t))
             (workspace (make-macher-agent-workspace :project-root mock-dir))
             (target-ctx (macher--make-context :workspace workspace :contents nil))
             (ambient-ctx (macher--make-context :workspace (make-macher-agent-workspace :project-root "/unrelated") :contents nil))
             (state (list :workspace-id mock-dir
                          :diff (list (cons "scoped-file.txt" (cons nil "target payload"))))))
        (unwind-protect
            (progn
              (macher-agent--update-context-file ambient-ctx "scoped-file.txt" nil)
              (puthash (expand-file-name mock-dir) target-ctx macher-agent-active-workspaces)
              (let ((macher-agent--persistent-context ambient-ctx))
                (macher-agent-vfs--merge-payload state)
                (expect (macher-agent--read-context-file target-ctx "scoped-file.txt") :to-equal "target payload")
                (expect (macher-agent--read-context-file ambient-ctx "scoped-file.txt") :to-be nil)))
          (remhash (expand-file-name mock-dir) macher-agent-active-workspaces)
          (delete-directory mock-dir t))))

    (it "extracts target context from shared-state property list"
      (let* ((mock-dir (make-temp-file "macher-vfs-shared-test" t))
             (workspace (make-macher-agent-workspace :project-root mock-dir))
             (target-ctx (macher--make-context :workspace workspace :contents nil))
             (state (list :shared-state (list :context target-ctx)
                          :diff (list (cons "shared-file.txt" (cons nil "shared content"))))))
        (unwind-protect
            (progn
              (macher-agent-vfs--merge-payload state)
              (expect (macher-agent--read-context-file target-ctx "shared-file.txt") :to-equal "shared content"))
          (delete-directory mock-dir t))))

    (it "registers macher-agent-vfs--merge-payload and macher-agent-vfs--compose-artifact via macher-agent-vfs-install"
      (clrhash macher-agent-pipeline-registry)
      (setq macher-agent-task-flush-hook nil)
      (macher-agent-vfs-install)
      (let ((merge-steps (macher-agent-get-pipeline-steps 'payload-merge))
            (compose-steps (macher-agent-get-pipeline-steps 'artifact-compose)))
        (expect (member #'macher-agent-vfs--merge-payload merge-steps) :to-be-truthy)
        (expect (member #'macher-agent-vfs--compose-artifact compose-steps) :to-be-truthy))
      (let* ((entries (gethash 'artifact-compose macher-agent-pipeline-registry))
             (entry (cl-find #'macher-agent-vfs--compose-artifact entries
                             :key (lambda (e) (plist-get e :step)))))
        (expect (plist-get entry :priority) :to-equal 10))
      (expect (member #'macher-agent-vfs-handle-flush macher-agent-task-flush-hook) :to-be-truthy))

    (it "composes artifact payload with diff when context has modified files"
      (let* ((mock-entry (cons "file1.txt" (cons "original" "modified")))
             (mock-ctx (macher--make-context :contents (list mock-entry)))
             (macher-agent--persistent-context mock-ctx)
             (payload (list :status 'success :data "Done" :buffer-name "test-buf"))
             (composed (macher-agent-vfs--compose-artifact payload)))
        (expect (plist-get composed :diff) :not :to-be nil)
        (expect (length (plist-get composed :diff)) :to-equal 1)
        (expect (car (car (plist-get composed :diff))) :to-equal "file1.txt")))

    (it "returns artifact payload unmodified when context has no differences"
      (let* ((mock-entry (cons "file1.txt" (cons "same" "same")))
             (mock-ctx (macher--make-context :contents (list mock-entry)))
             (macher-agent--persistent-context mock-ctx)
             (payload (list :status 'success :data "Done" :buffer-name "test-buf"))
             (composed (macher-agent-vfs--compose-artifact payload)))
        (expect (plist-get composed :diff) :to-be nil)
        (expect (plist-get composed :data) :to-equal "Done"))))

  (describe "3. Programmatic Tool Calling (PTC)"
    (it "assesses primitives and injects ptc_execution tool when primitives are active"
      (let* ((state (list :tools (list 'search_in_workspace)
                          :ptc-primitives (list 'spawn-subagent)))
             (updated (macher-agent-ptc--inject-tool state nil)))
        (let ((tool-names (mapcar (lambda (tl)
                                    (if (symbolp tl) (symbol-name tl) (gptel-tool-name tl)))
                                  (plist-get updated :tools))))
          (expect (member "ptc_execution" tool-names) :to-be-truthy))))

    (it "does not inject ptc_execution tool when no primitives are active"
      (let* ((state (list :tools (list 'search_in_workspace)
                          :ptc-primitives nil))
             (macher-agent--active-ptc-primitives nil)
             (updated (macher-agent-ptc--inject-tool state nil)))
        (expect (plist-get updated :tools) :to-equal (list 'search_in_workspace))))

    (it "registers macher-agent-ptc--inject-tool to preset-composition pipeline via macher-agent-sandbox-install"
      (clrhash macher-agent-pipeline-registry)
      (macher-agent-sandbox-install)
      (let ((steps (macher-agent-get-pipeline-steps 'preset-composition)))
        (expect (member #'macher-agent-ptc--inject-tool steps) :to-be-truthy))
      (let* ((entries (gethash 'preset-composition macher-agent-pipeline-registry))
             (entry (cl-find #'macher-agent-ptc--inject-tool entries
                             :key (lambda (e) (plist-get e :step)))))
        (expect (plist-get entry :priority) :to-equal 50)))

    (it "verifies macher-agent-ptc-install is an alias of macher-agent-sandbox-install"
      (expect (symbol-function 'macher-agent-ptc-install) :not :to-be nil)
      (clrhash macher-agent-pipeline-registry)
      (macher-agent-ptc-install)
      (expect (member #'macher-agent-ptc--inject-tool (macher-agent-get-pipeline-steps 'preset-composition)) :to-be-truthy))

    (it "evaluates AST safely using macher-agent-ptc--evaluate-ast"
      (let ((res (macher-agent-ptc--evaluate-ast '(+ 40 2) nil 5.0)))
        (expect res :to-equal 42)))

    (it "restricts erratic evaluation with a timeout failsafe boundary in macher-agent-ptc--evaluate-ast"
      (spy-on 'macher-agent-sandbox--eval-sync :and-call-fake
              (lambda (_ast _env)
                (sleep-for 0.3)))
      (expect (macher-agent-ptc--evaluate-ast '(infinite-loop) nil 0.05)
              :to-throw 'error))

    (it "throws error without falling back to eval when macher-agent-sandbox--eval-sync is unmapped"
      (cl-letf (((symbol-function 'macher-agent-sandbox--eval-sync) nil))
        (fmakunbound 'macher-agent-sandbox--eval-sync)
        (expect (macher-agent-ptc--evaluate-ast '(+ 1 1) nil 1.0)
                :to-throw 'error))))

  (describe "4. Memory System"
    (it "calculates limits using context horizon in macher-agent-memory--truncate-prompt"
      (with-temp-buffer
        (insert (make-string 2000 ?a))
        (let* ((macher-agent-max-context-chars '((nil . 500)))
               (state (make-macher-agent-transmission-state :target-buffer (current-buffer)
                                                            :tools (list 'read_file)))
               (updated (macher-agent-memory--truncate-prompt state (current-buffer) nil nil nil)))
          (let ((tool-names (mapcar (lambda (tl)
                                      (if (symbolp tl) (symbol-name tl) (gptel-tool-name tl)))
                                    (macher-agent-transmission-state-tools updated))))
            (expect (member "search_conversation_history" tool-names) :to-be-truthy)))))

    (it "does not append memory tool when context is within horizon limit"
      (with-temp-buffer
        (insert "short prompt text")
        (let* ((macher-agent-max-context-chars '((nil . 50000)))
               (state (make-macher-agent-transmission-state :target-buffer (current-buffer)
                                                            :tools (list 'read_file)))
               (updated (macher-agent-memory--truncate-prompt state (current-buffer) nil nil nil)))
          (let ((tool-names (mapcar (lambda (tl)
                                      (if (symbolp tl) (symbol-name tl) (gptel-tool-name tl)))
                                    (macher-agent-transmission-state-tools updated))))
            (expect (member "search_conversation_history" tool-names) :to-be nil)))))

    (it "registers macher-agent-memory--truncate-prompt to transmission pipeline via macher-agent-zero-mem-install"
      (clrhash macher-agent-pipeline-registry)
      (macher-agent-zero-mem-install)
      (let ((steps (macher-agent-get-pipeline-steps 'transmission)))
        (expect (member #'macher-agent-memory--truncate-prompt steps) :to-be-truthy))
      (let* ((entries (gethash 'transmission macher-agent-pipeline-registry))
             (entry (cl-find #'macher-agent-memory--truncate-prompt entries
                             :key (lambda (e) (plist-get e :step)))))
        (expect (plist-get entry :priority) :to-equal 90)))

    (it "verifies macher-agent-memory-install is an alias of macher-agent-zero-mem-install"
      (expect (symbol-function 'macher-agent-memory-install) :not :to-be nil)
      (clrhash macher-agent-pipeline-registry)
      (macher-agent-memory-install)
      (expect (member #'macher-agent-memory--truncate-prompt (macher-agent-get-pipeline-steps 'transmission)) :to-be-truthy))

    (it "persists conversation history to vector storage with macher-agent-memory--persist-interaction"
      (with-temp-buffer
        (rename-buffer "*test-persist-session*" t)
        (insert "User query regarding system architecture.\nAssistant reply explaining decoupled plugins.\n")
        (let ((graph (macher-agent-memory--persist-interaction (current-buffer))))
          (expect graph :not :to-be nil)
          (expect (gethash (buffer-name (current-buffer)) macher-agent-memory-vector-storage) :to-be graph))))

    (it "does not attach macher-agent-memory--persist-interaction to macher-agent-task-flush-hook on file load"
      (setq macher-agent-task-flush-hook nil)
      (load (expand-file-name "macher-agent-zero-mem.el") nil t)
      (expect (member #'macher-agent-memory--persist-interaction macher-agent-task-flush-hook) :to-be nil))

    (it "attaches macher-agent-memory--persist-interaction to macher-agent-task-flush-hook"
      (setq macher-agent-task-flush-hook nil)
      (macher-agent-zero-mem-install)
      (expect (member #'macher-agent-memory--persist-interaction macher-agent-task-flush-hook) :to-be-truthy))

    (it "aggregates results and invokes final-callback inside parent buffer without premature task flush"
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
          (kill-buffer parent-buf))))

    (it "executes final-callback exactly once across multiple task results without premature task flush"
      (let* ((hook-count 0)
             (hook-fn (lambda () (setq hook-count (1+ hook-count))))
             (callback-calls 0)
             (final-cb (lambda (_res) (setq callback-calls (1+ callback-calls))))
             (parent-buf (generate-new-buffer "test-parent-flush-count-buf"))
             (results-ht (make-hash-table :test 'equal))
             (task1 "task-1")
             (task2 "task-2")
             (orig-payloads (list (list :task-id task1) (list :task-id task2))))
        (add-hook 'macher-agent-task-flush-hook hook-fn)
        (unwind-protect
            (progn
              (macher-agent--aggregate-a2a-results
               task1 (list :status 'success :val 1) results-ht 2 orig-payloads final-cb parent-buf nil)
              (expect callback-calls :to-equal 0)
              (expect hook-count :to-equal 0)
              (macher-agent--aggregate-a2a-results
               task2 (list :status 'success :val 2) results-ht 2 orig-payloads final-cb parent-buf nil)
              (expect callback-calls :to-equal 1)
              (expect hook-count :to-equal 0)
              (macher-agent--aggregate-a2a-results
               task2 (list :status 'success :val 2) results-ht 2 orig-payloads final-cb parent-buf nil)
              (expect callback-calls :to-equal 1)
              (expect hook-count :to-equal 0))
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
             (parent-ctx (macher--make-context :workspace workspace :contents nil))
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
              (let* ((state (list :a2a-msg (list :task-id "task-merge-dynamic-1" :metadata nil)
                                  :shared-state (list :results (make-hash-table :test 'equal)
                                                      :total 1
                                                      :final-callback nil
                                                      :parent-buf parent-buf
                                                      :parent-fsm nil
                                                      :original-payloads nil)
                                  :child-buf child-buf))
                     (res-state (macher-agent-a2a-pipe--bind-closure state))
                     (cb (plist-get res-state :a2a-cb)))
                (funcall cb (list :type 'ARTIFACT_UPDATE
                                  :task-id "task-merge-dynamic-1"
                                  :message (list :data "sample-data" :diff nil)))
                (expect dyn-merge-called :to-be t)))
          (let ((entries (gethash 'payload-merge macher-agent-pipeline-registry)))
            (puthash 'payload-merge
                     (cl-remove-if (lambda (e) (equal (plist-get e :step) dyn-merge-step)) entries)
                     macher-agent-pipeline-registry))
          (kill-buffer parent-buf)
          (kill-buffer child-buf)
          (delete-directory mock-dir t))))

    (it "correctly merges child diffs into parent context even when child buffer is live"
      (let* ((mock-dir (make-temp-file "macher-subagent-parent-merge" t))
             (workspace (make-macher-agent-workspace :project-root mock-dir))
             (parent-ctx (macher--make-context :workspace workspace
                                               :contents (list (macher-agent-vfs-make-entry "merged-file.el" "initial" "initial"))))
             (child-ctx (macher--make-context :workspace workspace
                                              :contents (list (macher-agent-vfs-make-entry "merged-file.el" "initial" "initial"))))
             (parent-buf (generate-new-buffer "test-parent-ctx-merge"))
             (child-buf (generate-new-buffer "test-child-ctx-merge")))
        (unwind-protect
            (progn
              (with-temp-file (expand-file-name "merged-file.el" mock-dir)
                (insert "initial"))
              (with-current-buffer parent-buf
                (setq macher-agent--persistent-context parent-ctx))
              (with-current-buffer child-buf
                (setq macher-agent--persistent-context child-ctx))
              (let* ((state (list :a2a-msg (list :task-id "task-child-parent-1" :metadata nil)
                                  :shared-state (list :results (make-hash-table :test 'equal)
                                                      :total 1
                                                      :final-callback nil
                                                      :parent-buf parent-buf
                                                      :parent-fsm nil
                                                      :original-payloads nil)
                                  :child-buf child-buf))
                     (res-state (macher-agent-a2a-pipe--bind-closure state))
                     (cb (plist-get res-state :a2a-cb)))
                (funcall cb (list :type 'ARTIFACT_UPDATE
                                  :task-id "task-child-parent-1"
                                  :message (list :status 'success
                                                 :data "child final result"
                                                 :buffer-name (buffer-name child-buf)
                                                 :diff (list (macher-agent-vfs-make-entry "merged-file.el" "initial" "updated from subagent")))))
                ;; Parent context should receive the updated file
                (expect (macher-agent--read-context-file parent-ctx "merged-file.el")
                        :to-equal "updated from subagent")
                ;; Child context should remain untouched
                (expect (macher-agent--read-context-file child-ctx "merged-file.el")
                        :to-equal "initial")))
          (kill-buffer parent-buf)
          (kill-buffer child-buf)
          (delete-directory mock-dir t))))

    (it "does not trigger macher-agent-task-flush-hook inside submit_task_result tool command"
      (load (expand-file-name "skills/scripts/submit_task_result.el") nil t)
      (let* ((buf (generate-new-buffer "test-flush-tool-buf"))
             (hook-executed nil)
             (hook-fn (lambda () (setq hook-executed t)))
             (tool-cmd (or (get 'macher-agent-submit-task-result-tool 'command-fn)
                           (get 'macher-agent-tool-submit-task-result 'command-fn))))
        (add-hook 'macher-agent-task-flush-hook hook-fn)
        (unwind-protect
            (with-current-buffer buf
              (macher-agent--push-routing "task-no-flush-123" "originator" t)
              (funcall tool-cmd '(:final_answer "Done") nil nil)
              (expect hook-executed :to-be nil))
          (remove-hook 'macher-agent-task-flush-hook hook-fn)
          (kill-buffer buf))))

    (it "does not execute macher-agent--apply-vfs-diff inside macher-agent-a2a-pipe--bind-closure"
      (let* ((parent-buf (generate-new-buffer "test-no-apply-vfs-parent"))
             (child-buf (generate-new-buffer "test-no-apply-vfs-child"))
             (task-id "task-no-hardcode-vfs")
             (results-tbl (make-hash-table :test 'equal))
             (shared-state (list :results results-tbl
                                 :total 1
                                 :final-callback nil
                                 :parent-buf parent-buf
                                 :parent-fsm nil
                                 :original-payloads (list (list :type 'SEND_MESSAGE :task-id task-id))))
             (initial-state (list :a2a-msg (list :type 'SEND_MESSAGE :task-id task-id)
                                  :child-buf child-buf
                                  :shared-state shared-state)))
        (unwind-protect
            (progn
              (spy-on 'macher-agent--apply-vfs-diff :and-call-through)
              (macher-agent-a2a-pipe--bind-closure initial-state)
              (let ((cb (gethash task-id macher-agent--pending-callbacks)))
                (funcall cb (list :type 'ARTIFACT_UPDATE
                                  :task-id task-id
                                  :message (list :status 'success
                                                 :data "output"
                                                 :diff nil)))
                (expect 'macher-agent--apply-vfs-diff :not :to-have-been-called)))
          (kill-buffer parent-buf)
          (kill-buffer child-buf))))

    (it "chains multiple reducer steps sequentially in payload-merge pipeline"
      (let* ((parent-buf (generate-new-buffer "test-chain-parent"))
             (child-buf (generate-new-buffer "test-chain-child"))
             (task-id "task-reducer-chain-1")
             (results-tbl (make-hash-table :test 'equal))
             (step-one (lambda (st)
                         (plist-put (copy-sequence st) :step-one-flag t)))
             (step-two (lambda (st)
                         (if (plist-get st :step-one-flag)
                             (plist-put (copy-sequence st) :step-two-flag t)
                           st)))
             (shared-state (list :results results-tbl
                                 :total 1
                                 :final-callback nil
                                 :parent-buf parent-buf
                                 :parent-fsm nil
                                 :original-payloads (list (list :type 'SEND_MESSAGE :task-id task-id))))
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
      (macher-agent-install)
      (expect (member #'macher-agent-vfs--merge-payload (macher-agent-get-pipeline-steps 'payload-merge)) :to-be-truthy)
      (expect (member #'macher-agent-vfs--compose-artifact (macher-agent-get-pipeline-steps 'artifact-compose)) :to-be-truthy)
      (expect (member #'macher-agent-ptc--inject-tool (macher-agent-get-pipeline-steps 'preset-composition)) :to-be-truthy)
      (expect (member #'macher-agent-memory--truncate-prompt (macher-agent-get-pipeline-steps 'transmission)) :to-be-truthy)
      (expect (member #'macher-agent-ctx-pipe--explicit (macher-agent-get-pipeline-steps 'context-resolution)) :to-be-truthy)
      (expect (member #'macher-agent-ctx-pipe--payload-explicit (macher-agent-get-pipeline-steps 'context-resolution)) :to-be-truthy)
      (expect (member #'macher-agent-ctx-pipe--payload-shared (macher-agent-get-pipeline-steps 'context-resolution)) :to-be-truthy)
      (expect (member #'macher-agent-memory--persist-interaction macher-agent-task-flush-hook) :to-be-truthy)
      (expect (member #'macher-agent-vfs-handle-flush macher-agent-task-flush-hook) :to-be-truthy)
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
        (expect (macher-agent-install) :not :to-throw))))

  (describe "6. Static Dependency & Declare-Function Eradication"
    (it "verifies macher-agent-api.el has no static require of sandbox, vfs, or macher"
      (let* ((root (locate-dominating-file default-directory "macher-agent-core.el"))
             (full-path (expand-file-name "macher-agent-api.el" (or root default-directory))))
        (with-temp-buffer
          (insert-file-contents full-path)
          (goto-char (point-min))
          (let ((forms nil))
            (condition-case nil
                (while t
                  (push (read (current-buffer)) forms))
              (end-of-file nil))
            (expect (cl-some (lambda (f) (and (consp f) (eq (car f) 'require) (equal (cadr f) ''macher-agent-sandbox))) forms) :to-be nil)
            (expect (cl-some (lambda (f) (and (consp f) (eq (car f) 'require) (equal (cadr f) ''macher-agent-vfs))) forms) :to-be nil)
            (expect (cl-some (lambda (f) (and (consp f) (eq (car f) 'require) (equal (cadr f) ''macher-agent-macher))) forms) :to-be nil)))))

    (it "verifies macher-agent-gptel.el has no static require of vfs or macher"
      (let* ((root (locate-dominating-file default-directory "macher-agent-core.el"))
             (full-path (expand-file-name "macher-agent-gptel.el" (or root default-directory))))
        (with-temp-buffer
          (insert-file-contents full-path)
          (goto-char (point-min))
          (let ((forms nil))
            (condition-case nil
                (while t
                  (push (read (current-buffer)) forms))
              (end-of-file nil))
            (expect (cl-some (lambda (f) (and (consp f) (eq (car f) 'require) (equal (cadr f) ''macher-agent-vfs))) forms) :to-be nil)
            (expect (cl-some (lambda (f) (and (consp f) (eq (car f) 'require) (equal (cadr f) ''macher-agent-macher))) forms) :to-be nil)))))

    (it "verifies macher-agent-orchestration.el has no static require of vfs or macher"
      (let* ((root (locate-dominating-file default-directory "macher-agent-core.el"))
             (full-path (expand-file-name "macher-agent-orchestration.el" (or root default-directory))))
        (with-temp-buffer
          (insert-file-contents full-path)
          (goto-char (point-min))
          (let ((forms nil))
            (condition-case nil
                (while t
                  (push (read (current-buffer)) forms))
              (end-of-file nil))
            (expect (cl-some (lambda (f) (and (consp f) (eq (car f) 'require) (equal (cadr f) ''macher-agent-vfs))) forms) :to-be nil)
            (expect (cl-some (lambda (f) (and (consp f) (eq (car f) 'require) (equal (cadr f) ''macher-agent-macher))) forms) :to-be nil)))))

    (it "verifies macher-agent-presets.el has no static require of vfs"
      (let* ((root (locate-dominating-file default-directory "macher-agent-core.el"))
             (full-path (expand-file-name "macher-agent-presets.el" (or root default-directory))))
        (with-temp-buffer
          (insert-file-contents full-path)
          (goto-char (point-min))
          (let ((forms nil))
            (condition-case nil
                (while t
                  (push (read (current-buffer)) forms))
              (end-of-file nil))
            (expect (cl-some (lambda (f) (and (consp f) (eq (car f) 'require) (equal (cadr f) ''macher-agent-vfs))) forms) :to-be nil)))))

    (it "verifies macher-agent-tools.el has no static require of sandbox, vfs, or zero-mem"
      (let* ((root (locate-dominating-file default-directory "macher-agent-core.el"))
             (full-path (expand-file-name "macher-agent-tools.el" (or root default-directory))))
        (with-temp-buffer
          (insert-file-contents full-path)
          (goto-char (point-min))
          (let ((forms nil))
            (condition-case nil
                (while t
                  (push (read (current-buffer)) forms))
              (end-of-file nil))
            (expect (cl-some (lambda (f) (and (consp f) (eq (car f) 'require) (equal (cadr f) ''macher-agent-sandbox))) forms) :to-be nil)
            (expect (cl-some (lambda (f) (and (consp f) (eq (car f) 'require) (equal (cadr f) ''macher-agent-vfs))) forms) :to-be nil)
            (expect (cl-some (lambda (f) (and (consp f) (eq (car f) 'require) (equal (cadr f) ''macher-agent-zero-mem))) forms) :to-be nil)))))

    (it "verifies macher-agent-core.el and macher-agent-api.el have no declare-function targeting plugin files"
      (let* ((root (locate-dominating-file default-directory "macher-agent-core.el"))
             (plugins '("macher-agent-sandbox" "macher-agent-vfs" "macher-agent-macher" "macher-agent-zero-mem"))
             (check-file
              (lambda (file-name)
                (let ((full-path (expand-file-name file-name (or root default-directory))))
                  (with-temp-buffer
                    (insert-file-contents full-path)
                    (goto-char (point-min))
                    (let ((forms nil))
                      (condition-case nil
                          (while t
                            (push (read (current-buffer)) forms))
                        (end-of-file nil))
                      (dolist (plugin plugins)
                        (expect (cl-some (lambda (f)
                                           (and (consp f)
                                                (eq (car f) 'declare-function)
                                                (let ((f-target (caddr f)))
                                                  (and (stringp f-target)
                                                       (or (string= f-target plugin)
                                                           (string-prefix-p plugin f-target))))))
                                         forms)
                                :to-be nil))))))))
        (funcall check-file "macher-agent-core.el")
        (funcall check-file "macher-agent-api.el")))))

(provide 'macher-agent-plugin-test)
;;; macher-agent-plugin-test.el ends here