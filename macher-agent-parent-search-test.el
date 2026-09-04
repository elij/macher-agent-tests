;;; macher-agent-parent-search-test.el --- Tests for Parent Conversation History Search -*- lexical-binding: t; -*-

(let* ((file (or load-file-name buffer-file-name))
       (root-dir (locate-dominating-file (or file default-directory) "macher-agent.el"))
       (test-dir (cond
                  ((and file (file-exists-p (expand-file-name "macher-agent-test-setup.el" (file-name-directory (expand-file-name file)))))
                   (file-name-directory (expand-file-name file)))
                  ((file-exists-p (expand-file-name "macher-agent-test-setup.el" default-directory))
                   (expand-file-name default-directory))
                  ((file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" default-directory))
                   (expand-file-name "tests" default-directory))
                  ((and root-dir (file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" root-dir)))
                   (expand-file-name "tests" root-dir))
                  (t (or (locate-dominating-file default-directory "tests") default-directory)))))
  (when root-dir
    (add-to-list 'load-path (expand-file-name root-dir)))
  (add-to-list 'load-path (expand-file-name test-dir))
  (add-to-list 'load-path (expand-file-name "helpers" test-dir)))

(require 'macher-agent-test-setup)
(require 'macher-agent-zero-mem)
(require 'macher-agent-gptel)
(require 'macher-agent-orchestration)

(describe "Parent Conversation History Retrieval and Search"
  (macher-agent-test-setup-before-each)

  (before-each
    (let* ((root (locate-dominating-file (or load-file-name buffer-file-name default-directory) "macher-agent.el"))
           (script (expand-file-name "skills/scripts/search_parent_conversation_history.el" (or root default-directory))))
      (load script nil t)))

  (after-each
    (when (fboundp 'macher-agent-zero-mem-uninstall)
      (macher-agent-zero-mem-uninstall))
    (setq macher-agent-search-backend-function #'macher-agent-search-glob))

  (describe "1. Parent Buffer Detection and Tool Injection"
    (it "injects search_parent_conversation_history tool when a valid live parent buffer exists"
      (let* ((parent-buf (generate-new-buffer "*macher-test: parent-orchestrator*"))
             (child-buf (generate-new-buffer "*macher-test: subagent-worker*")))
        (unwind-protect
            (progn
              (with-current-buffer child-buf
                (macher-agent--push-routing "task-uuid-101" (buffer-name parent-buf)))
              (let* ((state (make-macher-agent-transmission-state
                             :target-buffer child-buf
                             :tools nil))
                     (updated-state (macher-agent-parent-memory-pipe--inject-tool
                                     state child-buf nil nil nil))
                     (tool-names (mapcar #'macher-agent-canonical-tool-name
                                         (macher-agent-transmission-state-tools updated-state))))
                (expect (member "search_parent_conversation_history" tool-names) :to-be-truthy)))
          (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
          (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

    (it "ensures tool injection is idempotent and does not duplicate tools"
      (let* ((parent-buf (generate-new-buffer "*macher-test: parent-orchestrator-idemp*"))
             (child-buf (generate-new-buffer "*macher-test: subagent-worker-idemp*")))
        (unwind-protect
            (progn
              (with-current-buffer child-buf
                (macher-agent--push-routing "task-uuid-102" (buffer-name parent-buf)))
              (let* ((state (make-macher-agent-transmission-state
                             :target-buffer child-buf
                             :tools nil))
                     (state1 (macher-agent-parent-memory-pipe--inject-tool
                              state child-buf nil nil nil))
                     (state2 (macher-agent-parent-memory-pipe--inject-tool
                              state1 child-buf nil nil nil))
                     (tool-names (mapcar #'macher-agent-canonical-tool-name
                                         (macher-agent-transmission-state-tools state2)))
                     (occurrences (cl-count "search_parent_conversation_history"
                                            tool-names :test #'equal)))
                (expect occurrences :to-equal 1)))
          (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
          (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

    (it "resolves parent buffer via :originator-buffer in routing frame"
      (let* ((parent-buf (generate-new-buffer "*macher-test: parent-orig-buf*"))
             (child-buf (generate-new-buffer "*macher-test: child-orig-buf*")))
        (unwind-protect
            (progn
              (with-current-buffer child-buf
                (setq-local macher-agent--routing-stack
                            (list (list :task-id "task-uuid-orig-buf" :originator-buffer parent-buf))))
              (expect (macher-agent-zero-mem--resolve-parent-buffer child-buf nil) :to-equal parent-buf))
          (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
          (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

    (it "resolves parent buffer via context plist properties"
      (let* ((parent-buf (generate-new-buffer "*macher-test: parent-ctx-prop*"))
             (child-buf (generate-new-buffer "*macher-test: child-ctx-prop*"))
             (ctx1 (make-macher-agent-context :plugins (list :origin-buffer parent-buf)))
             (ctx2 (make-macher-agent-context :plugins (list :originator-buffer (buffer-name parent-buf)))))
        (unwind-protect
            (progn
              (expect (macher-agent-zero-mem--resolve-parent-buffer
                       child-buf ctx1)
                      :to-equal parent-buf)
              (expect (macher-agent-zero-mem--resolve-parent-buffer
                       child-buf ctx2)
                      :to-equal parent-buf))
          (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
          (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

    (it "injects search_parent_conversation_history tool when unpacked from state context and target-buffer"
      (let* ((parent-buf (generate-new-buffer "*macher-test: parent-state-ctx*"))
             (child-buf (generate-new-buffer "*macher-test: child-state-ctx*"))
             (ctx (make-macher-agent-context :origin-buffer parent-buf)))
        (unwind-protect
            (let* ((state (make-macher-agent-transmission-state
                           :target-buffer child-buf
                           :context ctx
                           :tools nil))
                   (updated-state (macher-agent-parent-memory-pipe--inject-tool
                                   state nil nil nil nil))
                   (tool-names (mapcar #'macher-agent-canonical-tool-name
                                       (macher-agent-transmission-state-tools updated-state))))
              (expect (member "search_parent_conversation_history" tool-names) :to-be-truthy))
          (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
          (when (buffer-live-p child-buf) (kill-buffer child-buf))))))

  (describe "2. Isolation and Root Buffers"
    (it "does not inject search_parent_conversation_history tool for root buffers without parent"
      (let* ((root-buf (generate-new-buffer "*macher-test: root-orchestrator*")))
        (unwind-protect
            (progn
              (let* ((state (make-macher-agent-transmission-state
                             :target-buffer root-buf
                             :tools nil))
                     (updated-state (macher-agent-parent-memory-pipe--inject-tool
                                     state root-buf nil nil nil))
                     (tool-names (mapcar #'macher-agent-canonical-tool-name
                                         (macher-agent-transmission-state-tools updated-state))))
                (expect (member "search_parent_conversation_history" tool-names) :to-be nil)))
          (when (buffer-live-p root-buf) (kill-buffer root-buf)))))

    (it "does not inject tool when parent buffer is killed or invalid"
      (let* ((parent-buf (generate-new-buffer "*macher-test: parent-to-kill*"))
             (parent-name (buffer-name parent-buf))
             (child-buf (generate-new-buffer "*macher-test: child-orphan*")))
        (kill-buffer parent-buf)
        (unwind-protect
            (progn
              (with-current-buffer child-buf
                (macher-agent--push-routing "task-uuid-103" parent-name))
              (let* ((state (make-macher-agent-transmission-state
                             :target-buffer child-buf
                             :tools nil))
                     (updated-state (macher-agent-parent-memory-pipe--inject-tool
                                     state child-buf nil nil nil))
                     (tool-names (mapcar #'macher-agent-canonical-tool-name
                                         (macher-agent-transmission-state-tools updated-state))))
                (expect (member "search_parent_conversation_history" tool-names) :to-be nil)))
          (when (buffer-live-p child-buf) (kill-buffer child-buf))))))

  (describe "3. Instruction-based Parent Context Extraction and Directive Injection"
    (it "extracts parent context traces using zero-mem retrieval and injects parent_conversation_context directive"
      (let* ((parent-buf (generate-new-buffer "*macher-test: parent-context-buf*"))
             (child-buf (generate-new-buffer "*macher-test: child-context-buf*")))
        (unwind-protect
            (progn
              (with-current-buffer parent-buf
                (insert "Turn 1: Initializing Redis Cluster deployment on port 6379.\nTurn 2: Setting up sentinel failover configuration.\nTurn 3: Unrelated discussion about typography and themes.\n"))
              (with-current-buffer child-buf
                (macher-agent--push-routing "task-uuid-104" (buffer-name parent-buf))
                (insert "Deploy and configure Redis Cluster failover."))
              (let* ((state (make-macher-agent-transmission-state
                             :target-buffer child-buf
                             :directives nil))
                     (updated-state (macher-agent-pipe--inject-parent-context
                                     state child-buf nil nil nil))
                     (dirs (macher-agent-transmission-state-directives updated-state)))
                (expect (length dirs) :to-be-greater-than 0)
                (let ((directive-text (string-join dirs "\n\n")))
                  (expect directive-text :to-match "<parent_conversation_context>")
                  (expect directive-text :to-match "</parent_conversation_context>")
                  (expect directive-text :to-match "<trace id=\"[0-9]+\">")
                  (expect directive-text :to-match "Redis Cluster"))))
          (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
          (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

    (it "injects parent context directive when unpacked from state context and target-buffer"
      (let* ((parent-buf (generate-new-buffer "*macher-test: parent-state-dir*"))
             (child-buf (generate-new-buffer "*macher-test: child-state-dir*"))
             (ctx (make-macher-agent-context :origin-buffer parent-buf
                                             :prompt "Deploy and configure Redis Cluster failover.")))
        (unwind-protect
            (progn
              (with-current-buffer parent-buf
                (insert "Turn 1: Initializing Redis Cluster deployment on port 6379.\nTurn 2: Setting up sentinel failover configuration.\nTurn 3: Unrelated discussion about typography and themes.\n"))
              (let* ((state (make-macher-agent-transmission-state
                             :target-buffer child-buf
                             :context ctx
                             :directives nil))
                     (updated-state (macher-agent-pipe--inject-parent-context
                                     state nil nil nil nil))
                     (dirs (macher-agent-transmission-state-directives updated-state)))
                (expect (length dirs) :to-be-greater-than 0)
                (let ((directive-text (string-join dirs "\n\n")))
                  (expect directive-text :to-match "<parent_conversation_context>")
                  (expect directive-text :to-match "</parent_conversation_context>")
                  (expect directive-text :to-match "Redis Cluster"))))
          (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
          (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

    (it "injects directive instructing sub-agent when search_parent_conversation_history is present in tools"
      (let* ((tool (or (bound-and-true-p macher-agent-search-parent-conversation-history-tool)
                       'search_parent_conversation_history))
             (state (make-macher-agent-transmission-state
                     :tools (list tool)
                     :directives nil))
             (updated-state (macher-agent-parent-memory-pipe--inject-directive
                             state nil nil nil nil))
             (dirs (macher-agent-transmission-state-directives updated-state)))
        (expect (length dirs) :to-equal 1)
        (expect (car dirs) :to-match "search_parent_conversation_history")))

    (it "does not inject parent search directive when search_parent_conversation_history tool is absent"
      (let* ((state (make-macher-agent-transmission-state
                     :tools (list 'other_tool)
                     :directives nil))
             (updated-state (macher-agent-parent-memory-pipe--inject-directive
                             state nil nil nil nil))
             (dirs (macher-agent-transmission-state-directives updated-state)))
        (expect dirs :to-be nil))))

  (describe "4. Tool Script Execution"
    (it "returns a clean error message when parent buffer is unavailable or killed"
      (let* ((child-buf (generate-new-buffer "*macher-test: child-dead-parent*"))
             (cmd-fn (get 'macher-agent-search-parent-conversation-history-tool 'ptc-function)))
        (unwind-protect
            (progn
              (with-current-buffer child-buf
                (macher-agent--push-routing "task-uuid-106" "*nonexistent-parent-buffer*"))
              (with-current-buffer child-buf
                (let ((result (funcall cmd-fn '(:query "any keywords" :context_lines 2) nil nil)))
                  (expect result :to-match "^Error:"))))
          (when (buffer-live-p child-buf) (kill-buffer child-buf))))))

  (describe "5. Pipeline Lifecycle Integration"
    (it "registers and unregisters parent pipeline steps via macher-agent-zero-mem-install and uninstall"
      (let ((saved-registry (copy-hash-table macher-agent-pipeline-registry)))
        (unwind-protect
            (progn
              (clrhash macher-agent-pipeline-registry)
              (macher-agent-zero-mem-install)
              (let ((steps (macher-agent-get-pipeline-steps 'transmission)))
                (expect (member #'macher-agent-parent-memory-pipe--inject-tool steps) :to-be-truthy)
                (expect (member #'macher-agent-pipe--inject-parent-context steps) :to-be-truthy)
                (expect (member #'macher-agent-parent-memory-pipe--inject-directive steps) :to-be-truthy))
              (macher-agent-zero-mem-uninstall)
              (let ((steps (macher-agent-get-pipeline-steps 'transmission)))
                (expect (member #'macher-agent-parent-memory-pipe--inject-tool steps) :to-be nil)
                (expect (member #'macher-agent-pipe--inject-parent-context steps) :to-be nil)
                (expect (member #'macher-agent-parent-memory-pipe--inject-directive steps) :to-be nil)))
          (setq macher-agent-pipeline-registry saved-registry)))))

  (describe "6. Unbound Variable Defense and Tool Normalisation"
    (it "does not signal void-variable when macher-agent--routing-stack is unbound during parent buffer resolution"
      (let ((buf (generate-new-buffer "*macher-test: unbound-resolve*"))
            (stack-bound (boundp 'macher-agent--routing-stack))
            (saved-val (when (boundp 'macher-agent--routing-stack)
                         (default-value 'macher-agent--routing-stack))))
        (unwind-protect
            (progn
              (makunbound 'macher-agent--routing-stack)
              (expect (boundp 'macher-agent--routing-stack) :to-be nil)
              (expect (macher-agent-zero-mem--resolve-parent-buffer buf nil) :to-be nil))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when stack-bound
            (set-default 'macher-agent--routing-stack saved-val)))))

    (it "does not signal void-variable when executing search_parent_conversation_history with unbound macher-agent--routing-stack"
      (let ((buf (generate-new-buffer "*macher-test: unbound-script*"))
            (cmd-fn (get 'macher-agent-search-parent-conversation-history-tool 'ptc-function))
            (stack-bound (boundp 'macher-agent--routing-stack))
            (saved-val (when (boundp 'macher-agent--routing-stack)
                         (default-value 'macher-agent--routing-stack))))
        (unwind-protect
            (progn
              (makunbound 'macher-agent--routing-stack)
              (with-current-buffer buf
                (let ((result (funcall cmd-fn '(:query "test query" :context_lines 2) nil nil)))
                  (expect result :to-match "^Error:"))))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when stack-bound
            (set-default 'macher-agent--routing-stack saved-val)))))

    (it "does not signal void-variable when injecting parent memory tool with unbound routing stack"
      (let ((buf (generate-new-buffer "*macher-test: unbound-inject*"))
            (stack-bound (boundp 'macher-agent--routing-stack))
            (saved-val (when (boundp 'macher-agent--routing-stack)
                         (default-value 'macher-agent--routing-stack))))
        (unwind-protect
            (progn
              (makunbound 'macher-agent--routing-stack)
              (let* ((state (make-macher-agent-transmission-state
                             :target-buffer buf
                             :tools nil))
                     (updated-state (macher-agent-parent-memory-pipe--inject-tool
                                     state buf nil nil nil)))
                (expect (macher-agent-transmission-state-tools updated-state) :to-be nil)))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when stack-bound
            (set-default 'macher-agent--routing-stack saved-val)))))

    (it "properly normalises tool names in macher-agent-memory-pipe--inject-tool using macher-agent-canonical-tool-name"
      (let* ((buf (generate-new-buffer "*macher-test: canonical-tool-test*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (insert (make-string 5000 ?x)))
              ;; Test with symbol tool already present
              (let* ((state-sym (make-macher-agent-transmission-state
                                 :target-buffer buf
                                 :tools (list 'search_conversation_history)))
                     (res-sym (macher-agent-memory-pipe--inject-tool state-sym buf nil nil nil)))
                (expect (length (macher-agent-transmission-state-tools res-sym)) :to-equal 1))
              ;; Test with plist tool already present
              (let* ((state-plist (make-macher-agent-transmission-state
                                   :target-buffer buf
                                   :tools (list '(:name "search_conversation_history"))))
                     (res-plist (macher-agent-memory-pipe--inject-tool state-plist buf nil nil nil)))
                (expect (length (macher-agent-transmission-state-tools res-plist)) :to-equal 1)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(provide 'macher-agent-parent-search-test)
;;; macher-agent-parent-search-test.el ends here
