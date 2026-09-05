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
   (when (fboundp 'macher-agent-sandbox-uninstall)
     (macher-agent-sandbox-uninstall))
   (setq macher-agent-search-backend-function #'macher-agent-search-glob))
  (after-all
   (when (fboundp 'macher-agent-zero-mem-uninstall)
     (macher-agent-zero-mem-uninstall))
   (when (fboundp 'macher-agent-sandbox-uninstall)
     (macher-agent-sandbox-uninstall))
   (setq macher-agent-search-backend-function #'macher-agent-search-glob))

  (describe "1. Core Routing and Pipeline Registry"
    (it "initialises macher-agent-pipeline-registry as a hash table"
      (expect (hash-table-p macher-agent-pipeline-registry) :to-be t))

    (it "registers pipeline steps in strict priority depth order and rejects non-symbol keys"
      (let ((sym-pipe (intern "test-priority-pipe")))
        (macher-agent-register-pipeline-step sym-pipe #'ignore 90)
        (macher-agent-register-pipeline-step sym-pipe #'identity 10)
        (macher-agent-register-pipeline-step sym-pipe #'car 50)
        (expect (macher-agent-get-pipeline-steps sym-pipe)
                :to-equal (list #'identity #'car #'ignore))
        (macher-agent-register-pipeline-step sym-pipe #'ignore 20)
        (expect (macher-agent-get-pipeline-steps sym-pipe)
                :to-equal (list #'identity #'ignore #'car))
        ;; Pipeline step registration requires symbol key, not string
        (expect (macher-agent-register-pipeline-step (symbol-name sym-pipe) #'identity 5)
                :to-throw 'wrong-type-argument)))

    (it "bridges user interface buffers to state machine with buffer-local macher-agent-fsm-id"
      (with-temp-buffer
        (setq-local macher-agent-fsm-id "fsm-session-12345")
        (expect (local-variable-p 'macher-agent-fsm-id) :to-be t)
        (expect macher-agent-fsm-id :to-equal "fsm-session-12345"))
      (with-temp-buffer
        (expect macher-agent-fsm-id :to-be nil)))

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
              (let* ((state-plist (list :a2a-msg (make-macher-agent-transit-payload :task-id "task-p1" :metadata (list :buffer_name "test-a2a-child-buf"))
                                        :target "test-a2a-child-buf"
                                        :target-name "test-a2a-child-buf"
                                        :shared-state (list :parent-buffer parent-buf)))
                     (res-plist (macher-agent-a2a-pipe--acquire-target state-plist)))
                (expect (plist-get res-plist :child-buf) :to-be child-buf)
                (with-current-buffer child-buf
                  (expect (macher-agent-valid-context-p macher-agent--persistent-context) :to-be-truthy)
                  (expect (macher-agent-context-workspace macher-agent--persistent-context) :to-equal workspace))))
          (kill-buffer parent-buf)
          (kill-buffer child-buf)
          (delete-directory mock-dir t)))))

  (describe "2. Plugin Lifecycle (Install and Uninstall)"
    (it "installs all core and plugin pipeline steps, hooks, and dynamic loaders via macher-agent-install"
      (clrhash macher-agent-pipeline-registry)
      (setq macher-agent-task-flush-hook nil)
      (setq macher-agent-vfs-flush-hook nil)
      (macher-agent-install)
      ;; Core and VFS steps
      (expect (member #'macher-agent-vfs--merge-payload (macher-agent-get-pipeline-steps 'payload-merge)) :to-be-truthy)
      (expect (member #'macher-agent-ctx-pipe--explicit (macher-agent-get-pipeline-steps 'context-resolution)) :to-be-truthy)
      (expect (member #'macher-agent-resolve-from-transit-payload (macher-agent-get-pipeline-steps 'context-resolution)) :to-be-truthy)
      ;; Sandbox / PTC steps
      (expect (member #'macher-agent-ptc--inject-tool (macher-agent-get-pipeline-steps 'preset-composition)) :to-be-truthy)
      (expect (member #'macher-agent-sandbox-append-ptc-to-transmission (macher-agent-get-pipeline-steps 'transmission)) :to-be-truthy)
      ;; Zero-Mem steps
      (expect (member #'macher-agent-memory-pipe--inject-tool (macher-agent-get-pipeline-steps 'transmission)) :to-be-truthy)
      (expect (member #'macher-agent-memory-pipe--truncate-buffer (macher-agent-get-pipeline-steps 'transmission)) :to-be-truthy)
      (expect (member #'macher-agent-memory-pipe--inject-directive (macher-agent-get-pipeline-steps 'transmission)) :to-be-truthy)
      ;; Flush & Mutation hooks
      (expect (member #'macher-agent-memory--persist-interaction macher-agent-task-flush-hook) :to-be-truthy)
      (expect (member #'macher-agent-vfs-handle-flush macher-agent-task-flush-hook) :to-be-truthy)
      (expect (member #'macher-agent-vfs-build-patch-from-hook macher-agent-vfs-flush-hook) :to-be-truthy)
      (expect (member #'macher-agent--mutation-dispatcher macher-agent-context-mutated-hook) :to-be-truthy))

    (it "invokes plugin loaders directly via macher-agent-install"
      (let ((vfs-called nil)
            (sandbox-called nil)
            (zero-mem-called nil)
            (ctx-called nil)
            (trans-called nil))
        (cl-letf (((symbol-function 'macher-agent-vfs-install) (lambda () (setq vfs-called t)))
                  ((symbol-function 'macher-agent-sandbox-install) (lambda () (setq sandbox-called t)))
                  ((symbol-function 'macher-agent-zero-mem-install) (lambda () (setq zero-mem-called t)))
                  ((symbol-function 'macher-agent-context-resolution-install) (lambda () (setq ctx-called t)))
                  ((symbol-function 'macher-agent-transmission-install) (lambda () (setq trans-called t))))
          (macher-agent-install)
          (expect vfs-called :to-be t)
          (expect sandbox-called :to-be t)
          (expect zero-mem-called :to-be t)
          (expect ctx-called :to-be t)
          (expect trans-called :to-be t))))

    (it "manages VFS pipeline and flush hook registration via macher-agent-vfs-install"
      (clrhash macher-agent-pipeline-registry)
      (setq macher-agent-task-flush-hook nil)
      (setq macher-agent-vfs-flush-hook nil)
      (macher-agent-vfs-install)
      (expect (member #'macher-agent-vfs--merge-payload (macher-agent-get-pipeline-steps 'payload-merge)) :to-be-truthy)
      (let* ((entries (gethash 'payload-merge macher-agent-pipeline-registry))
             (entry (cl-find #'macher-agent-vfs--merge-payload entries :key (lambda (e) (plist-get e :step)))))
        (expect (plist-get entry :priority) :to-equal 10))
      (expect (member #'macher-agent-vfs-handle-flush macher-agent-task-flush-hook) :to-be-truthy)
      (expect (member #'macher-agent-vfs-build-patch-from-hook macher-agent-vfs-flush-hook) :to-be-truthy))

    (it "installs and uninstalls sandbox pipeline steps cleanly"
      (let ((saved-registry (copy-hash-table macher-agent-pipeline-registry)))
        (unwind-protect
            (progn
              (clrhash macher-agent-pipeline-registry)
              (macher-agent-sandbox-install)
              (expect (member #'macher-agent-ptc--inject-tool (macher-agent-get-pipeline-steps 'preset-composition)) :to-be-truthy)
              (expect (member #'macher-agent-sandbox-append-ptc-to-transmission (macher-agent-get-pipeline-steps 'transmission)) :to-be-truthy)
              (when (fboundp 'macher-agent-sandbox-uninstall)
                (macher-agent-sandbox-uninstall)
                (expect (member #'macher-agent-ptc--inject-tool (macher-agent-get-pipeline-steps 'preset-composition)) :to-be nil)
                (expect (member #'macher-agent-sandbox-append-ptc-to-transmission (macher-agent-get-pipeline-steps 'transmission)) :to-be nil)))
          (setq macher-agent-pipeline-registry saved-registry))))

    (it "installs and uninstalls zero-mem pipeline steps, hooks, and search backend cleanly"
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
          (setq macher-agent-pipeline-registry saved-registry)))))

  (describe "3. Polymorphic Plugin State Accessors"
    (it "reads and writes plugin state on context structs"
      (dolist (spec (list (list :getter #'macher-agent-sandbox-get-state
                                :setter #'macher-agent-sandbox-set-state
                                :key :sandbox
                                :val1 '(:active-primitives (spawn-subagent))
                                :val2 '(:active-primitives (read-file)))
                          (list :getter #'macher-agent-zero-mem-get-state
                                :setter #'macher-agent-zero-mem-set-state
                                :key :zero-mem
                                :val1 '(:traces ((:id 1 :text "trace-a")))
                                :val2 '(:traces ((:id 2 :text "trace-b"))))))
        (let ((getter (plist-get spec :getter))
              (setter (plist-get spec :setter))
              (key (plist-get spec :key))
              (val1 (plist-get spec :val1))
              (val2 (plist-get spec :val2)))
          ;; Context struct
          (let ((ctx (make-macher-agent-context :id "ctx-poly" :plugins '(:preserved "val"))))
            (expect (funcall getter ctx) :to-be nil)
            (funcall setter ctx val1)
            (expect (funcall getter ctx) :to-equal val1)
            (expect (plist-get (macher-agent-context-plugins ctx) key) :to-equal val1)
            (expect (plist-get (macher-agent-context-plugins ctx) :preserved) :to-equal "val"))))))

    (it "strictly enforces context structures and rejects alists in sandbox state accessors"
      (let ((alist-ctx '((:sandbox . (:active-primitives (spawn-subagent))))))
        (expect (macher-agent-sandbox-get-state alist-ctx) :to-throw 'wrong-type-argument)
        (expect (macher-agent-sandbox-set-state alist-ctx '(:active-primitives (spawn-subagent))) :to-throw 'wrong-type-argument)))

    (it "resolves context comprehensively across transit keys, wrapper states, and buffers"
      (let* ((mock-dir (make-temp-file "macher-transit-test" t))
             (workspace (make-macher-agent-workspace :project-root mock-dir))
             (ctx (macher-agent--make-vfs-context :workspace workspace :contents nil)))
        (unwind-protect
            (dolist (key '(:target-context :parent-context :child-context))
              (let* ((payload (cond
                               ((eq key :target-context) (make-macher-agent-transit-payload :target-context ctx))
                               ((eq key :parent-context) (make-macher-agent-transit-payload :parent-context ctx))
                               (t (make-macher-agent-transit-payload :child-context ctx))))
                     (resolved (macher-agent-context-from-payload payload)))
                (expect resolved :to-be ctx)))
          (delete-directory mock-dir t))))

    (it "verifies macher-agent--extract-fsm-context is removed in favor of explicit context transport"
      (expect (fboundp 'macher-agent--extract-fsm-context) :to-be nil)))

  (describe "4. Storage and Virtual File System"
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
          (delete-directory mock-dir t)))))

  (describe "5. Dynamic Pipeline Execution and Dispatching"
    (it "executes dynamically registered steps during macher-agent--compile-transmission-payload"
      (with-temp-buffer
        (let* ((dyn-tx-called nil)
               (dyn-tx-step (lambda (state)
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

    (it "chains multiple reducer steps sequentially and merges child diffs during payload-merge in bind closure"
      (let* ((mock-dir (make-temp-file "macher-chain-merge-test" t))
             (workspace (make-macher-agent-workspace :project-root mock-dir))
             (parent-ctx (macher-agent--make-vfs-context :workspace workspace :contents nil))
             (child-ctx (macher-agent--make-vfs-context :workspace workspace
                                                        :contents (list (macher-agent-vfs-make-entry "merged-file.el" "initial" "initial"))))
             (parent-buf (generate-new-buffer "test-chain-parent"))
             (child-buf (generate-new-buffer "test-chain-child"))
             (results-tbl (make-hash-table :test 'equal))
             (task-id "task-chain-merge")
             (step-one (lambda (msg)
                         (if (macher-agent-transit-payload-p msg)
                             (let ((pl (macher-agent-transit-payload-payload msg)))
                               (when (listp pl)
                                 (setf (macher-agent-transit-payload-payload msg)
                                       (plist-put pl :step-one-flag t)))
                               msg)
                           (plist-put (copy-sequence msg) :step-one-flag t))))
             (step-two (lambda (msg)
                         (if (macher-agent-transit-payload-p msg)
                             (let ((pl (macher-agent-transit-payload-payload msg)))
                               (when (listp pl)
                                 (setf (macher-agent-transit-payload-payload msg)
                                       (plist-put pl :step-two-flag t)))
                               msg)
                           (plist-put (copy-sequence msg) :step-two-flag t)))))
        (macher-agent-register-pipeline-step 'payload-merge step-one 12)
        (macher-agent-register-pipeline-step 'payload-merge step-two 14)
        (unwind-protect
            (progn
              (with-current-buffer parent-buf
                (setq macher-agent--persistent-context parent-ctx))
              (with-current-buffer child-buf
                (setq macher-agent--persistent-context child-ctx))
              (let* ((shared-state (list :results results-tbl
                                         :total 1
                                         :final-callback #'ignore
                                         :parent-buffer parent-buf
                                         :original-payloads (list (make-macher-agent-transit-payload :task-id task-id))))
                     (initial-state (list :a2a-msg (make-macher-agent-transit-payload :type 'SEND_MESSAGE :task-id task-id)
                                          :child-buf child-buf
                                          :shared-state shared-state))
                     (_res-state (macher-agent-a2a-pipe--bind-closure initial-state))
                     (cb (gethash task-id macher-agent--pending-callbacks)))
                (expect cb :not :to-be nil)
                (macher-agent--update-context-file child-ctx "merged-file.el" "updated-content")
                (let ((payload (make-macher-agent-transit-payload
                                :type 'ARTIFACT_UPDATE
                                :task-id task-id
                                :child-context child-ctx
                                :payload (list :diff (list (make-macher-agent-vfs-entry :path "merged-file.el" :orig "initial" :curr "updated-content"))
                                               :data "chain-result"
                                               :status 'success
                                               :buffer-name (buffer-name child-buf)
                                               :shared-state shared-state))))
                  (funcall cb payload))
                ;; Assert diff merge into parent context
                (expect (macher-agent--read-context-file parent-ctx "merged-file.el") :to-equal "updated-content")
                ;; Assert sequential reducer step propagation
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
          (kill-buffer child-buf)
          (delete-directory mock-dir t))))

    (it "assesses primitives and injects ptc_execution tool when primitives are active"
      (let* ((state (list :tools (list 'search_in_workspace)
                          :ptc-primitives (list 'spawn-subagent)))
             (updated (macher-agent-ptc--inject-tool state)))
        (let ((tool-names (mapcar (lambda (tl)
                                    (if (symbolp tl) (symbol-name tl) (gptel-tool-name tl)))
                                  (plist-get updated :tools))))
          (expect (member "ptc_execution" tool-names) :to-be-truthy)))
      (let* ((state (list :tools (list 'search_in_workspace)
                          :ptc-primitives nil))
             (macher-agent--active-ptc-primitives nil)
             (updated (macher-agent-ptc--inject-tool state)))
        (expect (plist-get updated :tools) :to-equal (list 'search_in_workspace))))

    (it "calculates memory limits and persists conversation history without premature task flush"
      (with-temp-buffer
        (insert (make-string 3000 ?a))
        (let* ((macher-agent-max-context-chars '((nil . 500)))
               (state (make-macher-agent-transmission-state :target-buffer (current-buffer)
                                                            :tools nil))
               (updated (macher-agent-memory-pipe--inject-tool state)))
          (expect (member "search_conversation_history"
                          (mapcar #'macher-agent-canonical-tool-name
                                  (macher-agent-transmission-state-tools updated)))
                  :to-be-truthy)))
      (with-temp-buffer
        (insert "short content")
        (let* ((macher-agent-max-context-chars '((nil . 50000)))
               (state (make-macher-agent-transmission-state :target-buffer (current-buffer)
                                                            :tools nil))
               (updated (macher-agent-memory-pipe--inject-tool state)))
          (expect (member "search_conversation_history"
                          (mapcar #'macher-agent-canonical-tool-name
                                  (macher-agent-transmission-state-tools updated)))
                  :to-be nil)))
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
               task-id msg-body results-ht 1 (list (make-macher-agent-transit-payload :task-id task-id)) final-cb parent-buf)
              (expect hook-called :to-be nil)
              (expect callback-result :to-equal (vector msg-body)))
          (remove-hook 'macher-agent-task-flush-hook hook-fn)
          (kill-buffer parent-buf)))))

  (describe "6. Decoupling and Module Independence Invariants"
    (it "contains zero internal declare-function forms across decoupled modules"
      (dolist (file-name '("macher-agent-sandbox.el" "macher-agent-api.el" "macher-agent-orchestration.el"))
        (let* ((file (or (locate-library file-name)
                         (expand-file-name file-name default-directory)))
               (forms nil))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (condition-case nil
                (while t
                  (push (read (current-buffer)) forms))
              (end-of-file nil)))
          (let ((internal-declares
                 (cl-remove-if-not
                  (lambda (form)
                    (and (consp form)
                         (eq (car form) 'declare-function)
                         (let ((fn (cadr form)))
                           (when (and (consp fn) (eq (car fn) 'quote))
                             (setq fn (cadr fn)))
                           (and (symbolp fn)
                                (string-prefix-p "macher-agent-" (symbol-name fn))))))
                  forms)))
            (expect internal-declares :to-equal nil))))))

(provide 'macher-agent-plugin-test)
;;; macher-agent-plugin-test.el ends here
