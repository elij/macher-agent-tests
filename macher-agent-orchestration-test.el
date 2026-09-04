;;; macher-agent-orchestration-test.el --- Tests for macher-agent orchestration -*- lexical-binding: t; -*-

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
(require 'macher-agent-orchestration)

(describe "Macher Agent Orchestration Suite"
          (macher-agent-test-setup-before-each)

          ;; ---------------------------------------------------------------------
          ;; 1. Routing Stack Mechanics
          ;; ---------------------------------------------------------------------
          (describe "Routing Stack Mechanics"
                    (it "pushes routing frames and updates buffer-local task state"
                        (let ((buf (get-buffer-create "*orch-route-test*")))
                          (unwind-protect
                              (with-current-buffer buf
                                (expect macher-agent--routing-stack :to-be nil)
                                (let ((frame (macher-agent--push-routing "task-101" "parent-agent" t)))
                                  (expect frame :to-equal '(:task-id "task-101" :originator-name "parent-agent" :suppress-patch t))
                                  (expect macher-agent--current-task-id :to-equal "task-101")
                                  (expect macher-agent--suppress-patch :to-be t)
                                  (expect (car macher-agent--routing-stack) :to-equal frame))
                                ;; Push nested frame
                                (let ((frame2 (macher-agent--push-routing "task-102" "peer-agent" nil)))
                                  (expect (length macher-agent--routing-stack) :to-equal 2)
                                  (expect macher-agent--current-task-id :to-equal "task-102")
                                  (expect macher-agent--suppress-patch :to-be nil)
                                  (expect (car macher-agent--routing-stack) :to-equal frame2)))
                            (kill-buffer buf))))

                    (it "restores previous routing frame after popping frame"
                        (let ((buf (get-buffer-create "*orch-pop-test*")))
                          (unwind-protect
                              (with-current-buffer buf
                                (macher-agent--push-routing "task-201" "agent-alpha" t)
                                (macher-agent--push-routing "task-202" "agent-beta" nil)
                                (expect macher-agent--current-task-id :to-equal "task-202")
                                (expect macher-agent--suppress-patch :to-be nil)
                                ;; Pop top frame (task-202) and restore previous frame (task-201)
                                (let ((popped (macher-agent--pop-routing)))
                                  (expect (plist-get popped :task-id) :to-equal "task-202")
                                  (expect (plist-get popped :originator-name) :to-equal "agent-beta")
                                  (expect (length macher-agent--routing-stack) :to-equal 1)
                                  (expect macher-agent--current-task-id :to-equal "task-201")
                                  (expect macher-agent--suppress-patch :to-be t))
                                ;; Pop remaining frame
                                (let ((popped2 (macher-agent--pop-routing)))
                                  (expect (plist-get popped2 :task-id) :to-equal "task-201")
                                  (expect macher-agent--routing-stack :to-be nil)
                                  (expect macher-agent--current-task-id :to-be nil)
                                  (expect macher-agent--suppress-patch :to-be nil)))
                            (kill-buffer buf))))

                    (it "maintains permanent-local persistence across mode shifts"
                        (expect (get 'macher-agent--routing-stack 'permanent-local) :to-be t)
                        (expect (get 'macher-agent--current-task-id 'permanent-local) :to-be t)
                        (expect (get 'macher-agent--suppress-patch 'permanent-local) :to-be t)))

          ;; ---------------------------------------------------------------------
          ;; 2. A2A Delegation and Dispatch Pipeline
          ;; ---------------------------------------------------------------------
          (describe "A2A Delegation and Dispatch Pipeline"
                    (it "normalizes payloads, dispatches through pipeline, and aggregates results on completion"
                        (let* ((parent-buf (get-buffer-create "*orch-parent*"))
                               (child-buf (get-buffer-create "*orch-child*"))
                               (final-results nil)
                               (payloads (list (macher-agent-make-a2a-payload
                                                :type 'SEND_MESSAGE
                                                :task-id "task-alpha"
                                                :payload "Execute alpha step"
                                                :metadata (list :buffer_name (buffer-name child-buf)
                                                                :background t)))))
                          (unwind-protect
                              (progn
                                (with-current-buffer parent-buf
                                  (cl-letf (((symbol-function 'gptel-send)
                                             (lambda ()
                                               (let* ((tid (bound-and-true-p macher-agent--current-task-id))
                                                      (cb (when tid (gethash tid macher-agent--pending-callbacks))))
                                                 (when cb
                                                   (funcall cb (list :status 'success :data "Alpha completed" :task-id tid)))))))
                                    (macher-agent-a2a-dispatch payloads (lambda (res) (setq final-results res)))))
                                (expect (vectorp final-results) :to-be t)
                                (expect (length final-results) :to-equal 1)
                                (expect (plist-get (aref final-results 0) :status) :to-be 'success)
                                (expect (plist-get (aref final-results 0) :data) :to-equal "Alpha completed"))
                            (kill-buffer parent-buf)
                            (kill-buffer child-buf))))

                    (it "pushes routing frame exactly once onto child buffer routing stack"
                        (let* ((parent-buf (get-buffer-create "*orch-single-parent*"))
                               (child-buf (get-buffer-create "*orch-single-child*"))
                               (stack-depth-during-run nil)
                               (payloads (list (macher-agent-make-a2a-payload
                                                :type 'SEND_MESSAGE
                                                :task-id "task-single-push"
                                                :payload "Single push verification"
                                                :metadata (list :buffer_name (buffer-name child-buf)
                                                                :suppress-patch t
                                                                :background t)))))
                          (unwind-protect
                              (progn
                                (with-current-buffer parent-buf
                                  (cl-letf (((symbol-function 'gptel-send)
                                             (lambda ()
                                               (with-current-buffer child-buf
                                                 (setq stack-depth-during-run
                                                       (length (bound-and-true-p macher-agent--routing-stack))))
                                               (let* ((tid (bound-and-true-p macher-agent--current-task-id))
                                                      (cb (when tid (gethash tid macher-agent--pending-callbacks))))
                                                 (when cb
                                                   (funcall cb (list :status 'success :data "Done" :task-id tid)))))))
                                    (macher-agent-a2a-dispatch payloads #'ignore)))
                                (expect stack-depth-during-run :to-equal 1))
                            (kill-buffer parent-buf)
                            (kill-buffer child-buf))))

                    (it "handles missing target buffers by returning structured error plists"
                        (let* ((results nil)
                               (payloads (list (macher-agent-make-a2a-payload
                                                :type 'SEND_MESSAGE
                                                :task-id "task-missing"
                                                :payload "Target nowhere"
                                                :metadata (list :buffer_name "nonexistent-target-buf")))))
                          (macher-agent-a2a-dispatch payloads (lambda (res) (setq results res)))
                          (expect (vectorp results) :to-be t)
                          (expect (plist-get (aref results 0) :status) :to-be 'error)
                          (expect (plist-get (aref results 0) :error) :to-match "Sub-agent buffer 'nonexistent-target-buf' not found")))

                    (it "invokes final-callback with an empty vector when payloads list is empty"
                        (let ((callback-result :not-called))
                          (macher-agent-a2a-dispatch nil (lambda (res) (setq callback-result res)))
                          (expect (vectorp callback-result) :to-be t)
                          (expect callback-result :to-equal [])))

                    (it "wakes suspended subagents awaiting callbacks without re-transmitting"
                        (let* ((target-buf (get-buffer-create "*orch-wake-target*"))
                               (wake-data nil)
                               (payloads (list (macher-agent-make-a2a-payload
                                                :type 'SEND_MESSAGE
                                                :task-id "task-wake"
                                                :payload "Resume instructions"
                                                :metadata (list :buffer_name (buffer-name target-buf))))))
                          (unwind-protect
                              (progn
                                (puthash (buffer-name target-buf) (lambda (msg) (setq wake-data msg)) macher-agent--pending-callbacks)
                                (macher-agent-a2a-dispatch payloads nil)
                                (expect wake-data :to-equal "Resume instructions")
                                (expect (gethash (buffer-name target-buf) macher-agent--pending-callbacks) :to-be nil))
                            (kill-buffer target-buf))))

                    (it "routes ARTIFACT_UPDATE directly to the registered pending callback"
                        (let* ((artifact-received nil)
                               (task-id "task-artifact-999")
                               (msg (macher-agent-make-a2a-payload
                                     :type 'ARTIFACT_UPDATE
                                     :task-id task-id
                                     :payload (list :status 'success :result "Artifact text"))))
                          (puthash task-id (lambda (res) (setq artifact-received res)) macher-agent--pending-callbacks)
                          (macher-agent-a2a-dispatch (list msg) nil)
                          (expect (macher-agent-transit-payload-type artifact-received) :to-equal 'ARTIFACT_UPDATE)
                          (expect (gethash task-id macher-agent--pending-callbacks) :to-be nil)))

                    (it "registers ownership paths in task and global ownership hash tables"
                        (let* ((parent-buf (get-buffer-create "*orch-owner-parent*"))
                               (child-buf (get-buffer-create "*orch-owner-child*"))
                               (parent-ctx (macher-agent--make-context :project-root "/mock/orch-proj/")))
                          (unwind-protect
                              (progn
                                (with-current-buffer parent-buf
                                  (setq-local macher-agent--persistent-context parent-ctx))
                                (let* ((state (list :a2a-msg (make-macher-agent-transit-payload
                                                              :task-id "task-owner-101"
                                                              :metadata (list :buffer_name (buffer-name child-buf)))
                                                    :shared-state (list :parent-buffer parent-buf)
                                                    :originator-name (buffer-name parent-buf)
                                                    :child-buf child-buf)))
                                  (macher-agent-a2a-pipe--register-ownership state)
                                  (expect (gethash "task-owner-101" macher-agent--task-registry) :to-equal (buffer-name parent-buf))
                                  (expect (member (buffer-name child-buf)
                                                  (gethash (buffer-name parent-buf) macher-agent--a2a-ownership))
                                          :to-be-truthy)))
                            (kill-buffer parent-buf)
                            (kill-buffer child-buf))))

                    (it "resolves callback collisions by assigning unique task-id when colliding with pending registry"
                        (let* ((existing-id "task-orch-collision")
                               (child-buf (get-buffer-create "*orch-collision-child*"))
                               (state (list :a2a-msg (make-macher-agent-transit-payload
                                                      :task-id existing-id
                                                      :metadata nil)
                                            :shared-state (list :results (make-hash-table :test 'equal)
                                                                :total 1
                                                                :final-callback nil
                                                                :parent-buffer (current-buffer)
                                                                :original-payloads nil)
                                            :child-buf child-buf)))
                          (puthash existing-id #'ignore macher-agent--pending-callbacks)
                          (unwind-protect
                              (let* ((res (macher-agent-a2a-pipe--bind-closure state))
                                     (msg (plist-get res :a2a-msg))
                                     (assigned-id (macher-agent-transit-payload-task-id msg)))
                                (expect assigned-id :not :to-equal existing-id)
                                (expect (string-prefix-p "task-" assigned-id) :to-be t)
                                (expect (gethash assigned-id macher-agent--pending-callbacks) :not :to-be nil))
                            (remhash existing-id macher-agent--pending-callbacks)
                            (kill-buffer child-buf))))

                    (it "aggregates sub-agent results with 7-argument signature without FSM parameters"
                        (let* ((parent-buf (get-buffer-create "*orch-agg-parent*"))
                               (results-tbl (make-hash-table :test 'equal))
                               (task-1 "task-agg-01")
                               (task-2 "task-agg-02")
                               (p1 (make-macher-agent-transit-payload :task-id task-1))
                               (p2 (make-macher-agent-transit-payload :task-id task-2))
                               (payloads (list p1 p2))
                               (cb-result nil)
                               (executed-in-buf nil)
                               (final-cb (lambda (res)
                                           (setq cb-result res)
                                           (setq executed-in-buf (current-buffer)))))
                          (unwind-protect
                              (progn
                                ;; First task completes
                                (macher-agent--aggregate-a2a-results
                                 task-1 '(:status success :data "Result 1") results-tbl 2 payloads final-cb parent-buf)
                                (expect cb-result :to-be nil)
                                ;; Second task completes, triggering callback in parent buffer
                                (macher-agent--aggregate-a2a-results
                                 task-2 '(:status success :data "Result 2") results-tbl 2 payloads final-cb parent-buf)
                                (expect (vectorp cb-result) :to-be t)
                                (expect (length cb-result) :to-equal 2)
                                (expect (plist-get (aref cb-result 0) :data) :to-equal "Result 1")
                                (expect (plist-get (aref cb-result 1) :data) :to-equal "Result 2")
                                (expect executed-in-buf :to-equal parent-buf))
                            (kill-buffer parent-buf)))))

          ;; ---------------------------------------------------------------------
          ;; 3. Subagent Buffer State and Lifecycle
          ;; ---------------------------------------------------------------------
          (describe "Subagent Buffer State and Lifecycle"
                    (it "creates and initializes subagent buffers with cloned context and tracking registration"
                        (let* ((parent-buf (get-buffer-create "*orch-life-parent*"))
                               (parent-ctx (macher-agent--make-context :id "parent-ctx-id" :project-root "/mock/subagent-root/"))
                               (subagent-buf nil))
                          (unwind-protect
                              (progn
                                (with-current-buffer parent-buf
                                  (setq-local macher-agent--persistent-context parent-ctx)
                                  (setq subagent-buf (macher-agent-add-subagent "*orch-life-child*" '(mock-preset) parent-buf)))
                                (expect (bufferp subagent-buf) :to-be t)
                                (expect (buffer-name subagent-buf) :to-equal "*orch-life-child*")
                                (with-current-buffer subagent-buf
                                  (expect macher-agent--is-workspace :to-be t)
                                  (expect (macher-agent-context-p macher-agent--persistent-context) :to-be t)
                                  (expect (macher-agent-context-project-root macher-agent--persistent-context) :to-equal "/mock/subagent-root/")
                                  (expect (eq macher-agent--persistent-context parent-ctx) :to-be nil)
                                  (expect macher-agent-presets :to-equal '(mock-preset))
                                  (expect (assoc "*orch-life-child*" (macher-agent-context-subagents macher-agent--persistent-context)) :to-be-truthy))
                                ;; Registered in tracking list and parent context
                                (expect (assoc "*orch-life-child*" macher-agent-active-subagents) :to-be-truthy)
                                (expect (assoc "*orch-life-child*" (macher-agent-context-subagents parent-ctx)) :to-be-truthy))
                            (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
                            (when (and subagent-buf (buffer-live-p subagent-buf)) (kill-buffer subagent-buf)))))

                    (it "isolates subagent persistent context modifications from parent context"
                        (let* ((parent-buf (get-buffer-create "*orch-iso-parent*"))
                               (parent-ctx (macher-agent--make-context :id "parent-iso" :project-root "/mock/iso-root/" :plugins '(:key "orig")))
                               (child-buf nil))
                          (unwind-protect
                              (progn
                                (with-current-buffer parent-buf
                                  (setq-local macher-agent--persistent-context parent-ctx)
                                  (setq child-buf (macher-agent-add-subagent "*orch-iso-child*" nil parent-buf)))
                                (with-current-buffer child-buf
                                  (setf (macher-agent-context-plugins macher-agent--persistent-context)
                                        (plist-put (copy-sequence (macher-agent-context-plugins macher-agent--persistent-context))
                                                   :key "child-mutated"))
                                  (expect (plist-get (macher-agent-context-plugins macher-agent--persistent-context) :key) :to-equal "child-mutated"))
                                (with-current-buffer parent-buf
                                  (expect (plist-get (macher-agent-context-plugins parent-ctx) :key) :to-equal "orig")))
                            (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
                            (when (and child-buf (buffer-live-p child-buf)) (kill-buffer child-buf)))))

                    (it "unconditionally sets gptel--known-presets and gptel-directives as buffer-local in subagent buffer"
                        (let* ((parent-buf (get-buffer-create "*orch-presets-parent*"))
                               (child-buf nil)
                               (parent-presets '((custom-preset . (:system "Custom Preset"))))
                               (parent-directives '((custom-directive . "Custom Directive Prompt"))))
                          (unwind-protect
                              (progn
                                (with-current-buffer parent-buf
                                  (setq-local gptel--known-presets parent-presets)
                                  (setq-local gptel-directives parent-directives))
                                (let ((state (list :name "*orch-presets-child*"
                                                   :target-dir default-directory
                                                   :parent-buffer parent-buf
                                                   :cloned-ctx nil
                                                   :presets nil)))
                                  (macher-agent-subagent-pipe--init-buffer state)
                                  (setq child-buf (get-buffer "*orch-presets-child*"))
                                  (expect (bufferp child-buf) :to-be t)
                                  (with-current-buffer child-buf
                                    (expect (local-variable-p 'gptel--known-presets) :to-be t)
                                    (expect (local-variable-p 'gptel-directives) :to-be t)
                                    (expect gptel--known-presets :to-equal parent-presets)
                                    (expect gptel-directives :to-equal parent-directives))))
                            (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
                            (when (and child-buf (buffer-live-p child-buf)) (kill-buffer child-buf)))))

                    (it "normalizes overloaded argument permutations in subagent pipeline"
                        (let* ((ctx (macher-agent--make-context :project-root "/mock/norm/"))
                               (parent-buf (get-buffer-create "*orch-norm-parent*")))
                          (unwind-protect
                              (let* ((state1 (list :name "*s1*" :presets "/custom/dir/" :parent-buffer parent-buf :context ctx))
                                     (norm1 (macher-agent-subagent-pipe--normalize-args state1)))
                                (expect (plist-get norm1 :dir) :to-equal "/custom/dir/")
                                (expect (plist-get norm1 :context) :to-be ctx))
                            (kill-buffer parent-buf))))

                    (it "initializes buffer-local flags and context via macher-agent--init-subagent-state"
                        (let* ((buf (get-buffer-create "*orch-init-state*"))
                               (ctx (macher-agent--make-context :id "test-init-ctx" :project-root "/mock/init/"))
                               (meta (list :background t :ephemeral t :suppress-patch t)))
                          (unwind-protect
                              (progn
                                (macher-agent--init-subagent-state buf "task-init-001" meta ctx)
                                (with-current-buffer buf
                                  (expect macher-agent--current-task-id :to-equal "task-init-001")
                                  (expect macher-agent--is-background :to-be t)
                                  (expect macher-agent--is-ephemeral :to-be t)
                                  (expect macher-agent--suppress-patch :to-be t)
                                  (expect macher-agent--ready-to-reap :to-be nil)
                                  (expect (macher-agent-context-id macher-agent--persistent-context) :to-equal "test-init-ctx")))
                            (kill-buffer buf))))

                    (it "reaps completed buffers when ready and cleans registries"
                        (let ((buf (get-buffer-create "*orch-reap-target*")))
                          (with-current-buffer buf
                            (setq-local macher-agent--ready-to-reap t)
                            (setq-local macher-agent-task-finished t))
                          (setq macher-agent-active-subagents (list (cons "*orch-reap-target*" "/tmp")))
                          (macher-agent--reap-buffer buf)
                          (expect (buffer-live-p buf) :to-be nil)
                          (expect (assoc "*orch-reap-target*" macher-agent-active-subagents) :to-be nil))))

          ;; ---------------------------------------------------------------------
          ;; 4. Virtual Buffer Synchronization
          ;; ---------------------------------------------------------------------
          (describe "Virtual Buffer Application"
                    (it "applies single and batch virtual buffer edits to live buffers"
                        (let* ((buf (get-buffer-create "*orch-vfs-live*"))
                               (ctx (macher-agent--make-context
                                     :project-root default-directory
                                     :plugins (list :vfs (list :contents (list (make-macher-agent-vfs-entry :path (buffer-name buf) :orig "initial" :curr "updated content")))))))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf
                                  (insert "initial"))
                                (macher-agent-apply-virtual-buffers ctx)
                                (with-current-buffer buf
                                  (expect (buffer-string) :to-equal "updated content")))
                            (kill-buffer buf))))

                    (it "safely handles invalid or non-existent entries in macher-agent--apply-single-virtual-buffer"
                        (expect (macher-agent--apply-single-virtual-buffer nil) :to-be nil)
                        (expect (macher-agent--apply-single-virtual-buffer (make-macher-agent-vfs-entry :path "*nonexistent-buf*" :orig "" :curr "text")) :to-be nil)
                        (expect (macher-agent--apply-single-virtual-buffer (make-macher-agent-vfs-entry :path nil :orig "" :curr "text")) :to-be nil)))

          ;; ---------------------------------------------------------------------
          ;; 5. Preset and Payload Composition and Skills
          ;; ---------------------------------------------------------------------
          (describe "Preset and Payload Composition"
                    (it "composes payload directives, tools, and parameters from presets"
                        (let* ((base-state (list :system "Base system prompt"
                                                 :tools nil
                                                 :known-presets '((alpha-skill :system "Alpha prompt"
                                                                               :tools (tool-alpha)
                                                                               :boot-directive "Alpha boot"))))
                               (payload (macher-agent-compose-payload base-state '(alpha-skill))))
                          (expect (plist-get payload :system) :to-match "Alpha prompt")
                          (expect (plist-get payload :boot-directive) :to-equal "Alpha boot")))

                    (it "applies skills directly to target buffer with macher-agent-use-skill"
                        (let ((buf (get-buffer-create "*orch-skill-buf*")))
                          (unwind-protect
                              (progn
                                (macher-agent-use-skill 'test-skill-sym buf)
                                (with-current-buffer buf
                                  (expect macher-agent-presets :to-equal '(test-skill-sym))))
                            (kill-buffer buf)))))

          ;; ---------------------------------------------------------------------
          ;; 6. Structs and Helper Utilities
          ;; ---------------------------------------------------------------------
          (describe "Structs and Helper Utilities"
                    (it "constructs macher-agent-task-context structures correctly"
                        (let ((tctx (make-macher-agent-task-context :workspace "/mock/ws"
                                                                    :target-buffer (current-buffer)
                                                                    :skill-sym 'test-sym
                                                                    :system-message "System prompt")))
                          (expect (macher-agent-task-context-p tctx) :to-be t)
                          (expect (macher-agent-task-context-workspace tctx) :to-equal "/mock/ws")
                          (expect (macher-agent-task-context-target-buffer tctx) :to-equal (current-buffer))
                          (expect (macher-agent-task-context-skill-sym tctx) :to-equal 'test-sym)
                          (expect (macher-agent-task-context-system-message tctx) :to-equal "System prompt")))

                    (it "extracts and updates context prompts and workspaces using specialized accessors"
                        (let ((ctx (macher-agent--make-context :id "ctx-acc-01"
                                                               :project-root "/mock/acc-root/"
                                                               :prompt "Initial Prompt")))
                          (expect (macher-agent-context-p ctx) :to-be t)
                          (expect (macher-agent-context-id ctx) :to-equal "ctx-acc-01")
                          (expect (macher-agent-context-project-root ctx) :to-equal "/mock/acc-root/")
                          (expect (macher-agent-context-prompt ctx) :to-equal "Initial Prompt")
                          (setf (macher-agent-context-prompt ctx) "Mutated Prompt")
                          (expect (macher-agent-context-prompt ctx) :to-equal "Mutated Prompt")
                          (expect (macher-agent-context-workspace ctx) :to-equal (cons 'project (expand-file-name "/mock/acc-root/")))))

                    (it "accesses and mutates transit payload target slots via macher-agent-transit-payload-target"
                        (let ((payload (make-macher-agent-transit-payload :target-buffer "target-buf-1")))
                          (expect (macher-agent-transit-payload-target payload) :to-equal "target-buf-1")
                          (expect (macher-agent-transit-payload-target-buf payload) :to-equal "target-buf-1")
                          (setf (macher-agent-transit-payload-target payload) "target-buf-2")
                          (expect (macher-agent-transit-payload-target-buffer payload) :to-equal "target-buf-2")
                          (expect (macher-agent-transit-payload-target payload) :to-equal "target-buf-2")
                          (setf (macher-agent-transit-payload-target-buf payload) "target-buf-3")
                          (expect (macher-agent-transit-payload-target-buffer payload) :to-equal "target-buf-3")
                          (expect (macher-agent-transit-payload-target-buf payload) :to-equal "target-buf-3")))

                    (it "resolves buffer names from strings, buffers, and file paths via macher-agent--resolve-buffer-name"
                        (let ((buf (get-buffer-create "*orch-name-resolve*")))
                          (unwind-protect
                              (progn
                                (expect (macher-agent--resolve-buffer-name buf) :to-equal "*orch-name-resolve*")
                                (expect (macher-agent--resolve-buffer-name "*orch-name-resolve*") :to-equal "*orch-name-resolve*")
                                (expect (macher-agent--resolve-buffer-name "unmatched-string") :to-equal "unmatched-string"))
                            (kill-buffer buf))))))

(provide 'macher-agent-orchestration-test)
;;; macher-agent-orchestration-test.el ends here
