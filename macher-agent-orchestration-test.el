;;; macher-agent-orchestration-test.el --- Tests for macher-agent orchestration -*- lexical-binding: t; -*-

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

    (it "maintains permanent-local persistence across mode shifts"
      (expect (get 'macher-agent--routing-stack 'permanent-local) :to-be t)
      (expect (get 'macher-agent--current-task-id 'permanent-local) :to-be t)
      (expect (get 'macher-agent--suppress-patch 'permanent-local) :to-be t)))

  ;; ---------------------------------------------------------------------
  ;; 2. A2A Delegation & Dispatch Pipeline
  ;; ---------------------------------------------------------------------
  (describe "A2A Delegation and Dispatch Pipeline"
    (it "normalizes payloads, dispatches through pipeline, and aggregates results on completion"
      (let* ((parent-buf (get-buffer-create "*orch-parent*"))
             (child-buf (get-buffer-create "*orch-child*"))
             (final-results nil)
             (payloads (list (list :type 'SEND_MESSAGE
                                   :task-id "task-alpha"
                                   :message "Execute alpha step"
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

    (it "prevents circular communication loops when routing to self"
      (let* ((self-buf (get-buffer-create "*orch-self*"))
             (results nil)
             (payloads (list (list :type 'SEND_MESSAGE
                                   :task-id "task-self"
                                   :message "Loopback message"
                                   :metadata (list :buffer_name (buffer-name self-buf)
                                                   :originator (buffer-name self-buf))))))
        (unwind-protect
            (with-current-buffer self-buf
              (macher-agent-a2a-dispatch payloads (lambda (res) (setq results res)))
              (expect (vectorp results) :to-be t)
              (expect (plist-get (aref results 0) :status) :to-be 'error)
              (expect (plist-get (aref results 0) :error) :to-match "cannot route a sub-agent payload to its own buffer"))
          (kill-buffer self-buf))))

    (it "handles missing target buffers by returning structured error plists"
      (let* ((results nil)
             (payloads (list (list :type 'SEND_MESSAGE
                                   :task-id "task-missing"
                                   :message "Target nowhere"
                                   :metadata (list :buffer_name "nonexistent-target-buf")))))
        (macher-agent-a2a-dispatch payloads (lambda (res) (setq results res)))
        (expect (vectorp results) :to-be t)
        (expect (plist-get (aref results 0) :status) :to-be 'error)
        (expect (plist-get (aref results 0) :error) :to-match "Sub-agent buffer 'nonexistent-target-buf' not found")))

    (it "wakes suspended subagents awaiting callbacks without re-transmitting"
      (let* ((target-buf (get-buffer-create "*orch-wake-target*"))
             (wake-data nil)
             (payloads (list (list :type 'SEND_MESSAGE
                                   :task-id "task-wake"
                                   :message "Resume instructions"
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
             (msg (list :type 'ARTIFACT_UPDATE
                        :task-id task-id
                        :message (list :status 'success :result "Artifact text"))))
        (puthash task-id (lambda (res) (setq artifact-received res)) macher-agent--pending-callbacks)
        (macher-agent-a2a-dispatch (list msg) nil)
        (expect (plist-get artifact-received :type) :to-equal 'ARTIFACT_UPDATE)
        (expect (gethash task-id macher-agent--pending-callbacks) :to-be nil)))

    (it "registers ownership paths in task and global ownership hash tables"
      (let* ((parent-buf (get-buffer-create "*orch-owner-parent*"))
             (child-buf (get-buffer-create "*orch-owner-child*"))
             (parent-ctx (macher-agent--make-context :project-root "/mock/orch-proj/")))
        (unwind-protect
            (progn
              (with-current-buffer parent-buf
                (setq-local macher-agent--persistent-context parent-ctx))
              (let* ((state (list :a2a-msg (list :task-id "task-owner-101"
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
          (kill-buffer child-buf)))))

  ;; ---------------------------------------------------------------------
  ;; 3. Subagent Buffer State & Lifecycle
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
                (expect (macher-agent-valid-context-p macher-agent--persistent-context) :to-be t)
                (expect (macher-agent-context-project-root macher-agent--persistent-context) :to-equal "/mock/subagent-root/")
                (expect macher-agent-presets :to-equal '(mock-preset)))
              ;; Registered in tracking list
              (expect (assoc "*orch-life-child*" macher-agent-active-subagents) :to-be-truthy))
          (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
          (when (and subagent-buf (buffer-live-p subagent-buf)) (kill-buffer subagent-buf)))))

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
             (ctx (macher-agent--make-vfs-context
                   :contents (list (macher-agent-vfs-make-entry (buffer-name buf) "initial" "updated content")))))
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
      (expect (macher-agent--apply-single-virtual-buffer '("*nonexistent-buf*" . "text")) :to-be nil)
      (expect (macher-agent--apply-single-virtual-buffer '(:path nil :curr "text")) :to-be nil)))

  ;; ---------------------------------------------------------------------
  ;; 5. Preset / Payload Composition & Skills
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
