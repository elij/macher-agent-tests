;;; macher-agent-presets-test.el --- Preset & Payload Composition Tests -*- lexical-binding: t; -*-

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

(describe "Preset and Payload Composition"
          (macher-agent-test-setup-before-each)

          (it "applies the correct model from the skill metadata to gptel-model"
              (let* ((mock-ctx (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/mock/proj") :contents nil))
                     (workspace (macher-agent-context-workspace mock-ctx))
                     (skill-name 'rust-skill)
                     (skill-data '(:description "Test" :model gpt-4o :has-tools nil :context-dir nil :system "test"))
                     (execution (macher--make-action-execution :action skill-name)))
                (macher-agent--register-active-workspace-root "/mock/proj" mock-ctx)
                (setf (alist-get skill-name (macher-agent-workspace-skills-alist mock-ctx)) skill-data)
                (with-temp-buffer
                  (setq-local macher-agent--persistent-context mock-ctx)
                  (let ((gptel--known-presets nil))
                    (macher-agent-initialize-skills mock-ctx)
                    (let ((preset-def (alist-get skill-name gptel--known-presets)))
                      (expect preset-def :not :to-be nil)
                      (expect (plist-get preset-def :model) :to-equal 'gpt-4o))))))

          (it "does not change gptel-model if no model is specified in the skill"
              (let* ((mock-ctx (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/mock/proj") :contents nil))
                     (workspace (macher-agent-context-workspace mock-ctx))
                     (skill-name 'plain-skill)
                     (skill-data '(:description "Test" :model nil :has-tools nil :context-dir nil :system "test"))
                     (execution (macher--make-action-execution :action skill-name))
                     (original-model gptel-model))
                (macher-agent--register-active-workspace-root "/mock/proj" mock-ctx)
                (setf (alist-get skill-name (macher-agent-workspace-skills-alist mock-ctx)) skill-data)
                (with-temp-buffer
                  (setq-local macher-agent--persistent-context mock-ctx)
                  (let ((gptel--known-presets nil))
                    (macher-agent-initialize-skills mock-ctx)
                    (let ((preset-def (alist-get skill-name gptel--known-presets)))
                      (expect preset-def :not :to-be nil)
                      (expect (plist-member preset-def :model) :to-be nil))))))

          (it "ensures custom tools survive the preset purge and retain correct category"
              (let* ((custom-tool (gptel-make-tool
                                   :name "cargo_check_tool"
                                   :function #'ignore
                                   :category "macher-agent-rust"
                                   :description "Test tool"
                                   :args nil))
                     (clear-fn (plist-get (plist-get macher--preset-clear-tools :tools) :function))
                     (tools-list (list custom-tool
                                       (gptel-make-tool :name "native_tool" :function #'ignore :category "macher" :description "native" :args nil)))
                     (filtered-tools (funcall clear-fn tools-list)))
                (expect (seq-find (lambda (tool) (string= (gptel-tool-name tool) "cargo_check_tool")) filtered-tools) :not :to-be nil)
                (expect (gptel-tool-category custom-tool) :to-equal "macher-agent-rust")))

          (it "verifies that tools identified as 'macher' category get context injected"
              (let* ((mock-fsm (gptel-make-fsm))
                     (mock-tool (gptel-make-tool :name "test_tool"
                                                 :function (lambda (ctx) ctx)
                                                 :category "macher"
                                                 :description "test" :args nil))
                     (mock-context 'injected-context))
                (setf (gptel-fsm-info mock-fsm) (list :tools (list mock-tool) :buffer (current-buffer)))
                (macher--setup-tools mock-fsm (lambda () mock-context))
                (let* ((processed-tools (plist-get (gptel-fsm-info mock-fsm) :tools))
                       (processed-tool (car processed-tools)))
                  (expect (funcall (gptel-tool-function processed-tool)) :to-equal 'injected-context))))

          (it "preserves the custom category to avoid being purged by upstream read-only presets"
              (let ((mock-tool (gptel-make-tool :name "my_custom_tool"
                                                :function #'ignore
                                                :category "macher-agent-calendar"
                                                :description "test"
                                                :args nil))
                    (clear-fn (plist-get (plist-get macher--preset-clear-tools :tools) :function)))
                (expect (gptel-tool-category mock-tool) :not :to-equal macher-tool-category)
                (let ((filtered-tools (funcall clear-fn (list mock-tool))))
                  (expect (length filtered-tools) :to-equal 1)
                  (expect (gptel-tool-name (car filtered-tools)) :to-equal "my_custom_tool"))))

          (it "handles exclusive override flag by resetting accumulated base and preceding state"
              (let* ((tool1 (gptel-make-tool :name "tool1" :description "tool1"))
                     (toolA (gptel-make-tool :name "toolA" :description "toolA"))
                     (toolB (gptel-make-tool :name "toolB" :description "toolB"))
                     (gptel-tools (list tool1 toolA toolB))
                     (base-state `(:model gpt-3.5-turbo
                                          :system "Base system message"
                                          :temperature 0.7
                                          :max-tokens 100
                                          :tools (,tool1)
                                          :known-presets ((preset-a . (:system "Preset A prompt" :temperature 0.5 :tools (,toolA)))
                                                          (preset-b . (:system "Preset B prompt" :exclusive t :temperature 0.2 :tools (,toolB))))))
                     (presets '(preset-a preset-b)))
                (let ((payload (macher-agent-compose-payload base-state presets)))
                  (expect (plist-get payload :system) :to-equal "### Skill: preset-b\nPreset B prompt\n")
                  (expect (plist-get payload :temperature) :to-equal 0.2)
                  (expect (mapcar #'gptel-tool-name (plist-get payload :tools)) :to-equal '("toolB")))))

          (it "defines macher-agent-preset-pipeline-functions with all step functions in order"
              (expect macher-agent-preset-pipeline-functions
                      :to-equal '(macher-agent-preset-pipe--exclusive
                                  macher-agent-preset-pipe--system
                                  macher-agent-preset-pipe--tools
                                  macher-agent-preset-pipe--ptc
                                  macher-agent-preset-pipe--boot
                                  macher-agent-preset-pipe--parameters)))

          (it "handles exclusive override non-destructively in macher-agent-preset-pipe--exclusive"
              (let* ((state (list :system "existing sys" :tools '(tool1) :ptc-primitives '(p1) :boot-directive "boot" :model 'gpt-4o))
                     (item-exclusive '(preset my-preset (:exclusive t)))
                     (item-non-exclusive '(preset my-preset (:exclusive nil)))
                     (res (macher-agent-preset-pipe--exclusive state item-exclusive)))
                (expect res :not :to-be state)
                (expect (plist-get res :system) :to-be nil)
                (expect (plist-get res :tools) :to-be nil)
                (expect (plist-get res :ptc-primitives) :to-be nil)
                (expect (plist-get res :boot-directive) :to-be nil)
                (expect (plist-get res :model) :to-equal 'gpt-4o)
                (expect (plist-get state :system) :to-equal "existing sys")
                (expect (macher-agent-preset-pipe--exclusive state item-non-exclusive) :to-equal state)))

          (it "resolves and merges tool structs, strings, symbols, and plists in tool pipeline"
              (let* ((tool-struct (gptel-make-tool :name "struct_tool" :description "struct tool"))
                     (tool-str-obj (gptel-make-tool :name "str_tool" :description "str tool"))
                     (tool-sym-obj (gptel-make-tool :name "sym_tool" :description "sym tool"))
                     (raw-plist '(:name "plist_tool" :description "plist tool"))
                     (state nil)
                     (preset-item `(preset my-preset (:tools (,tool-struct "str_tool" 'sym_tool ,raw-plist))))
                     (tool-item `(tool ,tool-struct)))
                (spy-on 'gptel-get-tool :and-call-fake
                        (lambda (path)
                          (cond
                           ((equal path "str_tool") tool-str-obj)
                           ((equal path 'sym_tool) tool-sym-obj)
                           (t nil))))
                (let* ((res1 (macher-agent-preset-pipe--tools state preset-item))
                       (tools1 (plist-get res1 :tools)))
                  (expect (length tools1) :to-equal 4)
                  (expect (nth 0 tools1) :to-equal tool-struct)
                  (expect (nth 1 tools1) :to-equal tool-str-obj)
                  (expect (nth 2 tools1) :to-equal tool-sym-obj)
                  (expect (gptel-tool-name (nth 3 tools1)) :to-equal "plist_tool"))
                (let ((res2 (macher-agent-preset-pipe--tools state tool-item)))
                  (expect (plist-get res2 :tools) :to-equal (list tool-struct)))))

          (it "applies system prompt, ptc primitives, boot directive, and parameters across step reducers"
              (let* ((state '(:system "Initial prompt" :ptc-primitives (prim1) :temperature 0.8))
                     (item-sys '(preset sys-preset (:system "Added prompt")))
                     (item-ptc '(preset ptc-preset (:ptc-primitives (prim1 prim2))))
                     (item-boot '(preset boot-preset (:boot-directive "Run boot instructions.")))
                     (item-param '(preset param-preset (:model claude-3-5-sonnet :temperature 0.2 :max-tokens 4000))))
                ;; System prompt reducer
                (let ((res-sys (macher-agent-preset-pipe--system state item-sys)))
                  (expect (plist-get res-sys :system) :to-match "Initial prompt")
                  (expect (plist-get res-sys :system) :to-match "Added prompt"))
                ;; PTC reducer
                (let ((res-ptc (macher-agent-preset-pipe--ptc state item-ptc)))
                  (expect (plist-get res-ptc :ptc-primitives) :to-equal '(prim1 prim2)))
                ;; Boot directive reducer
                (let ((res-boot (macher-agent-preset-pipe--boot nil item-boot)))
                  (expect (plist-get res-boot :boot-directive) :to-equal "Run boot instructions."))
                ;; Model parameters reducer
                (let ((res-param (macher-agent-preset-pipe--parameters state item-param)))
                  (expect (plist-get res-param :model) :to-equal 'claude-3-5-sonnet)
                  (expect (plist-get res-param :temperature) :to-equal 0.2)
                  (expect (plist-get res-param :max-tokens) :to-equal 4000))))

          (it "pre-flattens preset parent dependencies in topological order and avoids cycles"
              (let ((known '((grandparent . (:system "Grandparent"))
                             (parent . (:parents (grandparent) :system "Parent"))
                             (child . (:parents (parent) :system "Child"))
                             (cyclic-a . (:parents (cyclic-b) :system "Cyclic A"))
                             (cyclic-b . (:parents (cyclic-a) :system "Cyclic B")))))
                (let ((flattened (macher-agent--flatten-preset-dependencies '(child) known)))
                  (expect (mapcar #'cadr flattened) :to-equal '(grandparent parent child)))
                (let ((flattened-cycle (macher-agent--flatten-preset-dependencies '(cyclic-a) known)))
                  (expect (length flattened-cycle) :to-equal 2))))

          (it "composes payload by pre-flattening and reducing over step functions"
              (let* ((tool1 (if (fboundp 'gptel-make-tool) (gptel-make-tool :name "tool1" :description "t1") "tool1"))
                     (tool2 (if (fboundp 'gptel-make-tool) (gptel-make-tool :name "tool2" :description "t2") "tool2"))
                     (base-state `(:system "Base"
                                           :temperature 0.8
                                           :known-presets ((parent-preset . (:system "Parent prompt" :ptc-primitives (p1)))
                                                           (child-preset . (:parents (parent-preset) :system "Child prompt" :boot-directive "Boot child" :ptc-primitives (p2) :temperature 0.3)))))
                     (composed (macher-agent-compose-payload base-state '(child-preset))))
                (expect (plist-get composed :system) :to-match "Parent prompt")
                (expect (plist-get composed :system) :to-match "Child prompt")
                (expect (plist-get composed :ptc-primitives) :to-equal '(p1 p2))
                (expect (plist-get composed :boot-directive) :to-equal "Boot child")
                (expect (plist-get composed :temperature) :to-equal 0.3)))

          (it "confirms dead function macher-agent--evaluate-and-cache-tool is removed"
              (expect (fboundp 'macher-agent--evaluate-and-cache-tool) :to-be nil))

          (it "resolves skill VFS content directly from context contents entries"
              (let* ((vfs-entry (make-macher-agent-vfs-entry :path "/mock/skills/my-skill/SKILL.md"
                                                             :curr "---\nname: vfs-skill\n---\nVFS Skill Body"))
                     (ctx (macher-agent--make-vfs-context :contents (list vfs-entry))))
                (expect (macher-agent--resolve-skill-vfs-content '("/mock/skills/my-skill/SKILL.md") ctx)
                        :to-equal "---\nname: vfs-skill\n---\nVFS Skill Body")))

          (it "contains no calls or references to obsolete context helpers in macher-agent-presets.el"
              (let* ((presets-file (or (locate-library "macher-agent-presets.el")
                                       (expand-file-name "macher-agent-presets.el" default-directory)))
                     (content (with-temp-buffer
                                (insert-file-contents presets-file)
                                (buffer-string))))
                (expect (string-match-p "macher-agent--get-context-workspace" content) :to-be nil)
                (expect (string-match-p "macher-agent--get-context-data" content) :to-be nil)
                (expect (string-match-p "macher-agent--set-context-data" content) :to-be nil)
                (expect (string-match-p "macher-agent--get-context-prompt" content) :to-be nil)))

          (it "resolves workspace via specialised accessor macher-agent-context-workspace in mutation and resolution"
              (let* ((mock-ws (make-macher-agent-workspace :project-root "/mock/ws-root/"))
                     (mock-ctx (macher-agent--make-vfs-context :workspace mock-ws :contents nil)))
                (expect (macher-agent-context-workspace mock-ctx) :to-equal mock-ws))))

(provide 'macher-agent-presets-test)
;;; macher-agent-presets-test.el ends here
