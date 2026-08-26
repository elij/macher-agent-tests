;;; macher-agent-presets-test.el --- Preset & Payload Composition Tests -*- lexical-binding: t; -*-

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

(describe "Preset and Payload Composition"
          (macher-agent-test-setup-before-each)

          (it "applies the correct model from the skill metadata to gptel-model"
              (let* ((mock-ctx (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/mock/proj") :contents nil))
                     (workspace (macher-agent--get-context-workspace mock-ctx))
                     (skill-name 'rust-skill)
                     (skill-data '(:description "Test" :model gpt-4o :has-tools nil :context-dir nil :system "test"))
                     (execution (macher--make-action-execution :action skill-name)))
                (macher-agent--register-active-workspace-root "/mock/proj" mock-ctx)
                (spy-on 'macher-agent-resolve-context :and-return-value mock-ctx)
                (setf (alist-get skill-name (macher-agent-workspace-skills-alist mock-ctx)) skill-data)
                (with-temp-buffer
                  (let ((gptel--known-presets nil))
                    (macher-agent-initialize-skills (macher-agent-resolve-context))
                    (let ((preset-def (alist-get skill-name gptel--known-presets)))
                      (expect preset-def :not :to-be nil)
                      (expect (plist-get preset-def :model) :to-equal 'gpt-4o))))))

          (it "does not change gptel-model if no model is specified in the skill"
              (let* ((mock-ctx (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/mock/proj") :contents nil))
                     (workspace (macher-agent--get-context-workspace mock-ctx))
                     (skill-name 'plain-skill)
                     (skill-data '(:description "Test" :model nil :has-tools nil :context-dir nil :system "test"))
                     (execution (macher--make-action-execution :action skill-name))
                     (original-model gptel-model))
                (macher-agent--register-active-workspace-root "/mock/proj" mock-ctx)
                (spy-on 'macher-agent-resolve-context :and-return-value mock-ctx)
                (setf (alist-get skill-name (macher-agent-workspace-skills-alist mock-ctx)) skill-data)
                (with-temp-buffer
                  (let ((gptel--known-presets nil))
                    (macher-agent-initialize-skills (macher-agent-resolve-context))
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

          (it "filters out and deletes search_in_workspace in macher-agent--wrap-macher-tools"
              (let* ((tool1 (gptel-make-tool :name "read_file" :category "macher" :function #'ignore :description "read" :args nil))
                     (tool-search (gptel-make-tool :name "search_in_workspace" :category "macher" :function #'ignore :description "search" :args nil))
                     (tool2 (gptel-make-tool :name "edit_file" :category "macher" :function #'ignore :description "edit" :args nil))
                     (gptel--known-tools (list (cons "macher" (list (cons "read_file" tool1)
                                                                    (cons "search_in_workspace" tool-search)
                                                                    (cons "edit_file" tool2))))))
                (macher-agent--wrap-macher-tools)
                (let* ((entry (assoc "macher" gptel--known-tools))
                       (tools (cdr entry)))
                  (expect (assoc "search_in_workspace" tools) :to-be nil)
                  (expect (assoc "read_file" tools) :not :to-be nil)
                  (expect (assoc "edit_file" tools) :not :to-be nil))))

          (it "ensures search_in_workspace with perception category replaces upstream macher version under presets"
              (let* ((upstream-search (gptel-make-tool :name "search_in_workspace" :category "macher" :function #'ignore :description "upstream" :args nil))
                     (agent-search (gptel-make-tool :name "search_in_workspace" :category "perception" :function #'ignore :description "agent" :args nil))
                     (clear-fn (plist-get (plist-get macher--preset-clear-tools :tools) :function))
                     (gptel--known-tools (list (cons "macher" (list (cons "search_in_workspace" upstream-search)))
                                               (cons "perception" (list (cons "search_in_workspace" agent-search))))))
                ;; wrap filters out upstream search_in_workspace from macher category
                (macher-agent--wrap-macher-tools)
                (let* ((macher-entry (assoc "macher" gptel--known-tools))
                       (perception-entry (assoc "perception" gptel--known-tools)))
                  (expect (assoc "search_in_workspace" (cdr macher-entry)) :to-be nil)
                  (expect (assoc "search_in_workspace" (cdr perception-entry)) :not :to-be nil))
                ;; preset purge removes macher category tools but retains perception category search_in_workspace
                (let ((filtered (funcall clear-fn (list upstream-search agent-search))))
                  (expect (length filtered) :to-equal 1)
                  (expect (gptel-tool-category (car filtered)) :to-equal "perception")
                  (expect (gptel-tool-name (car filtered)) :to-equal "search_in_workspace"))))

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
                (expect (plist-get composed :temperature) :to-equal 0.3))))

(provide 'macher-agent-presets-test)
;;; macher-agent-presets-test.el ends here
