;;; macher-agent-test-presets.el --- Preset & Payload Composition Tests -*- lexical-binding: t; -*-

(let* ((file (or load-file-name buffer-file-name))
       (test-dir (cond
                  (file (file-name-directory file))
                  ((file-exists-p (expand-file-name "macher-agent-test-setup.el" default-directory))
                   default-directory)
                  ((file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" default-directory))
                   (expand-file-name "tests" default-directory))
                  (t default-directory))))
  (add-to-list 'load-path test-dir)
  (add-to-list 'load-path (expand-file-name "helpers" test-dir)))

(require 'macher-agent-test-setup)

(describe "Preset and Payload Composition"
  (macher-agent-test-setup-before-each)

  (it "applies the correct model from the skill metadata to gptel-model"
      (spy-on 'macher-agent-resolve-context :and-return-value
              (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/mock/proj") :contents nil))
      (let* ((skill-name 'rust-skill)
             (skill-data '(:description "Test" :model gpt-4o :has-tools nil :context-dir nil :system "test"))
             (execution (macher--make-action-execution :action skill-name)))
        (let ((workspace (macher-agent--get-context-workspace (macher-agent-resolve-context))))
          (setf (alist-get skill-name (macher-agent-workspace-skills-alist workspace)) skill-data))
        (with-temp-buffer
          (let ((gptel--known-presets nil))
            (macher-agent-initialize-skills (macher-agent-resolve-context))
            (let ((preset-def (alist-get skill-name gptel--known-presets)))
              (expect preset-def :not :to-be nil)
              (expect (plist-get preset-def :model) :to-equal 'gpt-4o))))))

  (it "does not change gptel-model if no model is specified in the skill"
      (spy-on 'macher-agent-resolve-context :and-return-value
              (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/mock/proj") :contents nil))
      (let* ((skill-name 'plain-skill)
             (skill-data '(:description "Test" :model nil :has-tools nil :context-dir nil :system "test"))
             (execution (macher--make-action-execution :action skill-name))
             (original-model gptel-model))
        (let ((workspace (macher-agent--get-context-workspace (macher-agent-resolve-context))))
          (setf (alist-get skill-name (macher-agent-workspace-skills-alist workspace)) skill-data))
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

  (it "clears base and preceding state in macher-agent-preset-pipe--exclusive when exclusive is t"
      (let ((state (list :system "existing sys" :tools '(t1) :ptc-primitives '(p1) :boot-directive "boot" :model 'gpt-4o))
            (item-exclusive '(preset my-preset (:exclusive t)))
            (item-non-exclusive '(preset my-preset (:exclusive nil)))
            (item-tool '(tool tool1)))
        (let ((res (macher-agent-preset-pipe--exclusive state item-exclusive)))
          (expect res :to-be state)
          (expect (plist-get res :system) :to-be nil)
          (expect (plist-get res :tools) :to-be nil)
          (expect (plist-get res :ptc-primitives) :to-be nil)
          (expect (plist-get res :boot-directive) :to-be nil)
          (expect (plist-get res :model) :to-equal 'gpt-4o))
        (expect (macher-agent-preset-pipe--exclusive state item-non-exclusive) :to-equal state)
        (expect (macher-agent-preset-pipe--exclusive state item-tool) :to-equal state)))

  (it "directly mutates state in-place without memory allocation in macher-agent-preset-pipe--exclusive"
      (let* ((state (list :system "prompt" :tools '(tool1) :ptc-primitives '(p1) :boot-directive "boot" :model 'gpt-4))
             (item '(preset my-preset (:exclusive t)))
             (res (macher-agent-preset-pipe--exclusive state item)))
        (expect res :to-be state)
        (expect (plist-get state :system) :to-be nil)
        (expect (plist-get state :tools) :to-be nil)
        (expect (plist-get state :ptc-primitives) :to-be nil)
        (expect (plist-get state :boot-directive) :to-be nil)
        (expect (plist-get state :model) :to-equal 'gpt-4)))

  (it "merges system prompt in macher-agent-preset-pipe--system"
      (let ((state '(:system "Initial prompt"))
            (item '(preset sys-preset (:system "Added prompt"))))
        (let ((res (macher-agent-preset-pipe--system state item)))
          (expect (plist-get res :system) :to-match "Initial prompt")
          (expect (plist-get res :system) :to-match "Added prompt"))
        (expect (macher-agent-preset-pipe--system state '(tool tool1)) :to-equal state)))

  (it "merges tools and standalone tools in macher-agent-preset-pipe--tools"
      (let* ((tool-obj (if (fboundp 'gptel-make-tool)
                           (gptel-make-tool :name "t1" :description "d1")
                         "t1"))
             (state nil)
             (item-preset `(preset tool-preset (:tools (,tool-obj))))
             (item-tool `(tool ,tool-obj)))
        (spy-on 'gptel-tool-p :and-return-value t)
        (let ((res1 (macher-agent-preset-pipe--tools state item-preset)))
          (expect (plist-get res1 :tools) :not :to-be nil))
        (let ((res2 (macher-agent-preset-pipe--tools state item-tool)))
          (expect (plist-get res2 :tools) :to-equal (list tool-obj)))))

  (it "handles fully instantiated gptel-tool structures intact in macher-agent--compose-merge-tools"
      (let* ((instantiated-tool (gptel-make-tool :name "instantiated_tool"
                                                 :function (lambda () "ok")
                                                 :description "Fully instantiated tool struct"))
             (current-tools nil)
             (res (macher-agent--compose-merge-tools current-tools instantiated-tool)))
        (expect res :to-equal (list instantiated-tool))
        (expect (car res) :to-equal instantiated-tool)))

  (it "converts strings, symbols, and abstract lists to gptel-tool objects in macher-agent--compose-merge-tools"
      (let* ((mock-tool-str (gptel-make-tool :name "tool_str" :description "str tool"))
             (mock-tool-sym (gptel-make-tool :name "tool_sym" :description "sym tool"))
             (mock-tool-cat (gptel-make-tool :name "tool_cat" :description "cat tool")))
        (spy-on 'gptel-get-tool :and-call-fake
                (lambda (path)
                  (cond
                   ((equal path "tool_str") mock-tool-str)
                   ((equal path "tool_sym") mock-tool-sym)
                   ((equal path '("emacs" "tool_cat")) mock-tool-cat)
                   (t nil))))
        (let ((res-str (macher-agent--compose-merge-tools nil "tool_str"))
              (res-sym (macher-agent--compose-merge-tools nil 'tool_sym))
              (res-cat (macher-agent--compose-merge-tools nil '("emacs" "tool_cat"))))
          (expect (car res-str) :to-equal mock-tool-str)
          (expect (car res-sym) :to-equal mock-tool-sym)
          (expect (car res-cat) :to-equal mock-tool-cat))))

  (it "passes gptel-tool objects intact and resolves strings, symbols, and abstract lists in macher-agent-preset-pipe--tools"
      (let* ((tool-struct (gptel-make-tool :name "struct_tool" :description "struct tool"))
             (tool-str-obj (gptel-make-tool :name "str_tool" :description "str tool"))
             (state nil)
             (preset-item `(preset my-preset (:tools (,tool-struct "str_tool"))))
             (tool-item `(tool ,tool-struct)))
        (spy-on 'gptel-get-tool :and-call-fake
                (lambda (path)
                  (cond
                   ((equal path "str_tool") tool-str-obj)
                   (t nil))))
        (let ((res1 (macher-agent-preset-pipe--tools state preset-item)))
          (expect (plist-get res1 :tools) :to-equal (list tool-struct tool-str-obj)))
        (let ((res2 (macher-agent-preset-pipe--tools state tool-item)))
          (expect (plist-get res2 :tools) :to-equal (list tool-struct)))))

  (it "handles raw property lists for tool definitions in macher-agent--compose-merge-tools"
      (let* ((raw-plist '(:name "plist_tool" :description "Plist tool description"))
             (res (macher-agent--compose-merge-tools nil raw-plist)))
        (expect (length res) :to-equal 1)
        (expect (gptel-tool-p (car res)) :to-be t)
        (expect (gptel-tool-name (car res)) :to-equal "plist_tool")))

  (it "handles raw property lists for standalone tool definitions in macher-agent-preset-pipe--tools"
      (let* ((raw-plist '(:name "standalone_plist_tool" :description "Standalone plist description"))
             (state nil)
             (tool-item `(tool ,raw-plist))
             (res (macher-agent-preset-pipe--tools state tool-item))
             (tools (plist-get res :tools)))
        (expect (length tools) :to-equal 1)
        (expect (gptel-tool-p (car tools)) :to-be t)
        (expect (gptel-tool-name (car tools)) :to-equal "standalone_plist_tool")))

  (it "merges and deduplicates ptc primitives in macher-agent-preset-pipe--ptc"
      (let ((state '(:ptc-primitives (prim1 prim2)))
            (item '(preset ptc-preset (:ptc-primitives (prim2 prim3)))))
        (let ((res (macher-agent-preset-pipe--ptc state item)))
          (expect (plist-get res :ptc-primitives) :to-equal '(prim1 prim2 prim3)))))

  (it "applies boot directive in macher-agent-preset-pipe--boot"
      (let ((state nil)
            (item '(preset boot-preset (:boot-directive "Run boot instructions."))))
        (let ((res (macher-agent-preset-pipe--boot state item)))
          (expect (plist-get res :boot-directive) :to-equal "Run boot instructions."))))

  (it "applies model parameters in macher-agent-preset-pipe--parameters"
      (let ((state '(:temperature 0.5))
            (item '(preset param-preset (:model claude-3-5-sonnet :temperature 0.2 :max-tokens 4000))))
        (let ((res (macher-agent-preset-pipe--parameters state item)))
          (expect (plist-get res :model) :to-equal 'claude-3-5-sonnet)
          (expect (plist-get res :temperature) :to-equal 0.2)
          (expect (plist-get res :max-tokens) :to-equal 4000))))

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

(provide 'macher-agent-test-presets)
;;; macher-agent-test-presets.el ends here