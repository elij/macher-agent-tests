;;; macher-agent-skills-test.el --- Tests for macher-agent-skills -*- lexical-binding: t; -*-

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

(require 'buttercup)
(require 'macher-agent-test-setup)
(require 'macher-agent)
(require 'macher-agent-macher nil t)
(require 'macher-agent-vfs)
(require 'macher-agent-zero-mem)
(require 'macher-agent-test-harness)

(describe
 "macher-agent-skills"
 (before-all
  (when (fboundp 'macher-agent-zero-mem-uninstall)
    (macher-agent-zero-mem-uninstall))
  (setq macher-agent-search-backend-function #'macher-agent-search-glob))
 (before-each
  (setq macher-agent-search-backend-function #'macher-agent-search-glob))
 (after-each
  (when (fboundp 'macher-agent-zero-mem-uninstall)
    (macher-agent-zero-mem-uninstall))
  (setq macher-agent-search-backend-function #'macher-agent-search-glob))
 (after-all
  (when (fboundp 'macher-agent-zero-mem-uninstall)
    (macher-agent-zero-mem-uninstall))
  (setq macher-agent-search-backend-function #'macher-agent-search-glob))

 (describe
  "Orchestration Tools (skills/scripts/*.el)"
  (before-all
   ;; Load all tool scripts to define their variables for testing
   (let* ((file-name (or load-file-name buffer-file-name))
          (test-dir (if file-name (file-name-directory file-name) default-directory))
          (root-dir (or (locate-dominating-file test-dir "skills") test-dir))
          (scripts-dir (expand-file-name "skills/scripts" root-dir)))
     (dolist (script (directory-files scripts-dir t "\\.el$"))
       (load script nil t))))
  (it "properly parses a JSON string into a vector for task delegation"
      (let* ((ctx (macher-agent--make-context))
             (callback-called nil)
             (callback (lambda (res) (setq callback-called res)))
             (json-tasks (vector (list :buffer_name "test-sub" :instructions "do work")))
             (tool-fn (or (get 'macher-agent-delegate-tasks-to-subagents-tool 'ptc-function)
                          (get 'macher-agent-delegate-tasks-tool 'ptc-function)))
             (buf (get-buffer-create "test-sub")))
        
        (spy-on 'macher-agent-a2a-dispatch)
        (spy-on 'macher-agent--ensure-access)
        
        (funcall tool-fn json-tasks ctx callback)
        
        (expect 'macher-agent-a2a-dispatch :to-have-been-called)
        (kill-buffer buf)))

  (it "searches conversation history via glob search"
      (let ((buf (generate-new-buffer "test-conv-glob")))
        (with-current-buffer buf
          (insert "Line 1: system start\nLine 2: important keyword found\nLine 3: system end\n"))
        (let ((res (macher-agent-search-glob "keyword" buf 1)))
          (expect res :to-match "Match near line 2")
          (expect res :to-match "important keyword found"))
        (let ((res-none (macher-agent-search-glob "nonexistent" buf 1)))
          (expect res-none :to-match "No matches found in history for: nonexistent"))
        (kill-buffer buf)))

  (it "converts buffer lines into zero-mem trace plists"
      (let ((buf (generate-new-buffer "test-conv-traces")))
        (with-current-buffer buf
          (insert "First trace line\n\nSecond trace line\n"))
        (let ((traces (macher-agent-zero-mem--buffer-to-traces buf)))
          (expect (length traces) :to-equal 2)
          (expect (plist-get (nth 0 traces) :text) :to-equal "First trace line")
          (expect (plist-get (nth 0 traces) :timestamp) :to-equal 1.0)
          (expect (plist-get (nth 1 traces) :text) :to-equal "Second trace line")
          (expect (plist-get (nth 1 traces) :timestamp) :to-equal 3.0))
        (kill-buffer buf)))

  (it "defaults macher-agent-search-backend-function to macher-agent-search-glob"
      (expect (default-value 'macher-agent-search-backend-function) :to-equal #'macher-agent-search-glob))

  (it "handles zero-length matches and invalid-regexp safely in search_glob"
      (let ((buf (generate-new-buffer "test-conv-edge")))
        (with-current-buffer buf
          (insert "Line 1\nLine 2\n"))
        ;; Test zero-length match (e.g., "^" or ".*") does not infinite loop
        (let ((res (macher-agent-search-glob "^" buf 1)))
          (expect res :to-match "Match near line 1"))
        ;; Test invalid regexp handling
        (let ((res-err (macher-agent-search-glob "[unclosed" buf 1)))
          (expect res-err :to-match "Error: Invalid regular expression"))
        (kill-buffer buf)))

  )

 (describe "Agent Skills (macher-agent-skills.el)"
           (before-each
            (let* ((ws (make-macher-agent-workspace :project-root "/mock/proj"))
                   (ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))
              (puthash (expand-file-name "/mock/proj") ctx macher-agent-active-workspaces)
              (setq-local macher-agent--persistent-context ctx)))
           
           (it "parses SKILL.md files correctly extracting frontmatter and markdown body"
               (let* ((parsed (macher-agent-parse-skill-file "tests/fixtures/skills/global/SKILL.md" macher-agent--persistent-context)))
                 (expect (plist-get parsed :name) :to-equal "mock-skill")
                 (expect (plist-get parsed :name-sym) :to-equal 'mock-skill)
                 (expect (plist-get parsed :description) :to-equal "A mock skill for testing")
                 (expect (plist-get parsed :allowed-tools) :to-equal '("mock-tool-1" "mock-tool-2"))
                 (expect (plist-get parsed :body) :to-equal "This is the system prompt for the mock skill.\nIt spans multiple lines.")))

           (it "extracts boot-directive from SKILL.md frontmatter"
               (let* ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                               (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces)))
                      (vfs-file "tests/fixtures/skills/global/SKILL.md"))
                 (macher-agent--set-context-contents
                  ctx
                  (list (macher-agent-vfs-make-entry
                         vfs-file
                         "original"
                         "---\nname: boot-skill\ndescription: Skill with boot directive\nboot-directive: Perform initial setup before executing.\n---\nSkill body content")))
                 (let ((parsed (macher-agent-parse-skill-file vfs-file ctx)))
                   (expect (plist-get parsed :name) :to-equal "boot-skill")
                   (expect (plist-get parsed :boot-directive) :to-equal "Perform initial setup before executing."))))

           (it "persists boot-directive into gptel--known-presets and sets buffer-local macher-agent--boot-directive when applying preset"
               (let* ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                               (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces))))
                 (setf (alist-get 'boot-preset (macher-agent-workspace-skills-alist ctx))
                       '(:system "System prompt" :description "Boot Preset" :boot-directive "Initial boot directive text"))
                 (macher-agent-initialize-skills ctx)
                 (let ((spec (alist-get 'boot-preset gptel--known-presets)))
                   (expect (plist-get spec :boot-directive) :to-equal "Initial boot directive text"))
                 (let ((test-buf (generate-new-buffer "test-boot-preset-buf")))
                   (with-current-buffer test-buf
                     (if (boundp 'gptel--known-presets)
                         (setq gptel--known-presets (copy-tree gptel--known-presets))
                       (setq-local gptel--known-presets (copy-tree gptel--known-presets)))
                     (macher-agent--apply-preset '(boot-preset))
                     (expect macher-agent--boot-directive :to-equal "Initial boot directive text"))
                   (kill-buffer test-buf))))

           (it "sets buffer-local macher-agent--boot-directive via macher-agent-use-skill, macher-agent-add-subagent, and macher-agent--apply-payload-locally"
               (let* ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                               (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces))))
                 (setf (alist-get 'boot-preset (macher-agent-workspace-skills-alist ctx))
                       '(:system "System prompt" :description "Boot Preset" :boot-directive "Initial boot directive text"))
                 (macher-agent-initialize-skills ctx)
                 ;; Test macher-agent-use-skill
                 (let ((use-buf (generate-new-buffer "test-use-skill-buf")))
                   (macher-agent-use-skill 'boot-preset use-buf)
                   (with-current-buffer use-buf
                     (expect macher-agent--boot-directive :to-equal "Initial boot directive text"))
                   (kill-buffer use-buf))
                 ;; Test macher-agent-add-subagent
                 (let* ((parent-buf (current-buffer))
                        (sub-buf (macher-agent-add-subagent "test-subagent-boot-buf" '("boot-preset") parent-buf nil ctx)))
                   (with-current-buffer sub-buf
                     (expect macher-agent--boot-directive :to-equal "Initial boot directive text"))
                   (kill-buffer sub-buf))
                 ;; Test macher-agent--apply-payload-locally
                 (let ((local-buf (generate-new-buffer "test-apply-payload-buf")))
                   (with-current-buffer local-buf
                     (macher-agent--apply-payload-locally '(:boot-directive "Direct boot directive"))
                     (expect macher-agent--boot-directive :to-equal "Direct boot directive"))
                   (kill-buffer local-buf))))

           (it "prioritises virtual edits inside the VFS when parsing SKILL.md"
               (let* ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                               (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces)))
                      (vfs-file "tests/fixtures/skills/global/SKILL.md"))
                 (macher-agent--set-context-contents 
                  ctx 
                  (list (macher-agent-vfs-make-entry 
                         vfs-file 
                         "original content on disk" 
                         "---\nname: virtual-skill\ndescription: A virtual test skill\n---\nThis body is completely virtual.")))
                 (let ((parsed (macher-agent-parse-skill-file vfs-file ctx)))
                   (expect (plist-get parsed :name) :to-equal "virtual-skill")
                   (expect (plist-get parsed :body) :to-equal "This body is completely virtual."))))

           (it "resolves global skill tools by loading their script if not registered"
               (let* ((loaded-tool-object (gptel-make-tool :name "mock-tool-load" :category "test")))
                 (setq mock-tool-load-global loaded-tool-object)
                 (spy-on 'file-exists-p :and-return-value t)
                 (spy-on 'insert-file-contents :and-call-fake (lambda (f) (insert "(setq mock-tool-load mock-tool-load-global)")))
                 (let ((resolved (macher-agent-resolve-tool "mock-tool-load" nil "tests/fixtures/skills/global/")))
                   (expect resolved :to-equal loaded-tool-object))))

           (it "refuses to load workspace skill tools (security context)"
               (let* ((mock-script-dir (expand-file-name "tests/fixtures/skills/workspace/scripts"))
                      (mock-script-path (expand-file-name "workspace-tool-1.el" mock-script-dir)))
                 ;; Setup mock script
                 (make-directory mock-script-dir t)
                 (with-temp-file mock-script-path
                   (insert "(setq workspace-tool-1 'workspace-loaded)"))
                 
                 ;; Test workspace parsing logic
                 (let ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                                (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces))))
                   (macher-agent--load-skill-from-path "tests/fixtures/skills/workspace/" ctx)
                   (let ((skill-meta (alist-get 'workspace-skill (macher-agent-workspace-skills-alist ctx))))
                     (expect (plist-get skill-meta :context-dir) :to-be nil)))
                 
                 ;; Resolution should fail to load because context-dir is nil,
                 ;; returning the raw string fallback instead of a loaded tool object.
                 (let ((resolved (macher-agent-resolve-tool "workspace-tool-1" nil nil)))
                   (expect resolved :to-equal "workspace-tool-1"))
                 
                 (delete-directory mock-script-dir t)))

           (it "verifies tool resolution hierarchy (workspace shadows package tools)"
               (let* ((pkg-dir (make-temp-file "macher-pkg" t))
                      (ws-dir (make-temp-file "macher-ws" t))
                      (pkg-scripts (expand-file-name "scripts" pkg-dir))
                      (ws-scripts (expand-file-name "scripts" ws-dir))
                      (ws-a (gptel-make-tool :name "tool-a" :category "test1"))
                      (pkg-b (gptel-make-tool :name "tool-b" :category "test2")))
                 (setq mock-ws-a-global ws-a)
                 (setq mock-pkg-b-global pkg-b)
                 (make-directory pkg-scripts t)
                 (make-directory ws-scripts t)
                 ;; Package provides tool-a and tool-b
                 (with-temp-file (expand-file-name "tool-a.el" pkg-scripts) (insert "(setq tool-a 'pkg-a)"))
                 (with-temp-file (expand-file-name "tool-b.el" pkg-scripts) (insert "(setq tool-b mock-pkg-b-global)"))
                 ;; Workspace overrides tool-a
                 (with-temp-file (expand-file-name "tool-a.el" ws-scripts) (insert "(setq tool-a mock-ws-a-global)"))
                 
                 ;; Clear registry
                 (let ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                                (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces))))
                   (clrhash (macher-agent-workspace-tools-registry ctx)))
                 
                 ;; Resolve pkg first, then workspace shadows
                 (let* ((res-pkg-b (macher-agent-resolve-tool "tool-b" nil pkg-dir))
                        (res-ws-a (macher-agent-resolve-tool "tool-a" nil ws-dir)))
                   (expect res-pkg-b :to-equal pkg-b)
                   (expect res-ws-a :to-equal ws-a))
                 
                 (delete-directory pkg-dir t)
                 (delete-directory ws-dir t)))
           
           (it "applies skill tools correctly into gptel-tools when selected"
               (let* ((gptel-tools nil)
                      (gptel--known-presets nil)
                      (gptel-directives nil)
                      (mock-tool-obj (if (fboundp 'gptel-make-tool)
                                         (gptel-make-tool :name "the_tool" :function (lambda () nil) :description "A tool")
                                       'the-tool)))
                 (spy-on 'gptel-tool-p :and-return-value t)
                 (let ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                                (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces))))
                   (puthash "selected-tool" mock-tool-obj (macher-agent-workspace-tools-registry ctx))
                   (setf (alist-get 'test-preset (macher-agent-workspace-skills-alist ctx))
                         (list :description "test" :system "test system" :tools (list mock-tool-obj) :context-dir nil))
                   
                   (with-temp-buffer
                     (let ((gptel--known-presets nil))
                       (macher-agent-initialize-skills ctx)
                       (let ((preset-def (buffer-local-value 'gptel--known-presets (current-buffer))))
                         (setq preset-def (alist-get 'test-preset preset-def))
                         (expect preset-def :not :to-be nil)
                         (expect (plist-get preset-def :tools) :to-equal `(:append (,mock-tool-obj)))))))))

           (it "merges and applies buffer-local macher-agent-presets during composed skill evaluation"
               (let* ((gptel-tools nil)
                      (gptel--known-presets nil)
                      (gptel-directives nil)
                      (mock-tool-obj (if (fboundp 'gptel-make-tool)
                                         (gptel-make-tool :name "the_tool" :function (lambda () nil) :description "A tool")
                                       'the-tool)))
                 (spy-on 'gptel-tool-p :and-return-value t)
                 (let ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                                (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces))))
                   (puthash "selected-tool" mock-tool-obj (macher-agent-workspace-tools-registry ctx))
                   (setf (alist-get 'test-preset (macher-agent-workspace-skills-alist ctx))
                         (list :description "test" :system "test system" :tools (list mock-tool-obj) :context-dir nil))
                   
                   (with-temp-buffer
                     (let ((gptel--known-presets nil))
                       (macher-agent-initialize-skills ctx)
                       (setq-local macher-agent-presets '(test-preset))
                       (setq-local gptel-tools nil)
                       (let ((payload (macher-agent-compose-payload 
                                       (list :model nil :system nil :temperature nil :max-tokens nil :tools nil :known-presets gptel--known-presets)
                                       '(test-preset))))
                         (macher-agent--apply-payload-locally payload))
                       (expect gptel-tools :not :to-be nil)
                       (expect (car gptel-tools) :to-equal mock-tool-obj))))))

           (it "expands org-macros in SKILL.md body"
               (let* ((parsed (macher-agent-parse-skill-file "tests/fixtures/skills/macro-skill/SKILL.md" macher-agent--persistent-context)))
                 (expect (plist-get parsed :body) :to-match "Version: 0.1.0")))

           (it "creates a preset when allowed-tools is provided"
               (let* ((mock-dir (make-temp-file "macher-test-skills-preset" t))
                      (skill-dir (expand-file-name "test-skill" mock-dir)))
                 (make-directory skill-dir t)
                 (with-temp-file (expand-file-name "SKILL.md" skill-dir)
                   (insert "---\nname: my-preset\ndescription: test\nallowed-tools:\n  - some-tool\nmodel: gpt-4o\n---\nPreset body"))
                 (spy-on 'macher-agent-resolve-tool :and-return-value "some-tool")
                 (with-temp-buffer
                   (let ((gptel-directives nil)
                         (gptel--known-presets nil))
                     (let ((ctx (make-macher-agent-context :project-root mock-dir)))
                       (macher-agent-initialize-skills ctx))
                     
                     (let ((preset-def (alist-get 'my-preset gptel--known-presets)))
                       (expect preset-def :not :to-be nil)
                       (expect (plist-get preset-def :system) :to-equal "Preset body")
                       (expect (plist-get preset-def :model) :to-equal 'gpt-4o)
                       (expect (plist-get preset-def :tools) :to-equal '(:append ("some-tool"))))
                     (delete-directory mock-dir t)))))

           (it "injects directly into gptel-directives when allowed-tools is omitted"
               (let* ((mock-dir (make-temp-file "macher-test-skills-directive" t))
                      (skill-dir (expand-file-name "test-skill" mock-dir)))
                 (make-directory skill-dir t)
                 (with-temp-file (expand-file-name "SKILL.md" skill-dir)
                   (insert "---\nname: my-directive\n---\nDirective body"))
                 (with-temp-buffer
                   (let ((gptel-directives nil)
                         (gptel--known-presets nil))
                     (let ((ctx (make-macher-agent-context :project-root mock-dir)))
                       (macher-agent-initialize-skills ctx))
                     (expect (alist-get 'my-directive gptel-directives) :to-equal "Directive body")
                     (let ((preset-def (alist-get 'my-directive gptel--known-presets)))
                       (expect preset-def :not :to-be nil)
                       (expect (plist-get preset-def :system) :to-equal "Directive body"))
                     (delete-directory mock-dir t)))))

           (it "stores ptc-primitives in gptel--known-presets and composes them into payload"
               (let* ((mock-dir (make-temp-file "macher-test-skills-ptc" t))
                      (skill-dir (expand-file-name "test-skill" mock-dir)))
                 (make-directory skill-dir t)
                 (with-temp-file (expand-file-name "SKILL.md" skill-dir)
                   (insert "---\nname: my-ptc-preset\ndescription: test ptc\nptc-primitives:\n  - spawn_subagent\n  - delegate_tasks\n---\nPTC preset body"))
                 (with-temp-buffer
                   (let ((gptel-directives nil)
                         (gptel--known-presets nil))
                     (let ((ctx (make-macher-agent-context :project-root mock-dir)))
                       (macher-agent-initialize-skills ctx))
                     (let ((preset-def (alist-get 'my-ptc-preset gptel--known-presets)))
                       (expect preset-def :not :to-be nil)
                       (expect (plist-get preset-def :ptc-primitives) :to-equal '(spawn_subagent delegate_tasks))
                       (let ((composed (macher-agent-compose-payload
                                        (list :known-presets gptel--known-presets)
                                        '(my-ptc-preset))))
                         (expect (plist-get composed :ptc-primitives) :to-equal '(spawn_subagent delegate_tasks))))
                     (delete-directory mock-dir t)))))

           (describe "Skills Media Support"
                     (it "detects media file extensions"
                         (expect (macher-agent-media-file-p "image.png") :to-be-truthy)
                         (expect (macher-agent-media-file-p "photo.jpg") :to-be-truthy)
                         (expect (macher-agent-media-file-p "code.el") :to-be nil))))

  (describe "Canonical Context and Direct Plugin Access in Skills Scripts"
    (it "verifies all skills/scripts and presets files contain no occurrences of obsolete context helpers"
      (let* ((file-name (or load-file-name buffer-file-name))
             (test-dir (if file-name (file-name-directory file-name) default-directory))
             (root-dir (or (locate-dominating-file test-dir "skills") test-dir))
             (scripts-dir (expand-file-name "skills/scripts" root-dir))
             (script-files (directory-files scripts-dir t "\\.el$"))
             (preset-file (expand-file-name "macher-agent-presets.el" root-dir))
             (all-files (cons preset-file script-files)))
        (dolist (file all-files)
          (when (file-exists-p file)
            (let ((content (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))))
              (expect (string-match-p "macher-agent--get-context-data" content) :to-be nil)
              (expect (string-match-p "macher-agent--set-context-data" content) :to-be nil)
              (expect (string-match-p "macher-agent--get-context-workspace" content) :to-be nil))))))

    (describe "delegate_tasks_to_subagents strict positional and transit payload"
      (it "aggregates results strictly from :payload and :error without legacy fallback guessing"
        (let* ((ctx (macher-agent--make-context))
               (tool-fn (get 'macher-agent-delegate-tasks-to-subagents-tool 'ptc-function))
               (tasks (list (list :buffer_name "worker-ok" :instructions "Task ok")
                            (list :buffer_name "worker-err" :instructions "Task err")))
               (callback-called nil))
          (spy-on 'macher-agent-a2a-dispatch
                  :and-call-fake (lambda (payloads callback &optional context)
                                   (funcall callback (vector (list :payload "Result from worker-ok")
                                                             (list :error "Failed in worker-err")))))
          (funcall tool-fn tasks ctx (lambda (res) (setq callback-called res)))
          (expect callback-called :to-match "Result from worker-ok")
          (expect callback-called :to-match "Failed in worker-err")))

      (it "defaults ephemeral to true when omitted from task"
        (let* ((ctx (macher-agent--make-context))
               (tool-fn (get 'macher-agent-delegate-tasks-to-subagents-tool 'ptc-function))
               (tasks (vector (list :buffer_name "sub-worker-eph"
                                    :instructions "Run subtask default ephemeral")))
               (dispatched-payloads nil))
          (spy-on 'macher-agent-a2a-dispatch
                  :and-call-fake (lambda (payloads callback &optional context)
                                   (setq dispatched-payloads payloads)
                                   (funcall callback (vector (list :payload "Done")))))
          (funcall tool-fn tasks ctx #'ignore)
          (expect dispatched-payloads :not :to-be nil)
          (let ((payload (car dispatched-payloads)))
            (expect (plist-get (macher-agent-transit-payload-metadata payload) :ephemeral) :to-be t))))

      (it "invokes gptel presentation function with callback and tasks positionally"
        (let* ((ctx (macher-agent--make-context))
               (pres-fn (gptel-tool-function macher-agent-delegate-tasks-to-subagents-tool))
               (tasks (vector (list :buffer_name "pres-worker" :instructions "Presentation task")))
               (callback-called nil))
          (spy-on 'macher-agent-a2a-dispatch
                  :and-call-fake (lambda (payloads callback &optional context)
                                   (funcall callback (vector (list :payload "Presentation result")))))
          (funcall pres-fn (lambda (res) (setq callback-called res)) tasks)
          (expect callback-called :to-match "Presentation result"))))))

(provide 'macher-agent-skills-test)
;;; macher-agent-skills-test.el ends here
