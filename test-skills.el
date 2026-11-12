;;; test-skills.el --- Skill, preset, and command composition -*- lexical-binding: t; -*-

;;; Commentary:
;; Specification 5: Skill, preset, and command composition
;; SKL-01, SKL-02, SKL-03, SKL-04, SKL-05, SKL-06, SKL-07, SKL-08, SKL-09, SKL-10

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-test-helpers)

(describe "5. Skill, preset, and command composition"

  (before-each
    (macher-agent-test-reset-env))

  ;; --------------------------------------------------------------------------
  ;; SKL-01
  ;; --------------------------------------------------------------------------
  (it "SKL-01: parses new SKILL.md and tool script from VFS and registers in workspace registry and presets"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (let* ((skill-content (concat "---\n"
                                   "name: dynamic-worker\n"
                                   "description: Dynamic worker from VFS\n"
                                   "allowed-tools:\n"
                                   "  - dynamic_vfs_tool\n"
                                   "---\n"
                                   "Dynamic worker system instructions.\n"))
             (tool-content (concat "(gptel-make-tool\n"
                                  " :name \"dynamic_vfs_tool\"\n"
                                  " :description \"Dynamic tool from VFS\"\n"
                                  " :category \"misc\"\n"
                                  " :args nil\n"
                                  " :function #'ignore)\n"))
             (skill-path "skills/dynamic-worker/SKILL.md")
             (tool-path "skills/dynamic-worker/scripts/dynamic_vfs_tool.el"))
        ;; Create SKILL.md and tool script in VFS without saving to disk
        (macher-agent--update-context-file ctx skill-path skill-content)
        (macher-agent--update-context-file ctx tool-path tool-content)
        (expect (file-exists-p (expand-file-name skill-path root)) :to-be nil)
        (expect (file-exists-p (expand-file-name tool-path root)) :to-be nil)

        ;; Trigger mutation dispatcher
        (macher-agent--mutation-dispatcher skill-path ctx)

        ;; Tool is registered in the workspace-local registry
        (let* ((tools-reg (macher-agent-workspace-tools-registry ctx))
               (tool-obj (gethash "dynamic_vfs_tool" tools-reg)))
          (expect tool-obj :not :to-be nil)
          (expect (gptel-tool-name tool-obj) :to-equal "dynamic_vfs_tool"))

        ;; Preset is published in gptel--known-presets
        (let ((preset-entry (assq 'dynamic-worker gptel--known-presets)))
          (expect preset-entry :not :to-be nil)
          (expect (plist-get (cdr preset-entry) :description) :to-equal "Dynamic worker from VFS")
          (expect (plist-get (cdr preset-entry) :system) :to-match "Dynamic worker system instructions")))))

  ;; --------------------------------------------------------------------------
  ;; SKL-02
  ;; --------------------------------------------------------------------------
  (it "SKL-02: composes additive skills with ### Skill: headers, merged tools, and parameter overrides"
    (let* ((tool1 (gptel-make-tool :name "tool_one" :description "Tool 1" :category "misc"))
           (tool2 (gptel-make-tool :name "tool_two" :description "Tool 2" :category "misc"))
           (gptel-tools (list tool1 tool2))
           (base-state (list :system "Base instructions\n"
                             :model 'gpt-3.5-turbo
                             :temperature 0.7
                             :tools nil
                             :known-presets
                             `((skill-alpha . (:system "Alpha instructions"
                                               :tools (:append (,tool1))
                                               :model gpt-4o
                                               :temperature 0.5))
                               (skill-beta . (:system "Beta instructions"
                                              :tools (:append (,tool2))
                                              :model claude-3-5-sonnet
                                              :temperature 0.2)))))
           (composed (macher-agent-compose-payload base-state '(skill-alpha skill-beta))))

      ;; System prompts are concatenated with ### Skill: headers without duplication
      (let ((sys (plist-get composed :system)))
        (expect sys :to-match "### Skill: skill-alpha\nAlpha instructions")
        (expect sys :to-match "### Skill: skill-beta\nBeta instructions")
        ;; No duplicate headers
        (expect (cl-count "### Skill: skill-alpha" (split-string sys "\n") :test #'string=)
                :to-equal 1))

      ;; Tool lists are merged and normalised into gptel-tool structs
      (let ((tools (plist-get composed :tools)))
        (expect (length tools) :to-equal 2)
        (expect (mapcar #'gptel-tool-name tools) :to-equal '("tool_one" "tool_two")))

      ;; Later model parameters override earlier ones
      (expect (plist-get composed :model) :to-equal 'claude-3-5-sonnet)
      (expect (plist-get composed :temperature) :to-equal 0.2)))

  ;; --------------------------------------------------------------------------
  ;; SKL-03
  ;; --------------------------------------------------------------------------
  (it "SKL-03: clears prior system prompt, tools, boot directives, and primitives when composing exclusive skill"
    (let* ((tool-base (gptel-make-tool :name "tool_base" :description "Base"))
           (tool-excl (gptel-make-tool :name "tool_excl" :description "Excl"))
           (gptel-tools (list tool-base tool-excl))
           (base-state (list :system "Initial base prompt\n"
                             :tools (list tool-base)
                             :boot-directive "Setup base database\n"
                             :ptc-primitives '(base-primitive)
                             :model 'gpt-4o
                             :temperature 0.7
                             :known-presets
                             `((base-skill . (:system "Base skill instructions"
                                              :tools (,tool-base)
                                              :boot-directive "Base boot"))
                               (exclusive-skill . (:system "Strict exclusive persona"
                                                   :exclusive t
                                                   :tools (,tool-excl)
                                                   :temperature 0.1)))))
           (composed (macher-agent-compose-payload base-state '(base-skill exclusive-skill))))

      ;; Exclusive skill clears all prior prompt text and retains only its own
      (let ((sys (plist-get composed :system)))
        (expect sys :to-match "### Skill: exclusive-skill\nStrict exclusive persona")
        (expect sys :not :to-match "Initial base prompt")
        (expect sys :not :to-match "Base skill instructions"))

      ;; Prior tools and boot directives are cleared
      (let ((tools (plist-get composed :tools)))
        (expect (length tools) :to-equal 1)
        (expect (gptel-tool-name (car tools)) :to-equal "tool_excl"))
      (expect (plist-get composed :boot-directive) :to-be nil)
      (expect (plist-get composed :ptc-primitives) :to-be nil)
      (expect (plist-get composed :temperature) :to-equal 0.1)))

  ;; --------------------------------------------------------------------------
  ;; SKL-04
  ;; --------------------------------------------------------------------------
  (it "SKL-04: strips inline @skill tags at start and after whitespace while preserving user@domain emails"
    (with-temp-buffer
      (let* ((orig-buf (current-buffer))
             (prompt-buf (generate-new-buffer " *skl04-prompt*"))
             (fsm (gptel-make-fsm :info (list :buffer orig-buf :data prompt-buf)))
             (macher-agent--active-fsm fsm)
             (coder-tool (gptel-make-tool :name "coder_tool" :description "Coder"))
             (known-presets `((coder . (:system "You are an expert coder."
                                        :tools (,coder-tool)))
                              (tester . (:system "You are an expert tester.")))))
        (unwind-protect
            (progn
              (setq-local gptel--known-presets known-presets)
              (with-current-buffer prompt-buf
                (insert "@coder Please review the patch. Send notice to dev@example.com or @tester for signoff.\n")
                (macher-agent-sync-prompt-transformer nil fsm)
                ;; Strips inline tags at start of line and after whitespace, leaves email untouched
                (let ((text (buffer-string)))
                  (expect text :not :to-match "@coder")
                  (expect text :not :to-match "@tester")
                  (expect text :to-match "dev@example.com")
                  (expect text :to-match "Please review the patch. Send notice to dev@example.com or for signoff.")))
              ;; Applies skill system prompt and tools to active FSM
              (let ((inf (gptel-fsm-info fsm)))
                (expect (plist-get inf :system) :to-match "You are an expert coder")
                (expect (cl-some (lambda (tl) (equal (gptel-tool-name tl) "coder_tool"))
                                 (plist-get inf :tools))
                        :to-be-truthy)))
          (when (buffer-live-p prompt-buf)
            (kill-buffer prompt-buf))))))

  ;; --------------------------------------------------------------------------
  ;; SKL-05
  ;; --------------------------------------------------------------------------
  (it "SKL-05: interpolates markdown command arguments ($arguments, $args, $1, $2) and preserves base system prompt"
    (with-temp-buffer
      (let* ((buf (current-buffer))
             (ctx (macher-agent--make-context :id "cmd-ctx" :project-root "/tmp/mock-proj"
                                              :prompt "@review src/lib.rs \"check bounds\""))
             (cmd-body "Audit file $1 focusing on $2.\nFull command args: $arguments (alias: $args)")
             (cmd-preset (list :system cmd-body :is-command t)))
        (setq-local macher-agent--persistent-context ctx)
        (setq-local gptel-system-prompt "Base workspace system persona.")
        (setq-local gptel--known-presets (list (cons 'review cmd-preset)))
        ;; Compile transmission payload with command redirection
        (let ((state (macher-agent--compile-transmission-payload buf nil '(review) 'review ctx)))
          ;; User prompt text is replaced with interpolated command body
          (let ((redirect (macher-agent-transmission-state-redirect-prompt state)))
            (expect redirect :to-match "Audit file src/lib.rs focusing on check bounds.")
            (expect redirect :to-match "Full command args: src/lib.rs \"check bounds\" (alias: src/lib.rs \"check bounds\")"))
          ;; gptel-system-prompt retains base workspace system prompt
          (expect (macher-agent-transmission-state-compiled-prompt state)
                  :to-equal "Base workspace system persona.")))))

  ;; --------------------------------------------------------------------------
  ;; SKL-06
  ;; --------------------------------------------------------------------------
  (it "SKL-06: replaces empty command arguments with empty string and leaves unfulfilled $1 literal"
    (let ((template "Command template: arguments=[$arguments], args=[$args], pos1=[$1]"))
      ;; Call with empty argument string
      (let ((interpolated-empty (macher-agent--interpolate-command-body template ""))
            (interpolated-nil (macher-agent--interpolate-command-body template nil)))
        ;; $arguments and $args are replaced with empty string
        (expect interpolated-empty :to-match "arguments=\\[\\], args=\\[\\]")
        (expect interpolated-nil :to-match "arguments=\\[\\], args=\\[\\]")
        ;; Unfulfilled positional placeholder $1 remains literal without error
        (expect interpolated-empty :to-match "pos1=\\[\\$1\\]")
        (expect interpolated-nil :to-match "pos1=\\[\\$1\\]"))))

  ;; --------------------------------------------------------------------------
  ;; SKL-07
  ;; --------------------------------------------------------------------------
  (it "SKL-07: appends search_conversation_history and deduplicates tool blocks beyond limit"
    (with-temp-buffer
      (let* ((orig-buf (current-buffer))
             (macher-agent-max-context-chars '((nil . 50)))
             (macher-agent-max-duplicate-tools 2)
             (block "```tool (:name \"read_file\" :args (\"main.rs\"))\nfn main() {}\n```\n"))
        ;; Insert 4 identical tool result blocks with gptel property
        (insert "User initial request\n")
        (dotimes (_ 4)
          (let ((start (point)))
            (insert block)
            (put-text-property start (point) 'gptel t)))
        (insert "Latest user prompt")

        ;; 1. Deduplicate tools transformer replaces older duplicate blocks beyond limit 2
        (macher-agent-transformer-deduplicate-tools nil nil)
        (let ((content (buffer-string)))
          (expect content :to-match "{\"status\": \"omitted\", \"reason\": \"duplicate\"}")
          ;; Two recent occurrences retained, two older omitted
          (expect (cl-count "{\"status\": \"omitted\", \"reason\": \"duplicate\"}"
                            (split-string content "\n") :test #'string=)
                  :to-equal 2))

        ;; 2. Transmission memory step appends search_conversation_history when buffer exceeds max-chars
        (let* ((state (make-macher-agent-transmission-state
                       :target-buffer orig-buf
                       :tools (list (gptel-make-tool :name "other_tool" :description "Other"))))
               (updated (macher-agent-memory-pipe--inject-tool state))
               (tool-names (mapcar #'macher-agent-canonical-tool-name
                                   (macher-agent-transmission-state-tools updated))))
          (expect (member "search_conversation_history" tool-names) :to-be-truthy)))))

  ;; --------------------------------------------------------------------------
  ;; SKL-08
  ;; --------------------------------------------------------------------------
  (it "SKL-08: exclusive skill with :parents retains its parent's system prompt/tools while wiping prior presets"
    (let* ((tool-prior (gptel-make-tool :name "tool_prior" :description "Prior"))
           (tool-parent (gptel-make-tool :name "tool_parent" :description "Parent"))
           (tool-child (gptel-make-tool :name "tool_child" :description "Child"))
           (base-state (list :system "Prior base prompt\n"
                             :tools (list tool-prior)
                             :known-presets
                             `((prior-skill . (:system "Prior skill instructions"
                                               :tools (,tool-prior)))
                               (parent-skill . (:system "Parent skill instructions"
                                                :tools (,tool-parent)))
                               (exclusive-child . (:system "Child exclusive persona"
                                                   :exclusive t
                                                   :parents (parent-skill)
                                                   :tools (,tool-child))))))
           (composed (macher-agent-compose-payload base-state '(prior-skill exclusive-child))))
      ;; Prior skill wiped
      (let ((sys (plist-get composed :system)))
        (expect sys :not :to-match "Prior base prompt")
        (expect sys :not :to-match "Prior skill instructions")
        ;; Parent prompt retained
        (expect sys :to-match "### Skill: parent-skill\nParent skill instructions")
        ;; Child prompt retained
        (expect sys :to-match "### Skill: exclusive-child\nChild exclusive persona"))
      ;; Tools: prior tool wiped, parent tool and child tool retained
      (let* ((tools (plist-get composed :tools))
             (tool-names (mapcar #'gptel-tool-name tools)))
        (expect (member "tool_prior" tool-names) :to-be nil)
        (expect (member "tool_parent" tool-names) :to-be-truthy)
        (expect (member "tool_child" tool-names) :to-be-truthy))))

  ;; --------------------------------------------------------------------------
  ;; SKL-09
  ;; --------------------------------------------------------------------------
  (it "SKL-09: single-pass command interpolation does not re-expand literal $2 inside $1's value"
    (let* ((template "Pos1: $1, Pos2: $2")
           (user-args "$2 target-value")
           (result (macher-agent--interpolate-command-body template user-args)))
      ;; $1 is "$2", $2 is "target-value"
      ;; In single-pass, pos1 must remain literal "$2" and not be expanded to "target-value"
      (expect result :to-equal "Pos1: $2, Pos2: target-value")))

  ;; --------------------------------------------------------------------------
  ;; SKL-10
  ;; --------------------------------------------------------------------------
  (it "SKL-10: macher-agent--secure-ast-p rejects fset, eval, advice-add, and setq-default"
    (expect (macher-agent--secure-ast-p '(fset 'foo (lambda () 1))) :to-be nil)
    (expect (macher-agent--secure-ast-p '(eval '(message "pwn"))) :to-be nil)
    (expect (macher-agent--secure-ast-p '(advice-add 'find-file :override #'ignore)) :to-be nil)
    (expect (macher-agent--secure-ast-p '(setq-default fill-column 80)) :to-be nil)
    ;; Nested inside other forms
    (expect (macher-agent--secure-ast-p '(progn (let ((x 1)) (eval x)))) :to-be nil)
    ;; Safe forms are permitted
    (expect (macher-agent--secure-ast-p '(setq foo 42)) :to-be t)
    (expect (macher-agent--secure-ast-p '(gptel-make-tool :name "test")) :to-be t))

  ;; --------------------------------------------------------------------------
  ;; SKL-11
  ;; --------------------------------------------------------------------------
  (it "SKL-11: strips gptel and text properties on yank inside macher-agent-mode"
    (macher-agent-test-with-git-workspace (root _ctx buf)
      (with-current-buffer buf
        (setq-local default-directory root)
        (macher-agent-mode 1)
        (expect (lookup-key macher-agent-mode-map [remap yank])
                :to-equal #'macher-agent-yank)
        (let ((kill-ring (list (propertize "copied response text"
                                           'gptel 'response
                                           'gptel-tool t
                                           'face 'bold)))
              (kill-ring-yank-pointer nil))
          (setq kill-ring-yank-pointer kill-ring)
          (macher-agent-yank)
          (expect (buffer-string) :to-equal "copied response text")
          (expect (text-properties-at (point-min)) :to-be nil)
          (expect (get-text-property (point-min) 'gptel) :to-be nil)))))

  ;; --------------------------------------------------------------------------
  ;; SKL-12
  ;; --------------------------------------------------------------------------
  (it "SKL-12: executes mid-turn use_skill switch and done restoration with FSM :data sync, use_skill retention, boot-directive consumption, and zero tool carry-over"
    (macher-agent-test-with-git-workspace (root ctx buf)
      (let* ((reader-tool (gptel-make-tool :name "reader_tool" :description "Read" :category "misc"))
             (coder-tool (gptel-make-tool :name "coder_edit_tool" :description "Edit" :category "misc"))
             (use-skill-tool macher-agent-use-skill-tool)
             (known `((orchestrator . (:system "Orchestrator instructions"
                                       :description "Root orchestrator"
                                       :tools (,use-skill-tool ,reader-tool)
                                       :boot-directive "BOOT: Orchestrator init"))
                      (domain-coder . (:system "Domain coder instructions"
                                       :description "Non-exclusive coder omitting use_skill"
                                       :model claude-sonnet
                                       :tools (,coder-tool)
                                       :boot-directive "BOOT: Coder activated")))))
        (with-current-buffer buf
          (setq-local gptel--known-presets known)
          (setf (macher-agent-context-skills ctx) known)
          (macher-agent--sync-gptel-known-presets known)
          (macher-agent-use-skill 'orchestrator buf)

          ;; Turn 1: initial compilation consumes orchestrator boot directive
          (let ((st1 (macher-agent--compile-transmission-payload
                      buf '(orchestrator) '(orchestrator) nil ctx)))
            (expect (macher-agent-transmission-state-compiled-prompt st1)
                    :to-match "BOOT: Orchestrator init"))

          ;; Simulate assistant turn in buffer (setting 'gptel 'response text property)
          (let ((beg (point-max)))
            (goto-char beg)
            (insert "I will activate domain-coder now.\n")
            (put-text-property beg (point-max) 'gptel 'response))

          ;; Bind an active mid-turn FSM with OpenAI-compatible :data payload
          (let* ((init-data (list :model "gpt-4o"
                                  :messages (vector (list :role "system" :content "old-sys")
                                                    (list :role "user" :content "fix code"))
                                  :tools (vector (list :type "function" :function (list :name "reader_tool")))))
                 (fsm (gptel-make-fsm
                       :state 'TOOL
                       :info (list :buffer buf
                                   :origin-buffer buf
                                   :macher-agent-context ctx
                                   :tools (list use-skill-tool reader-tool)
                                   :data init-data))))
            (setq-local macher-agent--active-fsm fsm)

            ;; 1. Activate non-exclusive `domain-coder' mid-turn
            (let ((res (funcall (get 'macher-agent-use-skill-tool 'ptc-function)
                                "domain-coder" nil ctx)))
              (expect res :to-match "Activated skill 'domain-coder'")
              (expect res :to-match "coder_edit_tool")
              (expect res :to-match "use_skill"))

            ;; FSM :data was immediately updated for the TRET -> WAIT continuation leg:
            ;; - system message holds domain-coder + BOOT: Coder activated (despite 'gptel 'response!)
            ;; - orchestrator system prompt and reader_tool did NOT carry over
            ;; - use_skill WAS automatically retained so agent can exit
            (let* ((info (gptel-fsm-info fsm))
                   (data (plist-get info :data))
                   (sys-msg (plist-get (aref (plist-get data :messages) 0) :content))
                   (fsm-tool-names (mapcar #'macher-agent-canonical-tool-name
                                           (plist-get info :tools))))
              (expect (plist-get data :model) :to-equal "claude-sonnet")
              (expect sys-msg :to-match "### Skill: domain-coder\nDomain coder instructions")
              (expect sys-msg :to-match "BOOT: Coder activated")
              (expect sys-msg :not :to-match "Orchestrator instructions")
              (expect (member "coder_edit_tool" fsm-tool-names) :to-be-truthy)
              (expect (member "use_skill" fsm-tool-names) :to-be-truthy)
              (expect (member "reader_tool" fsm-tool-names) :to-be nil)
              (expect (> (length (plist-get data :tools)) 0) :to-be t))

            ;; Scope check allows use_skill & coder_edit_tool, blocks reader_tool
            (expect (macher-agent--enforce-tool-scope "use_skill" fsm) :to-be nil)
            (expect (macher-agent--enforce-tool-scope "coder_edit_tool" fsm) :to-be nil)
            (expect (plist-get (macher-agent--enforce-tool-scope "reader_tool" fsm) :block)
                    :to-match "not accessible")

            ;; Next turn does NOT re-inject BOOT: Coder activated
            (let ((st-next (macher-agent--compile-transmission-payload
                            buf '(domain-coder) '(domain-coder) nil ctx)))
              (expect (macher-agent-transmission-state-compiled-prompt st-next)
                      :not :to-match "BOOT: Coder activated"))

            ;; 2. Exit skill via use_skill(done: true)
            (let ((done-res (funcall (get 'macher-agent-use-skill-tool 'ptc-function)
                                     "done" t ctx)))
              (expect done-res :to-match "Restored original presets"))

            (let* ((info (gptel-fsm-info fsm))
                   (fsm-tool-names (mapcar #'macher-agent-canonical-tool-name
                                           (plist-get info :tools))))
              (expect macher-agent-presets :to-equal '(orchestrator))
              (expect (macher-agent-get-cached-presets buf) :to-be nil)
              (expect (member "reader_tool" fsm-tool-names) :to-be-truthy)
              (expect (member "use_skill" fsm-tool-names) :to-be-truthy)
              (expect (member "coder_edit_tool" fsm-tool-names) :to-be nil)))))))

  ;; --------------------------------------------------------------------------
  ;; SKL-13
  ;; --------------------------------------------------------------------------
  (it "SKL-13: excludes :is-command entries from <available_skills> and rejects commands in use_skill"
    (macher-agent-test-with-git-workspace (root ctx buf)
      (let* ((known `((worker-skill . (:system "Worker system"
                                       :description "Valid worker skill"))
                      (deploy-cmd . (:system "Deploy $1"
                                     :description "Slash command"
                                     :is-command t)))))
        (with-current-buffer buf
          (setq-local gptel--known-presets known)
          (setf (macher-agent-context-skills ctx) known)
          (let* ((state (make-macher-agent-transmission-state
                         :target-buffer buf
                         :context ctx
                         :known-presets known
                         :tools (list macher-agent-use-skill-tool)))
                 (after (macher-agent-pipe--inject-skill-directive state))
                 (dir-block (car (macher-agent-transmission-state-directives after))))
            (expect dir-block :to-match "<available_skills>")
            (expect dir-block :to-match "worker-skill: Valid worker skill")
            (expect dir-block :not :to-match "deploy-cmd"))

          ;; Calling use_skill on a command or unknown skill raises an error
          (expect (funcall (get 'macher-agent-use-skill-tool 'ptc-function)
                           "deploy-cmd" nil ctx)
                  :to-throw 'error)
          (expect (funcall (get 'macher-agent-use-skill-tool 'ptc-function)
                           "nonexistent-skill-xyz" nil ctx)
                  :to-throw 'error)))))

  ;; --------------------------------------------------------------------------
  ;; SKL-14
  ;; --------------------------------------------------------------------------
  (it "SKL-14: detects @command after mode prompt prefix ('### ') without erasing prior turns and ignores @tokens inside code fences"
    (with-temp-buffer
      (let* ((orig-buf (current-buffer))
             (prompt-buf (generate-new-buffer " *skl14-prompt*"))
             (ctx (macher-agent--make-context :id "skl14-ctx" :project-root "/tmp/mock"))
             (fsm (gptel-make-fsm :info (list :buffer orig-buf :macher-agent-context ctx)))
             (known `((review-cmd . (:system "Audit $1 assigned to $2"
                                     :is-command t))
                      (decorator . (:system "Should not match inside code fence")))))
        (unwind-protect
            (progn
              (setq-local macher-agent--persistent-context ctx)
              (setq-local gptel--known-presets known)
              (setf (macher-agent-context-skills ctx) known)
              ;; 1. Multi-turn buffer with Markdown prompt prefix "### " before @review-cmd
              (with-current-buffer prompt-buf
                (insert "### User turn 1\n")
                (let ((r-beg (point)))
                  (insert "Assistant turn 1 response.\n")
                  (put-text-property r-beg (point) 'gptel 'response))
                (insert "### @review-cmd src/core.el @alice\n")
                (macher-agent-sync-prompt-transformer nil fsm)
                (let ((full-text (buffer-string)))
                  ;; Prior conversation turns remain intact!
                  (expect full-text :to-match "Assistant turn 1 response\\.")
                  ;; Command body interpolated with @alice preserved as $2
                  (expect full-text :to-match "Audit src/core.el assigned to @alice")))

              ;; 2. @decorator inside fenced code block is NOT stripped
              (with-current-buffer prompt-buf
                (erase-buffer)
                (insert "### Please check:\n```python\n@decorator\ndef f(): pass\n```\n")
                (macher-agent-sync-prompt-transformer nil fsm)
                (expect (buffer-string) :to-match "@decorator")))
          (when (buffer-live-p prompt-buf)
            (kill-buffer prompt-buf)))))))

(provide 'test-skills)
;;; test-skills.el ends here
