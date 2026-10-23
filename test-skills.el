;;; test-skills.el --- Skill, preset, and command composition -*- lexical-binding: t; -*-

;;; Commentary:
;; Specification 5: Skill, preset, and command composition
;; SKL-01, SKL-02, SKL-03, SKL-04, SKL-05, SKL-06, SKL-07

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
          (expect (member "search_conversation_history" tool-names) :to-be-truthy))))))

(provide 'test-skills)
;;; test-skills.el ends here
