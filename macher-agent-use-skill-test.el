;;; macher-agent-use-skill-test.el --- Tests for use_skill and skill directive injection -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests covering:
;; 1. Buffer-local cache state in macher-agent-core.el
;; 2. Dynamic skill directive injection in the transmission pipeline
;; 3. use_skill tool definition, presentation context, caching, switching, and restoration

;;; Code:

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
(require 'macher-agent-core)
(require 'macher-agent-gptel)
(require 'macher-agent-presets)
(require 'macher-agent-tools)

;; Ensure use_skill script is loaded
(let* ((root (locate-dominating-file default-directory "skills"))
       (script (expand-file-name "skills/scripts/use_skill.el" (or root default-directory))))
  (when (file-exists-p script)
    (load script nil t)))

(describe "use_skill and Dynamic Skill Directives"
  (macher-agent-test-setup-before-each)

  (describe "1. Buffer-Local Cache State (macher-agent--cached-presets)"
    (it "declares macher-agent--cached-presets as permanent-local"
      (expect (get 'macher-agent--cached-presets 'permanent-local) :to-be t))

    (it "gets and clears cached presets via accessor and clearing functions"
      (with-temp-buffer
        (expect (macher-agent-get-cached-presets) :to-be nil)
        (setq-local macher-agent--cached-presets '(preset-a preset-b))
        (expect (macher-agent-get-cached-presets) :to-equal '(preset-a preset-b))
        (expect (local-variable-p 'macher-agent--cached-presets) :to-be t)
        (macher-agent-clear-cached-presets)
        (expect (local-variable-p 'macher-agent--cached-presets) :to-be nil)
        (expect (macher-agent-get-cached-presets) :to-be nil)))

    (it "isolates cached presets across different buffers"
      (let ((buf1 (generate-new-buffer "test-cache-buf-1"))
            (buf2 (generate-new-buffer "test-cache-buf-2")))
        (unwind-protect
            (progn
              (with-current-buffer buf1
                (setq-local macher-agent--cached-presets '(skill-1)))
              (with-current-buffer buf2
                (expect (macher-agent-get-cached-presets) :to-be nil)
                (expect (local-variable-p 'macher-agent--cached-presets) :to-be nil))
              (expect (macher-agent-get-cached-presets buf1) :to-equal '(skill-1))
              (macher-agent-clear-cached-presets buf1)
              (expect (macher-agent-get-cached-presets buf1) :to-be nil)
              (expect (local-variable-p 'macher-agent--cached-presets buf1) :to-be nil))
          (kill-buffer buf1)
          (kill-buffer buf2)))))

  (describe "2. Dynamic Skill Directive Injection Pipeline Step"
    (it "asserts transmission state type in macher-agent-pipe--inject-skill-directive"
      (expect (macher-agent-pipe--inject-skill-directive "not-a-state")
              :to-throw 'wrong-type-argument))

    (it "returns state unchanged when use_skill tool is not present"
      (let* ((mock-tool (gptel-make-tool :name "submit_task_result" :description "finish" :function #'ignore))
             (state (make-macher-agent-transmission-state
                     :tools (list mock-tool)
                     :directives nil)))
        (setq state (macher-agent-pipe--inject-skill-directive state))
        (expect (macher-agent-transmission-state-directives state) :to-be nil)))

    (it "injects <available_skills> directive when use_skill is present in tools"
      (let* ((use-skill-tool (gptel-make-tool :name "use_skill" :description "switch" :function #'ignore))
             (known '((coder :description "Writing and debugging code")
                      (reviewer :description "Reviewing code changes")
                      (standalone)))
             (state (make-macher-agent-transmission-state
                     :tools (list use-skill-tool)
                     :known-presets known
                     :directives nil)))
        (setq state (macher-agent-pipe--inject-skill-directive state))
        (let ((dirs (macher-agent-transmission-state-directives state)))
          (expect (length dirs) :to-equal 1)
          (let ((dir (car dirs)))
            (expect dir :to-match "<available_skills>")
            (expect dir :to-match "</available_skills>")
            (expect dir :to-match "- coder: Writing and debugging code")
            (expect dir :to-match "- reviewer: Reviewing code changes")
            (expect dir :to-match "- standalone")))))

    (it "handles empty known presets when use_skill is present"
      (let* ((use-skill-tool (gptel-make-tool :name "use_skill" :description "switch" :function #'ignore))
             (state (make-macher-agent-transmission-state
                     :tools (list use-skill-tool)
                     :known-presets nil
                     :directives nil)))
        (setq state (macher-agent-pipe--inject-skill-directive state))
        (let ((dirs (macher-agent-transmission-state-directives state)))
          (expect (length dirs) :to-equal 1)
          (expect (car dirs) :to-match "<available_skills>"))))

    (it "registers macher-agent-pipe--inject-skill-directive in transmission install"
      (macher-agent-transmission-install)
      (let ((steps (gethash 'transmission macher-agent-pipeline-registry)))
        (expect (cl-some (lambda (step)
                           (and (eq (plist-get step :fn) #'macher-agent-pipe--inject-skill-directive)
                                (equal (plist-get step :priority) 87)))
                         steps)
                :to-be-truthy)))

    (it "compiles <available_skills> directive into transmission compiled-prompt"
      (macher-agent-transmission-install)
      (with-temp-buffer
        (let* ((buf (current-buffer))
               (use-tool (gptel-make-tool :name "use_skill" :description "switch" :function #'ignore))
               (ctx (make-macher-agent-context :id "skill-pipe-ctx" :project-root "/tmp/skill-test")))
          (setq-local macher-agent--persistent-context ctx)
          (setq-local gptel-model 'mock-model)
          (setq-local gptel-system-prompt "System prompt baseline.")
          (setq-local gptel-tools (list use-tool))
          (setq-local gptel--known-presets '((dev :description "Developer skill")))
          (let ((state (macher-agent--compile-transmission-payload buf nil nil nil ctx)))
            (expect (macher-agent-transmission-state-compiled-prompt state) :to-match "System prompt baseline.")
            (expect (macher-agent-transmission-state-compiled-prompt state) :to-match "<available_skills>")
            (expect (macher-agent-transmission-state-compiled-prompt state) :to-match "- dev: Developer skill")))))

    (it "maps over buffer-local macher-agent-presets when gptel--known-presets is not set"
      (macher-agent-transmission-install)
      (with-temp-buffer
        (let* ((buf (current-buffer))
               (use-tool (gptel-make-tool :name "use_skill" :description "switch" :function #'ignore))
               (ctx (make-macher-agent-context :id "skill-pipe-ctx" :project-root "/tmp/skill-test")))
          (setq-local macher-agent--persistent-context ctx)
          (setq-local gptel-model 'mock-model)
          (setq-local gptel-system-prompt "System prompt baseline.")
          (setq-local gptel-tools (list use-tool))
          (setq-local macher-agent-presets '(coder-skill reviewer-skill))
          (let ((state (macher-agent--compile-transmission-payload buf nil nil nil ctx)))
            (expect (macher-agent-transmission-state-compiled-prompt state) :to-match "<available_skills>")
            (expect (macher-agent-transmission-state-compiled-prompt state) :to-match "- coder-skill")
            (expect (macher-agent-transmission-state-compiled-prompt state) :to-match "- reviewer-skill")))))

    (it "prevents global preset registry leakage in isolated buffer scopes"
      (macher-agent-transmission-install)
      (let ((gptel--known-presets '((leaked-preset :description "Should not leak into isolated buffer"))))
        (with-temp-buffer
          (let* ((buf (current-buffer))
                 (use-tool (gptel-make-tool :name "use_skill" :description "switch" :function #'ignore))
                 (ctx (make-macher-agent-context :id "skill-pipe-ctx" :project-root "/tmp/skill-test")))
            (setq-local macher-agent--persistent-context ctx)
            (setq-local gptel-model 'mock-model)
            (setq-local gptel-system-prompt "System prompt baseline.")
            (setq-local gptel-tools (list use-tool))
            ;; In this isolated buffer, gptel--known-presets and macher-agent-presets are NOT set locally.
            (let ((state (macher-agent--compile-transmission-payload buf nil nil nil ctx)))
              (expect (macher-agent-transmission-state-compiled-prompt state) :to-match "<available_skills>\n</available_skills>")
              (expect (macher-agent-transmission-state-compiled-prompt state) :not :to-match "leaked-preset"))))))

    (it "isolates skills between separate buffers during directive compilation"
      (macher-agent-transmission-install)
      (let ((buf1 (generate-new-buffer "test-pipe-buf-1"))
            (buf2 (generate-new-buffer "test-pipe-buf-2"))
            (use-tool (gptel-make-tool :name "use_skill" :description "switch" :function #'ignore))
            (ctx (make-macher-agent-context :id "skill-pipe-ctx" :project-root "/tmp/skill-test")))
        (unwind-protect
            (progn
              (with-current-buffer buf1
                (setq-local macher-agent--persistent-context ctx)
                (setq-local gptel-model 'mock-model)
                (setq-local gptel-tools (list use-tool))
                (setq-local gptel--known-presets '((buffer-one-skill :description "Scope 1"))))
              (with-current-buffer buf2
                (setq-local macher-agent--persistent-context ctx)
                (setq-local gptel-model 'mock-model)
                (setq-local gptel-tools (list use-tool))
                (setq-local gptel--known-presets '((buffer-two-skill :description "Scope 2"))))
              (let ((state1 (macher-agent--compile-transmission-payload buf1 nil nil nil ctx))
                    (state2 (macher-agent--compile-transmission-payload buf2 nil nil nil ctx)))
                (expect (macher-agent-transmission-state-compiled-prompt state1) :to-match "- buffer-one-skill: Scope 1")
                (expect (macher-agent-transmission-state-compiled-prompt state1) :not :to-match "buffer-two-skill")
                (expect (macher-agent-transmission-state-compiled-prompt state2) :to-match "- buffer-two-skill: Scope 2")
                (expect (macher-agent-transmission-state-compiled-prompt state2) :not :to-match "buffer-one-skill")))
          (kill-buffer buf1)
          (kill-buffer buf2)))))

  (describe "3. use_skill Tool Registration and Functionality"
    (it "registers use_skill tool with name, category meta, and expected args"
      (expect (boundp 'macher-agent-use-skill-tool) :to-be t)
      (let ((tool macher-agent-use-skill-tool))
        (expect (gptel-tool-name tool) :to-equal "use_skill")
        (expect (gptel-tool-category tool) :to-equal "meta")
        (let ((args (gptel-tool-args tool)))
          (expect (plist-get (cl-find "skill_name" args :key (lambda (a) (plist-get a :name)) :test #'equal) :type)
                  :to-equal "string")
          (expect (plist-get (cl-find "done" args :key (lambda (a) (plist-get a :name)) :test #'equal) :type)
                  :to-equal "boolean"))))

    (it "activates skill, caches current presets, and updates macher-agent-presets"
      (with-temp-buffer
        (setq-local macher-agent-presets '(original-skill))
        (let ((ptc-fn (get 'macher-agent-use-skill-tool 'ptc-function)))
          (funcall ptc-fn "coder")
          ;; Cached presets should contain the original
          (expect (macher-agent-get-cached-presets) :to-equal '(original-skill))
          ;; Presets updated to coder
          (expect macher-agent-presets :to-equal '(coder)))))

    (it "does not overwrite cached presets on multiple consecutive skill switches"
      (with-temp-buffer
        (setq-local macher-agent-presets '(base-preset))
        (let ((ptc-fn (get 'macher-agent-use-skill-tool 'ptc-function)))
          (funcall ptc-fn "skill-1")
          (expect (macher-agent-get-cached-presets) :to-equal '(base-preset))
          (expect macher-agent-presets :to-equal '(skill-1))
          ;; Switch again before finishing
          (funcall ptc-fn "skill-2")
          ;; Cache must remain unchanged (base-preset)
          (expect (macher-agent-get-cached-presets) :to-equal '(base-preset))
          (expect macher-agent-presets :to-equal '(skill-2)))))

    (it "restores original presets and clears cache when done is non-nil"
      (with-temp-buffer
        (setq-local macher-agent-presets '(base-preset))
        (let ((ptc-fn (get 'macher-agent-use-skill-tool 'ptc-function)))
          (funcall ptc-fn "temp-skill")
          (expect macher-agent-presets :to-equal '(temp-skill))
          (expect (macher-agent-get-cached-presets) :to-equal '(base-preset))
          ;; Restore with done = t
          (funcall ptc-fn nil t)
          (expect macher-agent-presets :to-equal '(base-preset))
          (expect (local-variable-p 'macher-agent--cached-presets) :to-be nil))))

    (it "restores original presets when skill_name is 'done'"
      (with-temp-buffer
        (setq-local macher-agent-presets '(base-preset))
        (let ((ptc-fn (get 'macher-agent-use-skill-tool 'ptc-function)))
          (funcall ptc-fn "temp-skill")
          (expect macher-agent-presets :to-equal '(temp-skill))
          ;; Restore with skill_name = "done"
          (funcall ptc-fn "done")
          (expect macher-agent-presets :to-equal '(base-preset))
          (expect (local-variable-p 'macher-agent--cached-presets) :to-be nil))))

    (it "correctly caches and restores when initial presets were nil"
      (with-temp-buffer
        (setq-local macher-agent-presets nil)
        (let ((ptc-fn (get 'macher-agent-use-skill-tool 'ptc-function)))
          (funcall ptc-fn "first-skill")
          (expect macher-agent-presets :to-equal '(first-skill))
          (expect (local-variable-p 'macher-agent--cached-presets) :to-be t)
          (expect (macher-agent-get-cached-presets) :to-be nil)
          ;; Restore
          (funcall ptc-fn "done")
          (expect macher-agent-presets :to-be nil)
          (expect (local-variable-p 'macher-agent--cached-presets) :to-be nil))))

    (it "preserves main buffer state non-destructively without mutating gptel-system-prompt or gptel-tools"
      (with-temp-buffer
        (let ((orig-prompt "Original untouched prompt")
              (orig-tools (list (gptel-make-tool :name "original_tool" :description "orig" :function #'ignore))))
          (setq-local gptel-system-prompt orig-prompt)
          (setq-local gptel-tools orig-tools)
          (setq-local macher-agent-presets '(initial-preset))
          (let ((ptc-fn (get 'macher-agent-use-skill-tool 'ptc-function)))
            (funcall ptc-fn "specialist-skill")
            ;; Main buffer prompt and tools MUST NOT be overwritten
            (expect gptel-system-prompt :to-equal orig-prompt)
            (expect gptel-tools :to-equal orig-tools)
            ;; Only macher-agent-presets changed
            (expect macher-agent-presets :to-equal '(specialist-skill))))))))

(provide 'macher-agent-use-skill-test)
;;; macher-agent-use-skill-test.el ends here
