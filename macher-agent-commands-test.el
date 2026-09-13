;;; macher-agent-commands-test.el --- Tests for flat markdown command support -*- lexical-binding: t; -*-

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
(require 'macher-agent-core)
(require 'macher-agent-gptel)
(require 'macher-agent-presets)

(describe "Command argument interpolation"
          (it "substitutes bulk arguments placeholders correctly"
              (let ((template "Action on: $arguments (alias $args)")
                    (args "src/main.rs and test.rs"))
                (expect (macher-agent--interpolate-command-body template args)
                        :to-equal "Action on: src/main.rs and test.rs (alias src/main.rs and test.rs)")))

          (it "substitutes positional variables respecting quotes"
              (let ((template "Review $1 with focus on $2 and details $arguments")
                    (args "\"first file.txt\" \"performance issue\" extra notes"))
                (expect (macher-agent--interpolate-command-body template args)
                        :to-equal "Review first file.txt with focus on performance issue and details \"first file.txt\" \"performance issue\" extra notes")))

          (it "handles empty or omitted arguments cleanly"
              (let ((template "Run task with args: [$arguments] and [$1]"))
                (expect (macher-agent--interpolate-command-body template "")
                        :to-equal "Run task with args: [] and [$1]")
                (expect (macher-agent--interpolate-command-body template nil)
                        :to-equal "Run task with args: [] and [$1]"))))

(describe "Command flag injection"
          (macher-agent-test-setup-before-each)

          (it "injects is-command flag when registered from commands directory"
              (let* ((ctx (make-macher-agent-context :id "cmd-test-ctx" :project-root "/tmp/mock-proj"))
                     (parsed (list :name "commit" :name-sym 'commit :body "Commit message: $1"))
                     (cmd-path "/tmp/mock-proj/commands/commit.md"))
                (macher-agent--register-parsed-skill parsed cmd-path ctx)
                (if-let* ((skills (macher-agent-context-skills ctx))
                          (entry (alist-get 'commit skills)))
                    (progn
                      (expect (plist-get entry :is-command) :to-equal t)
                      (expect (plist-get entry :system) :to-equal "Commit message: $1"))
                  (expect nil :to-be t))))

          (it "does not inject is-command flag for standard skills"
              (let* ((ctx (make-macher-agent-context :id "skill-test-ctx" :project-root "/tmp/mock-proj"))
                     (parsed (list :name "coder" :name-sym 'coder :body "Coder system persona"))
                     (skill-path "/tmp/mock-proj/skills/coder/SKILL.md"))
                (macher-agent--register-parsed-skill parsed skill-path ctx)
                (if-let* ((skills (macher-agent-context-skills ctx))
                          (entry (alist-get 'coder skills)))
                    (progn
                      (expect (plist-get entry :is-command) :to-be nil)
                      (expect (plist-get entry :system) :to-equal "Coder system persona"))
                  (expect nil :to-be t)))))

(describe "Transmission pipeline command redirection"
          (macher-agent-test-setup-before-each)

          (it "interpolates command body into redirect-prompt and preserves system persona"
              (macher-agent-transmission-install)
              (with-temp-buffer
                (let* ((buf (current-buffer))
                       (ctx (make-macher-agent-context :id "pipe-ctx"
                                                       :project-root "/tmp/mock-proj"
                                                       :prompt "@commit \"feat: initial release\""))
                       (cmd-preset (list :name "commit"
                                         :body "Git commit changes with message: $1"
                                         :is-command t)))
                  (setq-local macher-agent--persistent-context ctx)
                  (setq-local gptel-system-prompt "You are a helpful programming assistant.")
                  (setq-local gptel--known-presets (list (cons 'commit cmd-preset)))
                  (let ((state (macher-agent--compile-transmission-payload buf nil '(commit) 'commit ctx)))
                    (expect (macher-agent-transmission-state-redirect-prompt state)
                            :to-equal "Git commit changes with message: feat: initial release")
                    (expect (macher-agent-transmission-state-compiled-prompt state)
                            :to-equal "You are a helpful programming assistant.")))))

          (it "retains standard skill redirect behaviour when not a command"
              (macher-agent-transmission-install)
              (with-temp-buffer
                (let* ((buf (current-buffer))
                       (ctx (make-macher-agent-context :id "pipe-ctx"
                                                       :project-root "/tmp/mock-proj"
                                                       :prompt "@review"))
                       (skill-preset (list :name "review"
                                           :body "Review the pull request carefully."
                                           :system "Review the pull request carefully.")))
                  (setq-local macher-agent--persistent-context ctx)
                  (setq-local gptel-system-prompt nil)
                  (setq-local gptel--known-presets (list (cons 'review skill-preset)))
                  (let ((state (macher-agent--compile-transmission-payload buf nil '(review) skill-preset ctx)))
                    (expect (macher-agent-transmission-state-compiled-prompt state)
                            :to-equal "### Skill: review\nReview the pull request carefully.\n"))))))
