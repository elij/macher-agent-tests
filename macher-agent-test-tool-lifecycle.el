;;; macher-agent-test-tool-lifecycle.el --- Tool Validation & Lifecycle Hooks Tests -*- lexical-binding: t; -*-

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

(describe "Tool Schema Validation and Lifecycle Hooks"
          (macher-agent-test-setup-before-each)

          (before-all
           (macher-agent-make-tool mock-async-contract-tool
                                   "Mock async tool"
                                   :category "test"
                                   :args (list (list :name "arg1" :type 'string) (list :name "arg2" :type 'string))
                                   :command-fn (lambda (payload _context _root)
                                                 (format "Async %s %s" (plist-get payload :arg1) (plist-get payload :arg2))))

           (macher-agent-make-tool mock-sync-contract-tool
                                   "Mock sync tool"
                                   :category "test"
                                   :args (list (list :name "arg1" :type 'string))
                                   :command-fn (lambda (payload _context _root)
                                                 (format "Sync %s" (plist-get payload :arg1))))

           (macher-agent-make-tool mock-complex-schema-tool
                                   "Mock complex schema tool"
                                   :category "test"
                                   :args (list (list :name "tasks"
                                                     :type 'array
                                                     :items '(:type object
                                                                    :properties (:name (:type string)
                                                                                       :presets (:type array :items (:type string))))))
                                   :command-fn (lambda (_payload _context _root)
                                                 "Valid schema")))

          (before-each
           (setq macher-agent-pre-tool-use-hook nil)
           (setq macher-agent-permission-request-hook nil)
           (setq macher-agent-post-tool-use-hook nil)
           (setq macher-agent-post-tool-use-failure-hook nil))

          (after-all
           (setq macher-agent-pre-tool-use-hook nil)
           (setq macher-agent-permission-request-hook nil)
           (setq macher-agent-post-tool-use-hook nil)
           (setq macher-agent-post-tool-use-failure-hook nil))

          (it "validates schema parameters correctly for valid payloads"
              (let ((callback-result nil))
                (funcall (gptel-tool-function mock-complex-schema-tool)
                         (lambda (res) (setq callback-result res))
                         '[(:name "task1" :presets ["preset1" "preset2"])])
                (expect callback-result :to-match "Valid schema")))

          (it "returns a descriptive error for malformed schema parameters"
              (let ((callback-result nil))
                (funcall (gptel-tool-function mock-complex-schema-tool)
                         (lambda (res) (setq callback-result res))
                         '[(:name "task1" :presets "worker")])
                (expect callback-result :to-match "The 'tasks\\[0\\]\\.presets' parameter must be an array, not string")))

          (it "generates variadic signatures for async tools to safely absorb FSM contexts"
              (let* ((tool-fn (gptel-tool-function mock-async-contract-tool))
                     (arity (func-arity tool-fn)))
                (expect (car arity) :to-equal 1)
                (expect (cdr arity) :to-equal 'many)))

          (it "generates variadic signatures for sync tools to safely absorb FSM contexts"
              (let* ((tool-fn (gptel-tool-function mock-sync-contract-tool))
                     (arity (func-arity tool-fn)))
                (expect (car arity) :to-equal 1)
                (expect (cdr arity) :to-equal 'many)))

          (it "runs pre-tool-use-hook and aborts if it returns nil"
              (let ((pre-called nil)
                    (callback-result nil))
                (add-hook 'macher-agent-pre-tool-use-hook
                          (lambda (name args)
                            (setq pre-called (list name args))
                            nil))
                (funcall (gptel-tool-function mock-sync-contract-tool)
                         (lambda (res) (setq callback-result res))
                         "hello")
                (expect pre-called :to-equal (list 'mock-sync-contract-tool '(:arg1 "hello")))
                (expect callback-result :to-match "Execution blocked by macher-agent-pre-tool-use-hook")))

          (it "runs pre-tool-use-hook and aborts if it signals an error"
              (let ((callback-result nil))
                (add-hook 'macher-agent-pre-tool-use-hook
                          (lambda (_name _args)
                            (error "Custom pre error")))
                (funcall (gptel-tool-function mock-sync-contract-tool)
                         (lambda (res) (setq callback-result res))
                         "hello")
                (expect callback-result :to-match "Execution blocked by error in macher-agent-pre-tool-use-hook: Custom pre error")))

          (it "runs permission-request-hook and succeeds by default if empty"
              (let ((callback-result nil))
                (funcall (gptel-tool-function mock-sync-contract-tool)
                         (lambda (res) (setq callback-result res))
                         "hello")
                (expect callback-result :to-match "Sync hello")))

          (it "runs permission-request-hook and aborts if it returns nil"
              (let ((perm-called nil)
                    (callback-result nil))
                (add-hook 'macher-agent-permission-request-hook
                          (lambda (name args)
                            (setq perm-called (list name args))
                            nil))
                (funcall (gptel-tool-function mock-sync-contract-tool)
                         (lambda (res) (setq callback-result res))
                         "hello")
                (expect perm-called :to-equal (list 'mock-sync-contract-tool '(:arg1 "hello")))
                (expect callback-result :to-match "Permission denied by macher-agent-permission-request-hook")))

          (it "runs post-tool-use-hook upon successful execution"
              (let ((post-called nil)
                    (callback-result nil))
                (add-hook 'macher-agent-post-tool-use-hook
                          (lambda (name args result)
                            (setq post-called (list name args result))))
                (funcall (gptel-tool-function mock-sync-contract-tool)
                         (lambda (res) (setq callback-result res))
                         "world")
                (expect callback-result :to-match "Sync world")
                (expect post-called :to-equal (list 'mock-sync-contract-tool '(:arg1 "world") "Sync world"))))

          (it "runs post-tool-use-failure-hook if the tool body throws an error"
              (macher-agent-make-tool mock-error-tool
                                      "Mock error tool"
                                      :category "test"
                                      :args (list (list :name "arg1" :type 'string))
                                      :command-fn (lambda (_payload _context _root)
                                                    (error "Failing intentionally")))
              (let ((failure-called nil)
                    (callback-result nil))
                (add-hook 'macher-agent-post-tool-use-failure-hook
                          (lambda (name args err)
                            (setq failure-called (list name args err))))
                (funcall (gptel-tool-function mock-error-tool)
                         (lambda (res) (setq callback-result res))
                         "fail")
                (expect callback-result :to-match "Failing intentionally")
                (expect (car failure-called) :to-be 'mock-error-tool)
                (expect (cadr failure-called) :to-equal '(:arg1 "fail"))
                (expect (error-message-string (caddr failure-called)) :to-equal "Failing intentionally")))

          (it "extracts properties from plists with keyword and string keys"
              (let ((plist '(:foo-bar "val1" :baz 123)))
                (expect (macher-agent--extract-prop plist :foo-bar) :to-equal "val1")
                (expect (macher-agent--extract-prop plist "foo_bar") :to-equal "val1")
                (expect (macher-agent--extract-prop plist :baz) :to-equal 123)
                (expect (macher-agent--extract-prop plist :missing) :to-equal 'macher-missing)))

          (it "extracts properties from alists with keyword and string keys"
              (let ((alist '((:foo-bar . "val1") (:baz . 456))))
                (expect (macher-agent--extract-prop alist :foo-bar) :to-equal "val1")
                (expect (macher-agent--extract-prop alist "foo_bar") :to-equal "val1")
                (expect (macher-agent--extract-prop alist :baz) :to-equal 456)
                (expect (macher-agent--extract-prop alist :missing) :to-equal 'macher-missing)))

          (it "extracts properties from hash-tables with keyword and string keys"
              (let ((ht (make-hash-table :test 'equal)))
                (puthash :foo-bar "val1" ht)
                (puthash :baz 789 ht)
                (expect (macher-agent--extract-prop ht :foo-bar) :to-equal "val1")
                (expect (macher-agent--extract-prop ht "foo_bar") :to-equal "val1")
                (expect (macher-agent--extract-prop ht :baz) :to-equal 789)
                (expect (macher-agent--extract-prop ht :missing) :to-equal 'macher-missing))))

(provide 'macher-agent-test-tool-lifecycle)
;;; macher-agent-test-tool-lifecycle.el ends here
