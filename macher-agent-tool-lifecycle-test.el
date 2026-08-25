;;; macher-agent-tool-lifecycle-test.el --- Tool Validation & Lifecycle Hooks Tests -*- lexical-binding: t; -*-

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

          (it "generates variadic signatures for async and sync tools to safely absorb FSM contexts"
              (let* ((async-fn (gptel-tool-function mock-async-contract-tool))
                     (sync-fn (gptel-tool-function mock-sync-contract-tool)))
                (expect (func-arity async-fn) :to-equal '(1 . many))
                (expect (func-arity sync-fn) :to-equal '(1 . many))))

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

          (it "extracts properties comprehensively across plists, alists, and hash-tables with varied key formats"
              (let ((plist '(:foo-bar "val1" :baz 123 :user_name "alice"))
                    (alist-hyphen '((:foo-bar . "val1") (:baz . 456)))
                    (alist-str '(("foo_bar" . 1) ("baz" . 456)))
                    (ht (make-hash-table :test 'equal)))
                (puthash :foo-bar "val1" ht)
                (puthash "foo_bar" 2 ht)
                (puthash :baz 789 ht)
                ;; Plists
                (expect (macher-agent--extract-prop plist :foo-bar) :to-equal "val1")
                (expect (macher-agent--extract-prop plist "foo_bar") :to-equal "val1")
                (expect (macher-agent--extract-prop plist :foo_bar) :to-equal "val1")
                (expect (macher-agent--extract-prop plist "user_name") :to-equal "alice")
                (expect (macher-agent--extract-prop plist :missing) :to-equal 'macher-missing)
                ;; Alists
                (expect (macher-agent--extract-prop alist-hyphen :foo-bar) :to-equal "val1")
                (expect (macher-agent--extract-prop alist-hyphen "foo_bar") :to-equal "val1")
                (expect (macher-agent--extract-prop alist-hyphen :baz) :to-equal 456)
                (expect (macher-agent--extract-prop alist-str "foo_bar") :to-equal 1)
                (expect (macher-agent--extract-prop alist-str :foo_bar) :to-equal 1)
                ;; Hash tables
                (expect (macher-agent--extract-prop ht :foo-bar) :to-equal "val1")
                (expect (macher-agent--extract-prop ht "foo_bar") :to-equal 2)
                (expect (macher-agent--extract-prop ht :baz) :to-equal 789)
                (expect (macher-agent--extract-prop ht :missing) :to-equal 'macher-missing)))

          (it "matches schema parameters with underscores against parsed objects with underscored keywords"
              (macher-agent-make-tool mock-underscored-schema-tool
                                      "Mock underscored tool"
                                      :category "test"
                                      :args (list (list :name "user_profile"
                                                        :type 'object
                                                        :properties '(:foo_bar (:type integer)
                                                                      :user_name (:type string))
                                                        :required ["foo_bar" "user_name"]))
                                      :command-fn (lambda (payload _context _root)
                                                    (let ((profile (or (plist-get payload :user_profile)
                                                                       (plist-get payload :user-profile))))
                                                      (format "User: %s (foo_bar: %s)"
                                                              (macher-agent--extract-prop profile "user_name")
                                                              (macher-agent--extract-prop profile "foo_bar")))))
              (let ((callback-result nil))
                (funcall (gptel-tool-function mock-underscored-schema-tool)
                         (lambda (res) (setq callback-result res))
                         '(:foo_bar 1 :user_name "alice"))
                (expect callback-result :to-equal "User: alice (foo_bar: 1)"))
              (let ((callback-result nil))
                (funcall (gptel-tool-function mock-underscored-schema-tool)
                         (lambda (res) (setq callback-result res))
                         :user_profile '(:foo_bar 2 :user_name "bob"))
                (expect callback-result :to-equal "User: bob (foo_bar: 2)"))
              (let ((callback-result nil))
                (funcall (gptel-tool-function mock-underscored-schema-tool)
                         (lambda (res) (setq callback-result res))
                         :user-profile '(:foo_bar 3 :user_name "charlie"))
                (expect callback-result :to-equal "User: charlie (foo_bar: 3)")))

          (it "supports optional :include keyword in macher-agent-make-tool and respects omission"
              ;; When omitted, :include is not passed to gptel-make-tool and defaults to gptel's default (t)
              (macher-agent-make-tool mock-no-include-tool
                  "Tool without include"
                :category "test"
                :args '((:name "arg1" :type string))
                :command-fn (lambda (payload _ctx _root) (plist-get payload :arg1)))
              (expect (gptel-tool-include mock-no-include-tool) :to-be t)

              ;; When :include nil is supplied, it is passed to gptel-make-tool
              (macher-agent-make-tool mock-nil-include-tool
                  "Tool with nil include"
                :category "test"
                :include nil
                :args '((:name "arg1" :type string))
                :command-fn (lambda (payload _ctx _root) (plist-get payload :arg1)))
              (expect (gptel-tool-include mock-nil-include-tool) :to-be nil)

              ;; When :include 'call is supplied, it is passed to gptel-make-tool
              (macher-agent-make-tool mock-call-include-tool
                  "Tool with call include"
                :category "test"
                :include 'call
                :args '((:name "arg1" :type string))
                :command-fn (lambda (payload _ctx _root) (plist-get payload :arg1)))
              (expect (gptel-tool-include mock-call-include-tool) :to-equal 'call)

              ;; When :include t is explicitly supplied
              (macher-agent-make-tool mock-explicit-t-include-tool
                  "Tool with explicit t include"
                :category "test"
                :include t
                :args '((:name "arg1" :type string))
                :command-fn (lambda (payload _ctx _root) (plist-get payload :arg1)))
              (expect (gptel-tool-include mock-explicit-t-include-tool) :to-be t))

          (it "fully replaces search_in_workspace in macher-agent--wrap-single-tool with the script tool definition"
              (let* ((legacy-search-tool (gptel-make-tool
                                          :name "search_in_workspace"
                                          :category "legacy-cat"
                                          :description "Legacy search description"
                                          :args '((:name "old_arg" :type string))
                                          :function (lambda (&rest _) "legacy-result")))
                     (macher-agent--wrapped-tools-hash (make-hash-table :test 'eq)))
                (macher-agent--wrap-single-tool legacy-search-tool)
                (expect (gptel-tool-name legacy-search-tool) :to-equal "search_in_workspace")
                (expect (gptel-tool-category legacy-search-tool) :to-equal "perception")
                (expect (gptel-tool-description legacy-search-tool)
                        :to-equal "Search for a regular expression pattern within the strictly bounded workspace.")
                (expect (gptel-tool-args legacy-search-tool)
                        :to-equal '((:name "pattern" :type "string" :description "The regex pattern to search for"))))))

(provide 'macher-agent-tool-lifecycle-test)
;;; macher-agent-tool-lifecycle-test.el ends here
