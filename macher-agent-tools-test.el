;;; macher-agent-tools-test.el --- Tests for Macher Agent Tools -*- lexical-binding: t; -*-

;;; Commentary:

;; Consolidated unit and integration tests for macher-agent-tools.el,
;; covering tool definition, schema validation, payload normalization,
;; lifecycle hooks, instruction queues, and search utilities.

;;; Code:

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

(require 'buttercup)
(require 'macher-agent-test-setup)
(require 'macher-agent-tools)

(describe "Macher-Agent Tools Suite"
  (macher-agent-test-setup-before-each)

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

  ;; --------------------------------------------------------------------------
  ;; 1. Schema Generation and Type Validation
  ;; --------------------------------------------------------------------------
  (describe "1. Schema Generation and Type Validation"
    (it "validates primitive schema types correctly"
      ;; String
      (expect (macher-agent--validate-primitive-schema 'string "hello" "param") :to-be t)
      (expect (macher-agent--validate-primitive-schema "string" "hello" "param") :to-be t)
      (expect (macher-agent--validate-primitive-schema 'string 123 "param")
              :to-throw 'error)

      ;; Number & Integer
      (expect (macher-agent--validate-primitive-schema 'number 42 "param") :to-be t)
      (expect (macher-agent--validate-primitive-schema 'number 3.14 "param") :to-be t)
      (expect (macher-agent--validate-primitive-schema 'integer 42 "param") :to-be t)
      (expect (macher-agent--validate-primitive-schema 'integer 3.14 "param")
              :to-throw 'error)

      ;; Boolean
      (expect (macher-agent--validate-primitive-schema 'boolean t "param") :to-be t)
      (expect (macher-agent--validate-primitive-schema 'boolean :json-false "param") :to-be t)
      (expect (macher-agent--validate-primitive-schema 'boolean "true" "param")
              :to-throw 'error)

      ;; Array
      (expect (macher-agent--validate-primitive-schema 'array [1 2 3] "param") :to-be t)
      (expect (macher-agent--validate-primitive-schema 'array '(1 2 3) "param")
              :to-throw 'error)

      ;; Object
      (expect (macher-agent--validate-primitive-schema 'object '(:a 1) "param") :to-be t)
      (expect (macher-agent--validate-primitive-schema 'object (make-hash-table) "param") :to-be t)
      (expect (macher-agent--validate-primitive-schema 'object "not-obj" "param")
              :to-throw 'error)

      ;; Unknown type
      (expect (macher-agent--validate-primitive-schema 'unknown-type "val" "param")
              :to-throw 'error))

    (it "validates array schemas and item specifications recursively"
      (let ((spec '(:type array :items (:type string))))
        (expect (macher-agent--validate-array-schema spec ["a" "b" "c"] "items") :to-be nil)
        (expect (macher-agent--validate-array-schema spec ["a" 123 "c"] "items")
                :to-throw 'error)))

    (it "validates object schemas including required and optional properties"
      (let ((spec '(:type object
                    :properties (:name (:type string)
                                 :age (:type integer :optional t)
                                 :role (:type string))
                    :required ["name" "role"])))
        ;; Valid object plist
        (expect (macher-agent--validate-object-schema spec '(:name "Alice" :role "Admin" :age 30) "user")
                :to-be nil)
        (expect (macher-agent--validate-object-schema spec '(:name "Bob" :role "Member") "user")
                :to-be nil)

        ;; Missing required property
        (expect (macher-agent--validate-object-schema spec '(:name "Charlie") "user")
                :to-throw 'error)

        ;; Invalid property type
        (expect (macher-agent--validate-object-schema spec '(:name "Dave" :role 99) "user")
                :to-throw 'error)))

    (it "handles null values and optionality in general schema validation"
      (let ((opt-spec '(:name "opt_arg" :type string :optional t))
            (req-spec '(:name "req_arg" :type string))
            (null-spec '(:name "null_arg" :type null)))
        ;; Nil values
        (expect (macher-agent--validate-schema opt-spec nil) :to-be nil)
        (expect (macher-agent--validate-schema req-spec nil) :to-throw 'error)

        ;; :null values
        (expect (macher-agent--validate-schema null-spec :null) :to-be nil)
        (expect (macher-agent--validate-schema req-spec :null) :to-throw 'error)))

    (it "validates full payload against schema specifications"
      (let ((args-spec (list (list :name "file_path" :type 'string)
                             (list :name "line_count" :type 'integer :optional t))))
        (expect (macher-agent--validate-payload '(:file_path "/tmp/foo" :line_count 10) args-spec)
                :to-be nil)
        (expect (macher-agent--validate-payload '(:file-path "/tmp/foo") args-spec)
                :to-be nil)
        (expect (macher-agent--validate-payload '(:file_path 12345) args-spec)
                :to-throw 'error))))

  ;; --------------------------------------------------------------------------
  ;; 2. Argument Parsing and Payload Normalisation
  ;; --------------------------------------------------------------------------
  (describe "2. Argument Parsing and Payload Normalisation"
    (it "normalizes parameter names matching hyphens, underscores, and keywords"
      (expect (macher-agent--param-name-matches-p "foo_bar" "foo_bar") :to-be t)
      (expect (macher-agent--param-name-matches-p "foo-bar" "foo_bar") :to-be t)
      (expect (macher-agent--param-name-matches-p :foo_bar "foo-bar") :to-be t)
      (expect (macher-agent--param-name-matches-p :foo-bar :foo_bar) :to-be t)
      (expect (macher-agent--param-name-matches-p "different" "foo_bar") :to-be nil))

    (it "detects if candidate key matches any parameter in args spec"
      (let ((spec '((:name "file_path" :type string)
                    (:name "max_lines" :type integer))))
        (expect (macher-agent--spec-has-param-p spec :file_path) :to-be-truthy)
        (expect (macher-agent--spec-has-param-p spec :file-path) :to-be-truthy)
        (expect (macher-agent--spec-has-param-p spec "max-lines") :to-be-truthy)
        (expect (macher-agent--spec-has-param-p spec :unknown_param) :to-be nil)))

    (it "extracts payload from positional arguments"
      (let* ((spec '((:name "arg1" :type string)
                     (:name "arg2" :type integer)))
             (payload (macher-agent--extract-payload '("value1" 42) spec)))
        (expect (plist-get payload :arg1) :to-equal "value1")
        (expect (plist-get payload :arg2) :to-equal 42)))

    (it "extracts payload from direct keyword plists and normalizes separators"
      (let* ((spec '((:name "source_file" :type string)
                     (:name "target_dir" :type string)))
             (payload (macher-agent--extract-payload
                       '(:source_file "a.txt" :target-dir "/tmp") spec)))
        ;; Both underscored and hyphenated keys are available
        (expect (plist-get payload :source_file) :to-equal "a.txt")
        (expect (plist-get payload :source-file) :to-equal "a.txt")
        (expect (plist-get payload :target_dir) :to-equal "/tmp")
        (expect (plist-get payload :target-dir) :to-equal "/tmp")))

    (it "extracts payload from single nested plist parameter"
      (let* ((spec '((:name "param_a" :type string)))
             (payload (macher-agent--extract-payload
                       '((:param_a "nested_val")) spec)))
        (expect (plist-get payload :param_a) :to-equal "nested_val")
        (expect (plist-get payload :param-a) :to-equal "nested_val"))))

  ;; --------------------------------------------------------------------------
  ;; 3. Tool Construction, Callbacks, and Execution
  ;; --------------------------------------------------------------------------
  (describe "3. Tool Construction, Callbacks, and Execution"
    (it "wraps callbacks converting success and error plists correctly"
      (let ((success-res nil)
            (error-res nil))
        ;; Success case
        (let ((cb (macher-agent--wrap-callback (lambda (res) (setq success-res res)))))
          (funcall cb '(:status success :data "Processed successfully")))
        (expect success-res :to-equal "Processed successfully")

        ;; Error case
        (let ((cb (macher-agent--wrap-callback (lambda (res) (setq error-res res)))))
          (funcall cb '(:status error :error "Operation failed")))
        (expect error-res :to-equal "Operation failed")

        ;; Nil callback returns raw final string
        (let ((cb (macher-agent--wrap-callback nil)))
          (expect (funcall cb '(:status success :data "Direct return")) :to-equal "Direct return"))))

    (it "defines and executes synchronous tools via macher-agent-make-tool"
      (macher-agent-make-tool mock-tools-sync-tool
          "Mock sync execution tool"
        :category "test-tools"
        :args '((:name "prefix" :type string)
                (:name "content" :type string))
        :command-fn (lambda (payload _ctx _root)
                      (format "[%s]: %s"
                              (plist-get payload :prefix)
                              (plist-get payload :content))))

      (expect (gptel-tool-name mock-tools-sync-tool) :to-equal "mock_tools_sync")
      (expect (gptel-tool-category mock-tools-sync-tool) :to-equal "test-tools")
      (expect (gptel-tool-description mock-tools-sync-tool) :to-equal "Mock sync execution tool")

      (let ((result nil))
        (funcall (gptel-tool-function mock-tools-sync-tool)
                 (lambda (res) (setq result res))
                 :prefix "INFO" :content "all systems go")
        (expect result :to-equal "[INFO]: all systems go")))

    (it "defines and executes 4-arity asynchronous tools with callback"
      (macher-agent-make-tool mock-tools-async-tool
          "Mock async execution tool"
        :category "test-tools"
        :args '((:name "input" :type string))
        :command-fn (lambda (payload _ctx _root on-success)
                      (funcall on-success (format "Async: %s" (plist-get payload :input)))))

      (let ((result nil))
        (funcall (gptel-tool-function mock-tools-async-tool)
                 (lambda (res) (setq result res))
                 "test-data")
        (expect result :to-equal "Async: test-data")))

    (it "applies success-fn and output-filter-fn formatting on results"
      (macher-agent-make-tool mock-tools-formatter-tool
          "Mock formatting tool"
        :category "test-tools"
        :args '((:name "val" :type string))
        :command-fn (lambda (payload _ctx _root)
                      (plist-get payload :val))
        :success-fn (lambda (raw-res payload)
                      (format "%s (input was %s)" raw-res (plist-get payload :val)))
        :output-filter-fn (lambda (formatted)
                            (upcase formatted)))

      (let ((result nil))
        (funcall (gptel-tool-function mock-tools-formatter-tool)
                 (lambda (res) (setq result res))
                 :val "alpha")
        (expect result :to-equal "ALPHA (INPUT WAS ALPHA)"))

      ;; When active PTC execution is set, success-fn formatting is bypassed
      (let ((macher-agent--active-ptc-execution t)
            (ptc-result nil))
        (funcall (gptel-tool-function mock-tools-formatter-tool)
                 (lambda (res) (setq ptc-result res))
                 :val "beta")
        ;; output-filter-fn still runs on raw output, but success-fn is skipped
        (expect ptc-result :to-equal "BETA")))

    (it "respects explicit :include options during tool creation"
      (macher-agent-make-tool mock-tools-inc-default "Default include"
        :args '((:name "x" :type string))
        :command-fn #'ignore)
      (expect (gptel-tool-include mock-tools-inc-default) :to-be t)

      (macher-agent-make-tool mock-tools-inc-nil "Nil include"
        :include nil
        :args '((:name "x" :type string))
        :command-fn #'ignore)
      (expect (gptel-tool-include mock-tools-inc-nil) :to-be nil)

      (macher-agent-make-tool mock-tools-inc-call "Call include"
        :include 'call
        :args '((:name "x" :type string))
        :command-fn #'ignore)
      (expect (gptel-tool-include mock-tools-inc-call) :to-equal 'call)))

  ;; --------------------------------------------------------------------------
  ;; 4. Lifecycle Hooks & Error Trapping
  ;; --------------------------------------------------------------------------
  (describe "4. Lifecycle Hooks & Error Trapping"
    (it "aborts and returns error message if pre-tool-use-hook returns nil"
      (macher-agent-make-tool mock-hook-tool "Hook target"
        :args '((:name "msg" :type string))
        :command-fn (lambda (payload _c _r) (plist-get payload :msg)))

      (let ((pre-called nil)
            (result nil))
        (add-hook 'macher-agent-pre-tool-use-hook
                  (lambda (sym payload)
                    (setq pre-called (list sym payload))
                    nil))
        (funcall (gptel-tool-function mock-hook-tool)
                 (lambda (res) (setq result res))
                 "hello")
        (expect (car pre-called) :to-be 'mock-hook-tool)
        (expect result :to-match "Execution blocked by macher-agent-pre-tool-use-hook")))

    (it "aborts if pre-tool-use-hook signals an error"
      (macher-agent-make-tool mock-hook-tool-2 "Hook target 2"
        :args '((:name "msg" :type string))
        :command-fn (lambda (payload _c _r) (plist-get payload :msg)))

      (let ((result nil))
        (add-hook 'macher-agent-pre-tool-use-hook
                  (lambda (_sym _payload)
                    (error "Pre-hook failure")))
        (funcall (gptel-tool-function mock-hook-tool-2)
                 (lambda (res) (setq result res))
                 "hello")
        (expect result :to-match "Execution blocked by error in macher-agent-pre-tool-use-hook: Pre-hook failure")))

    (it "aborts if permission-request-hook returns nil"
      (macher-agent-make-tool mock-perm-tool "Perm target"
        :args '((:name "msg" :type string))
        :command-fn (lambda (payload _c _r) (plist-get payload :msg)))

      (let ((result nil))
        (add-hook 'macher-agent-permission-request-hook
                  (lambda (_sym _payload) nil))
        (funcall (gptel-tool-function mock-perm-tool)
                 (lambda (res) (setq result res))
                 "hello")
        (expect result :to-match "Permission denied by macher-agent-permission-request-hook")))

    (it "executes post-tool-use-hook on success"
      (macher-agent-make-tool mock-post-tool "Post target"
        :args '((:name "msg" :type string))
        :command-fn (lambda (payload _c _r) (format "OK: %s" (plist-get payload :msg))))

      (let ((post-data nil)
            (result nil))
        (add-hook 'macher-agent-post-tool-use-hook
                  (lambda (sym payload data)
                    (setq post-data (list sym payload data))))
        (funcall (gptel-tool-function mock-post-tool)
                 (lambda (res) (setq result res))
                 "test")
        (expect result :to-equal "OK: test")
        (expect (car post-data) :to-be 'mock-post-tool)
        (expect (nth 2 post-data) :to-equal "OK: test")))

    (it "traps execution errors and runs post-tool-use-failure-hook"
      (macher-agent-make-tool mock-fail-tool "Fail target"
        :args '((:name "msg" :type string))
        :command-fn (lambda (_p _c _r) (error "Fatal tool failure")))

      (let ((fail-info nil)
            (result nil))
        (add-hook 'macher-agent-post-tool-use-failure-hook
                  (lambda (sym payload err)
                    (setq fail-info (list sym payload err))))
        (funcall (gptel-tool-function mock-fail-tool)
                 (lambda (res) (setq result res))
                 "test")
        (expect result :to-match "Fatal tool failure")
        (expect (car fail-info) :to-be 'mock-fail-tool)
        (expect (error-message-string (nth 2 fail-info)) :to-equal "Fatal tool failure"))))

  ;; --------------------------------------------------------------------------
  ;; 5. Instruction Queue and Memory Search Utilities
  ;; --------------------------------------------------------------------------
  (describe "5. Instruction Queue and Memory Search Utilities"
    (it "pushes override directives buffer-locally to pending instructions queue"
      (with-temp-buffer
        (setq-local macher-agent--pending-instructions-queue nil)
        (macher-agent-add-pending-instruction "Refactor module X")
        (macher-agent-add-pending-instruction "Ensure all tests pass")
        (expect (length macher-agent--pending-instructions-queue) :to-equal 2)
        (expect (car macher-agent--pending-instructions-queue)
                :to-equal "USER OVERRIDE DIRECTIVE:\nRefactor module X")
        (expect (cadr macher-agent--pending-instructions-queue)
                :to-equal "USER OVERRIDE DIRECTIVE:\nEnsure all tests pass")))

    (it "searches conversation history buffers accurately with line contexts"
      (let ((buf (generate-new-buffer "*mock-search-history*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (insert "Line 1: preamble\n")
                (insert "Line 2: setup context\n")
                (insert "Line 3: target pattern ALPHA found here\n")
                (insert "Line 4: trailing detail\n")
                (insert "Line 5: end of segment\n"))

              ;; Successful match with context
              (let ((res (macher-agent-search-glob "ALPHA" buf 1)))
                (expect res :to-match "--- Match near line 3 ---")
                (expect res :to-match "target pattern ALPHA found here"))

              ;; No match found
              (let ((res (macher-agent-search-glob "NONEXISTENT_KEYWORD" buf 1)))
                (expect res :to-equal "No matches found in history for: NONEXISTENT_KEYWORD"))

              ;; Invalid regex
              (let ((res (macher-agent-search-glob "[unclosed-bracket" buf 1)))
                (expect res :to-match "Error: Invalid regular expression:")))
          (kill-buffer buf))))

    (it "handles dead buffers gracefully during history search"
      (let ((dead-buf (generate-new-buffer "*mock-dead-buf*")))
        (kill-buffer dead-buf)
        (expect (macher-agent-search-glob "query" dead-buf)
                :to-equal "Error: Cannot locate original conversation buffer.")))))

(provide 'macher-agent-tools-test)
;;; macher-agent-tools-test.el ends here
