;;; macher-agent-tools-test.el --- Tests for Macher Agent Tools -*- lexical-binding: t; -*-

;;; Commentary:

;; Consolidated unit and integration tests for macher-agent-tools.el,
;; covering tool construction, payload normalization, lifecycle hooks,
;; instruction queues, and search utilities.

;;; Code:

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
(require 'macher-agent-tools)

(defvar macher-agent-search-backend-function)

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
          ;; 2. Tool Construction, Callbacks, and Execution
          ;; --------------------------------------------------------------------------
          (describe "2. Tool Construction, Callbacks, and Execution"

                    (it "defines and executes synchronous tools via gptel-make-tool and ptc-function"
                        (defvar mock-tools-sync-tool
                          (gptel-make-tool
                           :name "mock_tools_sync"
                           :description "Mock sync execution tool"
                           :category "test-tools"
                           :args '((:name "prefix" :type "string")
                                   (:name "content" :type "string"))
                           :function (macher-agent-with-presentation-context (prefix content)
                                       (let* ((native-fn (get 'mock-tools-sync-tool 'ptc-function)))
                                         (funcall native-fn prefix content context)))))

                        (put 'mock-tools-sync-tool 'ptc-function
                             (lambda (prefix content _context)
                               (format "[%s]: %s" prefix content)))

                        (expect (gptel-tool-name mock-tools-sync-tool) :to-equal "mock_tools_sync")
                        (expect (gptel-tool-category mock-tools-sync-tool) :to-equal "test-tools")
                        (expect (gptel-tool-description mock-tools-sync-tool) :to-equal "Mock sync execution tool")

                        (let* ((mock-ctx (make-macher-agent-context :id "ctx-sync" :project-root "/mock/sync-root"))
                               (result (funcall (get 'mock-tools-sync-tool 'ptc-function)
                                                "INFO" "all systems go" mock-ctx)))
                          (expect result :to-equal "[INFO]: all systems go")))

                    (it "defines and executes 4-arity asynchronous tools with callback"
                        (defvar mock-tools-async-tool
                          (gptel-make-tool
                           :name "mock_tools_async"
                           :description "Mock async execution tool"
                           :category "test-tools"
                           :args '((:name "input" :type "string"))
                           :async t
                           :function (macher-agent-with-presentation-context (input)
                                       (let* ((native-fn (get 'mock-tools-async-tool 'ptc-function)))
                                         (funcall native-fn input context
                                                  (lambda (res) (funcall callback res)))))))

                        (put 'mock-tools-async-tool 'ptc-function
                             (lambda (input _context on-success)
                               (funcall on-success (format "Async: %s" input))))

                        (let* ((mock-ctx (make-macher-agent-context :id "ctx-async" :project-root "/mock/async-root"))
                               (result nil))
                          (funcall (get 'mock-tools-async-tool 'ptc-function)
                                   "test-data" mock-ctx
                                   (lambda (res) (setq result res)))
                          (expect result :to-equal "Async: test-data")))

                    (it "applies success formatting in presentation layer and executes raw logic in ptc-function"
                        (defvar mock-tools-formatter-tool
                          (gptel-make-tool
                           :name "mock_tools_formatter"
                           :description "Mock formatting tool"
                           :category "test-tools"
                           :args '((:name "val" :type "string"))
                           :function (macher-agent-with-presentation-context (val)
                                       (let* ((native-fn (get 'mock-tools-formatter-tool 'ptc-function))
                                              (raw-res (funcall native-fn val context))
                                              (formatted (format "%s (input was %s)" raw-res val)))
                                         (upcase formatted)))))

                        (put 'mock-tools-formatter-tool 'ptc-function
                             (lambda (val _context)
                               val))

                        ;; Direct PTC invocation on mock context returns raw value
                        (let* ((mock-ctx (make-macher-agent-context :id "ctx-fmt" :project-root "/mock/fmt-root"))
                               (ptc-result (funcall (get 'mock-tools-formatter-tool 'ptc-function) "beta" mock-ctx)))
                          (expect ptc-result :to-equal "beta"))

                        ;; Presentation context formats output for the LLM
                        (let* ((mock-ctx (make-macher-agent-context :id "ctx-fmt" :project-root "/mock/fmt-root"))
                               (fsm (gptel-make-fsm :info (list :macher-agent-context mock-ctx)))
                               (result nil))
                          (cl-letf (((symbol-function 'macher-agent-get-active-fsm) (lambda () fsm)))
                            (funcall (gptel-tool-function mock-tools-formatter-tool)
                                     (lambda (res) (setq result res))
                                     "alpha")
                            (expect result :to-equal "ALPHA (INPUT WAS ALPHA)"))))

                    (it "respects explicit :include options during tool creation"
                        (let ((tool-default (gptel-make-tool :name "mock_inc_default" :description "Default include"
                                                             :args '((:name "x" :type "string"))))
                              (tool-nil (gptel-make-tool :name "mock_inc_nil" :description "Nil include"
                                                         :include nil
                                                         :args '((:name "x" :type "string"))))
                              (tool-call (gptel-make-tool :name "mock_inc_call" :description "Call include"
                                                          :include 'call
                                                          :args '((:name "x" :type "string")))))
                          (expect (gptel-tool-include tool-default) :to-be t)
                          (expect (gptel-tool-include tool-nil) :to-be nil)
                          (expect (gptel-tool-include tool-call) :to-equal 'call))))

          ;; --------------------------------------------------------------------------
          ;; 3. Lifecycle Hooks & Error Trapping
          ;; --------------------------------------------------------------------------
          (describe "3. Lifecycle Hooks & Error Trapping"
                    (it "executes post-tool-use-hook on success"
                        (let ((post-data nil))
                          (add-hook 'macher-agent-post-tool-use-hook
                                    (lambda (sym payload data)
                                      (setq post-data (list sym payload data))))
                          (run-hook-with-args 'macher-agent-post-tool-use-hook 'mock-post-tool '(:msg "test") "OK: test")
                          (expect (car post-data) :to-be 'mock-post-tool)
                          (expect (nth 2 post-data) :to-equal "OK: test")))

                    (it "traps execution errors and runs post-tool-use-failure-hook"
                        (let ((fail-info nil))
                          (add-hook 'macher-agent-post-tool-use-failure-hook
                                    (lambda (sym payload err)
                                      (setq fail-info (list sym payload err))))
                          (condition-case err
                              (error "Fatal tool failure")
                            (error
                             (run-hook-with-args 'macher-agent-post-tool-use-failure-hook 'mock-fail-tool '(:msg "test") err)))
                          (expect (car fail-info) :to-be 'mock-fail-tool)
                          (expect (error-message-string (nth 2 fail-info)) :to-equal "Fatal tool failure"))))

          ;; --------------------------------------------------------------------------
          ;; 4. Instruction Queue and Memory Search Utilities
          ;; --------------------------------------------------------------------------
          (describe "4. Instruction Queue and Memory Search Utilities"
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
                                  :to-equal "Error: Cannot locate original conversation buffer.")))

                    (it "dispatches conversation search via macher-agent-search-dispatch"
                        (let ((buf (generate-new-buffer "*mock-dispatch-buf*")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf
                                  (insert "Line 1: init\nLine 2: MATCH_ME\nLine 3: end\n"))
                                (expect (macher-agent-search-dispatch "MATCH_ME" buf 1)
                                        :to-match "MATCH_ME")
                                (let* ((custom-called nil)
                                       (macher-agent-search-backend-function
                                        (lambda (query orig-buf ctx)
                                          (setq custom-called (list query orig-buf ctx))
                                          "custom-result")))
                                  (expect (macher-agent-search-dispatch "custom" buf 2)
                                          :to-equal "custom-result")
                                  (expect custom-called :to-equal (list "custom" buf 2)))
                                (let ((macher-agent-search-backend-function nil))
                                  (expect (macher-agent-search-dispatch "MATCH_ME" buf 1)
                                          :to-match "MATCH_ME")))
                            (when (buffer-live-p buf) (kill-buffer buf)))))

                    (it "does not duplicate search backend definitions or declare-function forms in macher-agent-tools.el"
                        (let* ((tools-file (or (locate-library "macher-agent-tools.el")
                                               (expand-file-name "macher-agent-tools.el" default-directory)))
                               (forms nil))
                          (with-temp-buffer
                            (insert-file-contents tools-file)
                            (goto-char (point-min))
                            (condition-case nil
                                (while t
                                  (push (read (current-buffer)) forms))
                              (end-of-file nil)))
                          ;; Verify no duplicate defvar / defun of search utilities in macher-agent-tools.el
                          (let ((search-defs
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form)
                                         (memq (car form) '(defun defvar defcustom cl-defun))
                                         (memq (cadr form) '(macher-agent-search-backend-function
                                                             macher-agent-search-glob
                                                             macher-agent-search-dispatch))))
                                  forms)))
                            (expect search-defs :to-equal nil))
                          ;; Verify no declare-function forms targeting internal macher-agent-* symbols
                          (let ((internal-declares
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form)
                                         (eq (car form) 'declare-function)
                                         (let ((fn (cadr form)))
                                           (when (and (consp fn) (eq (car fn) 'quote))
                                             (setq fn (cadr fn)))
                                           (and (symbolp fn)
                                                (string-prefix-p "macher-agent-" (symbol-name fn))))))
                                  forms)))
                            (expect internal-declares :to-equal nil)))))

          ;; --------------------------------------------------------------------------
          ;; 5. Active Execution & Direct Context Transport
          ;; --------------------------------------------------------------------------
          (describe "5. Active Execution & Direct Context Transport"
                    (it "delegates context extraction from FSM to macher-agent-gptel-context-from-fsm"
                        (let* ((mock-ctx (make-macher-agent-context :id "ctx-fsm-delegated" :project-root "/mock/fsm-delegated-root"))
                               (received-ctx nil)
                               (result nil))
                          (defvar mock-delegated-tool
                            (gptel-make-tool
                             :name "mock_delegated"
                             :description "Tool testing FSM delegation"
                             :category "test-tools"
                             :args '((:name "input" :type "string"))
                             :function (macher-agent-with-presentation-context (input)
                                         (funcall (get 'mock-delegated-tool 'ptc-function) input context))))
                          (put 'mock-delegated-tool 'ptc-function
                               (lambda (input context)
                                 (setq received-ctx context)
                                 input))
                          (let ((fsm (gptel-make-fsm :info (list :macher-agent-context mock-ctx))))
                            (cl-letf (((symbol-function 'macher-agent-get-active-fsm) (lambda () fsm)))
                              (funcall (gptel-tool-function mock-delegated-tool)
                                       (lambda (res) (setq result res))
                                       "val-delegated")
                              (expect result :to-equal "val-delegated")
                              (expect received-ctx :to-be mock-ctx)))))

                    (it "extracts active context directly from active FSM info plist :macher-agent-context"
                        (let* ((mock-ctx (make-macher-agent-context :id "ctx-fsm-direct" :project-root "/mock/fsm-direct-root"))
                               (received-ctx nil)
                               (result nil))
                          (defvar mock-fsm-context-tool
                            (gptel-make-tool
                             :name "mock_fsm_context"
                             :description "Tool extracting FSM context"
                             :category "test-tools"
                             :args '((:name "param" :type "string"))
                             :function (macher-agent-with-presentation-context (param)
                                         (funcall (get 'mock-fsm-context-tool 'ptc-function) param context))))
                          (put 'mock-fsm-context-tool 'ptc-function
                               (lambda (param context)
                                 (setq received-ctx context)
                                 param))
                          (let ((fsm (gptel-make-fsm :info (list :macher-agent-context mock-ctx))))
                            (cl-letf (((symbol-function 'macher-agent-get-active-fsm) (lambda () fsm)))
                              (funcall (gptel-tool-function mock-fsm-context-tool)
                                       (lambda (res) (setq result res))
                                       "data-1")
                              (expect result :to-equal "data-1")
                              (expect received-ctx :to-be mock-ctx)))))

                    (it "resolves context in isolated execution buffer directly via FSM :origin-buffer"
                        (let* ((orig-buf (generate-new-buffer "*mock-orig-buf*"))
                               (iso-buf (generate-new-buffer "*mock-isolated-buf*"))
                               (mock-ctx (make-macher-agent-context :id "ctx-origin-buf" :project-root "/mock/origin-root"))
                               (received-ctx nil)
                               (result nil))
                          (unwind-protect
                              (progn
                                (with-current-buffer orig-buf
                                  (setq-local macher-agent--persistent-context mock-ctx))
                                (defvar mock-origin-tool
                                  (gptel-make-tool
                                   :name "mock_origin"
                                   :description "Tool resolving via origin buffer"
                                   :category "test-tools"
                                   :args '((:name "action" :type "string"))
                                   :function (macher-agent-with-presentation-context (action)
                                               (funcall (get 'mock-origin-tool 'ptc-function) action context))))
                                (put 'mock-origin-tool 'ptc-function
                                     (lambda (action context)
                                       (setq received-ctx context)
                                       action))
                                (let ((fsm (gptel-make-fsm :info (list :origin-buffer orig-buf))))
                                  (cl-letf (((symbol-function 'macher-agent-get-active-fsm) (lambda () fsm)))
                                    (with-current-buffer iso-buf
                                      (funcall (gptel-tool-function mock-origin-tool)
                                               (lambda (res) (setq result res))
                                               "run")
                                      (expect result :to-equal "run")
                                      (expect received-ctx :to-be mock-ctx)))))
                            (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                            (when (buffer-live-p iso-buf) (kill-buffer iso-buf)))))

                    (it "resolves context in isolated execution buffer directly via FSM :buffer fallback"
                        (let* ((orig-buf (generate-new-buffer "*mock-orig-buf-2*"))
                               (iso-buf (generate-new-buffer "*mock-isolated-buf-2*"))
                               (mock-ctx (make-macher-agent-context :id "ctx-buffer-fallback" :project-root "/mock/buffer-root"))
                               (received-ctx nil)
                               (result nil))
                          (unwind-protect
                              (progn
                                (with-current-buffer orig-buf
                                  (setq-local macher-agent--persistent-context mock-ctx))
                                (defvar mock-buffer-fallback-tool
                                  (gptel-make-tool
                                   :name "mock_buffer_fallback"
                                   :description "Tool resolving via :buffer"
                                   :category "test-tools"
                                   :args '((:name "item" :type "string"))
                                   :function (macher-agent-with-presentation-context (item)
                                               (funcall (get 'mock-buffer-fallback-tool 'ptc-function) item context))))
                                (put 'mock-buffer-fallback-tool 'ptc-function
                                     (lambda (item context)
                                       (setq received-ctx context)
                                       item))
                                (let ((fsm (gptel-make-fsm :info (list :buffer orig-buf))))
                                  (cl-letf (((symbol-function 'macher-agent-get-active-fsm) (lambda () fsm)))
                                    (with-current-buffer iso-buf
                                      (funcall (gptel-tool-function mock-buffer-fallback-tool)
                                               (lambda (res) (setq result res))
                                               "ok")
                                      (expect result :to-equal "ok")
                                      (expect received-ctx :to-be mock-ctx)))))
                            (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                            (when (buffer-live-p iso-buf) (kill-buffer iso-buf)))))

                    (it "contains no calls to obsolete context data and prompt helpers in macher-agent-tools.el"
                        (let* ((tools-file (or (locate-library "macher-agent-tools.el")
                                               (expand-file-name "macher-agent-tools.el" default-directory)))
                               (content (with-temp-buffer
                                          (insert-file-contents tools-file)
                                          (buffer-string))))
                          (expect (string-match-p "macher-agent--get-context-data" content) :to-be nil)
                          (expect (string-match-p "macher-agent--set-context-data" content) :to-be nil)
                          (expect (string-match-p "macher-agent--get-context-workspace" content) :to-be nil)
                          (expect (string-match-p "macher-agent--get-context-prompt" content) :to-be nil)
                          (expect (string-match-p "macher-agent--set-context-prompt" content) :to-be nil)))))

(provide 'macher-agent-tools-test)
;;; macher-agent-tools-test.el ends here
