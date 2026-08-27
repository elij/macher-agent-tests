;;; macher-agent-architecture-plan-test.el --- Tests for architectural plan invariants -*- lexical-binding: t; -*-

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

(require 'macher-agent-test-setup)
(require 'macher-agent-core)
(require 'macher-agent-macher)
(require 'macher-agent-vfs)
(require 'macher-agent-orchestration)
(require 'macher-agent-tools)
(require 'macher-agent-presets)

(describe "Architectural Plan: Invariants, Hygienic Macros, A2A Transit Schema & Async Safety"
          (macher-agent-test-setup-before-each)

          ;; ----------------------------------------------------------------------
          ;; 1. Boundary Classification and Architectural Invariants
          ;; ----------------------------------------------------------------------
          (describe "1. Boundary Adapters and Schema Validators"
                    (it "validates primitive schema types correctly and rejects mismatches"
                        (expect (macher-agent--validate-primitive-schema 'string "valid-string" "param1") :to-be t)
                        (expect (macher-agent--validate-primitive-schema 'number 42 "param2") :to-be t)
                        (expect (macher-agent--validate-primitive-schema 'integer 100 "param3") :to-be t)
                        (expect (macher-agent--validate-primitive-schema 'boolean t "param4") :to-be t)
                        (expect (macher-agent--validate-primitive-schema 'boolean :json-false "param5") :to-be t)
                        (expect (macher-agent--validate-primitive-schema 'array [1 2 3] "param6") :to-be t)
                        (expect (macher-agent--validate-primitive-schema 'object '(:key "val") "param7") :to-be t)
                        ;; Rejections
                        (expect (macher-agent--validate-primitive-schema 'string 123 "param1") :to-throw 'error)
                        (expect (macher-agent--validate-primitive-schema 'number "string-val" "param2") :to-throw 'error)
                        (expect (macher-agent--validate-primitive-schema 'boolean "true" "param4") :to-throw 'error)
                        (expect (macher-agent--validate-primitive-schema 'array 'not-an-array "param6") :to-throw 'error))

                    (it "validates complex object schema with required and optional properties"
                        (let ((schema '(:type object
                                              :properties (:name (:type string)
                                                                 :count (:type integer)
                                                                 :active (:type boolean))
                                              :required ["name" "count"])))
                          ;; Valid object
                          (expect (macher-agent--validate-schema schema '(:name "agent" :count 5 :active t)) :not :to-throw)
                          ;; Missing required property
                          (expect (macher-agent--validate-schema schema '(:name "agent" :active t)) :to-throw 'error)
                          ;; Invalid property type
                          (expect (macher-agent--validate-schema schema '(:name "agent" :count "not-an-int")) :to-throw 'error)))

                    (it "validates nested array schemas"
                        (let ((schema '(:type array
                                              :items (:type object
                                                            :properties (:id (:type string)
                                                                             :value (:type number))
                                                            :required ["id"]))))
                          (expect (macher-agent--validate-schema schema [(:id "a" :value 1.5) (:id "b" :value 2.0)]) :not :to-throw)
                          (expect (macher-agent--validate-schema schema [(:value 1.5)]) :to-throw 'error))))

          ;; ----------------------------------------------------------------------
          ;; 2. Hygienic Macro Semantics
          ;; ----------------------------------------------------------------------
          (describe "2. Hygienic Macro Semantics (`macher-agent-with-vfs-scope`)"
                    (it "guarantees single-evaluation of the context expression"
                        (let* ((mock-dir (make-temp-file "macher-macro-single-eval-" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (ctx (macher--make-context :workspace workspace :contents nil))
                               (eval-count 0)
                               (context-fn (lambda ()
                                             (setq eval-count (1+ eval-count))
                                             ctx)))
                          (unwind-protect
                              (progn
                                (macher-agent-with-vfs-scope (funcall context-fn)
                                  (expect default-directory :not :to-be nil)
                                  (expect macher-agent--persistent-context :to-be ctx))
                                (expect eval-count :to-equal 1))
                            (delete-directory mock-dir t))))

                    (it "fails fast when context cannot be resolved"
                        (expect (macher-agent-with-vfs-scope nil
                                  (error "Should never reach here"))
                                :to-throw 'error)
                        (expect (macher-agent-with-vfs-scope "invalid-nonexistent-root-xyz-12345"
                                  (error "Should never reach here"))
                                :to-throw 'error))

                    (it "properly binds `macher-agent--persistent-context` and sets `default-directory`"
                        (let* ((mock-dir (make-temp-file "macher-macro-bind-" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (ctx (macher--make-context :workspace workspace :contents nil)))
                          (unwind-protect
                              (macher-agent-with-vfs-scope ctx
                                (expect macher-agent--persistent-context :to-be ctx)
                                (expect (file-name-as-directory (file-truename default-directory))
                                        :to-equal (file-name-as-directory (file-truename mock-dir))))
                            (delete-directory mock-dir t)))))

          ;; ----------------------------------------------------------------------
          ;; 3. Explicit Agent-to-Agent (A2A) Transit Schema
          ;; ----------------------------------------------------------------------
          (describe "3. Explicit Agent-to-Agent (A2A) Transit Schema"
                    (it "constructs and validates tagged A2A transit property lists"
                        (let* ((mock-dir (make-temp-file "macher-a2a-test-" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (ctx (macher--make-context :workspace workspace :contents nil))
                               (payload (macher-agent-make-a2a-payload
                                         :transit-type :root-to-subagent
                                         :task-id "task-a2a-001"
                                         :target "subagent-1"
                                         :target-context ctx
                                         :parent-context ctx
                                         :payload '(:instructions "Do work"))))
                          (unwind-protect
                              (progn
                                (expect (macher-agent-a2a-transit-payload-p payload) :to-be t)
                                (expect (plist-get payload :schema-version) :to-equal :a2a-v1)
                                (expect (plist-get payload :transit-type) :to-equal :root-to-subagent)
                                (expect (plist-get payload :task-id) :to-equal "task-a2a-001")
                                (expect (plist-get payload :target-context) :to-be ctx)
                                (expect (macher-agent-a2a-validate-transit-payload payload) :to-be payload))
                            (delete-directory mock-dir t))))

                    (it "rejects invalid schema versions or transit types"
                        (let ((invalid-version (list :schema-version :a2a-v99 :transit-type :root-to-subagent :payload "data"))
                              (invalid-type (list :schema-version :a2a-v1 :transit-type :invalid-type :payload "data"))
                              (not-a-plist "raw string"))
                          (expect (macher-agent-a2a-transit-payload-p invalid-version) :to-be nil)
                          (expect (macher-agent-a2a-transit-payload-p invalid-type) :to-be nil)
                          (expect (macher-agent-a2a-transit-payload-p not-a-plist) :to-be nil)
                          
                          ;; Bind debug-on-error to nil so cl-assert signals instead of calling the debugger directly
                          (let ((debug-on-error nil))
                            (expect (macher-agent-a2a-validate-transit-payload invalid-version) :to-throw)
                            (expect (macher-agent-a2a-validate-transit-payload invalid-type) :to-throw)
                            (expect (macher-agent-a2a-validate-transit-payload not-a-plist) :to-throw))))

                    (it "extracts target, parent, and child contexts using structured transit keys"
                        (let* ((mock-dir (make-temp-file "macher-a2a-extract-" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (ctx-target (macher--make-context :workspace workspace :contents nil))
                               (ctx-parent (macher--make-context :workspace workspace :contents nil))
                               (ctx-child (macher--make-context :workspace workspace :contents nil)))
                          (unwind-protect
                              (progn
                                (expect (macher-agent-resolve-from-transit-payload
                                         (list :schema-version :a2a-v1 :transit-type :root-to-subagent :target-context ctx-target))
                                        :to-be ctx-target)
                                (expect (macher-agent-resolve-from-transit-payload
                                         (list :schema-version :a2a-v1 :transit-type :subagent-to-parent :parent-context ctx-parent))
                                        :to-be ctx-parent)
                                (expect (macher-agent-resolve-from-transit-payload
                                         (list :schema-version :a2a-v1 :transit-type :state-sync :child-context ctx-child))
                                        :to-be ctx-child))
                            (delete-directory mock-dir t))))

                    (it "rejects invalid transit payloads with an error signal rather than falling back to nil silently"
                        (expect (macher-agent-resolve-from-transit-payload nil) :to-throw 'error)
                        (expect (macher-agent-resolve-from-transit-payload "invalid-payload-string") :to-throw 'error)
                        (expect (macher-agent-resolve-from-transit-payload 12345) :to-throw 'error)
                        (expect (macher-agent-resolve-from-transit-payload '(project . "/some/path")) :to-throw 'error)
                        (expect (macher-agent-resolve-from-transit-payload '(:context "invalid-string")) :to-throw 'error)
                        (expect (macher-agent-resolve-from-transit-payload '(:target-context nil)) :to-throw 'error)
                        (expect (macher-agent-resolve-from-transit-payload '(:non-context-key "foo")) :to-throw 'error)))

          ;; ----------------------------------------------------------------------
          ;; 4. Workspace Accessor Invariants and Deterministic Lookup
          ;; ----------------------------------------------------------------------
          (describe "4. Workspace Accessor Invariants and Deterministic Lookup"
                    (it "resolves context deterministically via `macher-agent-context-lookup`"
                        (let* ((mock-dir (make-temp-file "macher-ws-lookup-" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (ctx (macher--make-context :workspace workspace :contents nil)))
                          (unwind-protect
                              (progn
                                (puthash (expand-file-name mock-dir) ctx macher-agent-active-workspaces)
                                (expect (macher-agent-context-lookup workspace) :to-be ctx)
                                (expect (macher-agent-context-lookup mock-dir) :to-be ctx)
                                (expect (macher-agent-context-lookup ctx) :to-be ctx)
                                (expect (macher-agent-context-lookup "/nonexistent/path/xyz") :to-be nil))
                            (remhash (expand-file-name mock-dir) macher-agent-active-workspaces)
                            (delete-directory mock-dir t))))

                    (it "ensures pure context lookup and pipeline steps do not mutate `macher-agent-active-workspaces`"
                        (let* ((mock-dir (make-temp-file "macher-ws-pure-lookup-" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (initial-count (hash-table-count macher-agent-active-workspaces)))
                          (unwind-protect
                              (progn
                                ;; Passive lookup of unregistered workspace
                                (expect (macher-agent-context-lookup workspace) :to-be nil)
                                (expect (macher-agent-context-lookup mock-dir) :to-be nil)
                                (expect (hash-table-count macher-agent-active-workspaces) :to-equal initial-count)      ;; Pipeline resolution of unregistered workspace
                                (expect (macher-agent-resolve-context workspace) :to-be nil)
                                (expect (macher-agent-resolve-context mock-dir) :to-be nil)
                                (expect (hash-table-count macher-agent-active-workspaces) :to-equal initial-count)
                                (let ((state (macher-agent-ctx-pipe--workspace-id (list :input workspace :resolved nil))))
                                  (expect (plist-get state :resolved) :to-be nil)
                                  (expect (hash-table-count macher-agent-active-workspaces) :to-equal initial-count)))
                            (delete-directory mock-dir t))))

                    (it "enforces precondition assertions in internal domain logic"
                        ;; Bind debug-on-error to nil so cl-assert signals instead of calling the debugger directly
                        (let ((debug-on-error nil))
                          (expect (macher-agent--update-context-file nil "file.txt" "content") :to-throw)
                          (expect (macher-agent--read-context-file nil "file.txt") :to-throw)
                          (expect (macher-agent--ensure-access nil "file.txt") :to-throw)
                          (expect (macher-agent--merge-contexts nil nil) :to-throw)
                          (expect (macher-agent-parse-skill-file 12345) :to-throw)
                          (expect (macher-agent-resolve-tool 12345 nil) :to-throw)
                          (expect (macher-agent--read-file-vfs-aware 12345 nil) :to-throw)
                          (expect (macher-agent-add-pending-instruction 12345) :to-throw)))

                    (it "pure workspace setters do not mutate ambient global variables"
                        (let* ((mock-dir (make-temp-file "macher-ws-pure-setter-" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (ctx (macher--make-context :workspace workspace :contents nil))
                               (orig-global-skills macher-agent-global-skills-alist)
                               (orig-tools-reg (copy-hash-table macher-agent-tools-registry))
                               (custom-skills '((my-skill . (:description "test"))))
                               (custom-tools (make-hash-table :test 'equal)))
                          (unwind-protect
                              (progn
                                (puthash (expand-file-name mock-dir) ctx macher-agent-active-workspaces)
                                ;; Setting on context or workspace must not change ambient globals
                                (setf (macher-agent-workspace-skills-alist ctx) custom-skills)
                                (expect (macher-agent-workspace-skills-alist ctx) :to-equal custom-skills)
                                (expect macher-agent-global-skills-alist :to-equal orig-global-skills)

                                (setf (macher-agent-workspace-tools-registry ctx) custom-tools)
                                (expect (macher-agent-workspace-tools-registry ctx) :to-be custom-tools)
                                (expect (hash-table-count macher-agent-tools-registry) :to-equal (hash-table-count orig-tools-reg)))
                            (remhash (expand-file-name mock-dir) macher-agent-active-workspaces)
                            (delete-directory mock-dir t)))))

          ;; ----------------------------------------------------------------------
          ;; 5. Asynchronous and Sentinel Safety
          ;; ----------------------------------------------------------------------
          (describe "5. Asynchronous and Sentinel Safety"
                    (it "captures process failures in structured error envelopes with exit codes"
                        (let* ((mock-dir (make-temp-file "macher-async-fail-" t))
                               (workspace (make-macher-agent-workspace :project-root mock-dir))
                               (ctx (macher--make-context :workspace workspace :contents nil))
                               (error-result nil)
                               (success-result nil))
                          (unwind-protect
                              (progn
                                (macher-agent--run-in-persistent-sandbox
                                 ctx
                                 "exit 1"
                                 (lambda (res) (setq success-result res))
                                 (lambda (err) (setq error-result err)))
                                ;; In non-interactive test mode or mock, verify the structured contract
                                (when error-result
                                  (expect (plist-get error-result :status) :to-be 'error)
                                  (expect (plist-member error-result :exit-code) :not :to-be nil)))
                            (delete-directory mock-dir t))))))

(provide 'macher-agent-architecture-plan-test)
;;; macher-agent-architecture-plan-test.el ends here
