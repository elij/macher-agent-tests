;;; macher-agent-test-context-resolution.el --- Context Resolution Tests -*- lexical-binding: t; -*-

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

(describe "Context Resolution Pipeline"
          (macher-agent-test-setup-before-each)

          (it "verifies macher-agent--get-active-context is deleted and unmapped"
              (expect (fboundp 'macher-agent--get-active-context) :to-be nil))

          (it "successfully mocks an active FSM context using with-macher-agent-mock-fsm"
              (let ((mock-ctx (macher--make-context :contents nil)))
                (with-macher-agent-mock-fsm mock-ctx
                                            (expect (macher-agent-resolve-context) :to-be mock-ctx))))

          (it "resolves context via macher-agent-resolve-context when gptel--fsm is bound"
              (let* ((mock-ctx (macher--make-context :contents nil))
                     (fsm (if (fboundp 'gptel-make-fsm)
                              (gptel-make-fsm :info (list :macher-agent-context mock-ctx))
                            (list :macher-agent-context mock-ctx))))
                (dlet ((gptel--fsm fsm)
                       (macher-agent--active-fsm nil)
                       (gptel--fsm-last nil))
                  (expect (macher-agent-resolve-context) :to-be mock-ctx))))

          (it "resolves context via macher-agent-resolve-context when gptel--fsm-last is bound"
              (let* ((mock-ctx (macher--make-context :contents nil))
                     (fsm (if (fboundp 'gptel-make-fsm)
                              (gptel-make-fsm :info (list :macher-agent-context mock-ctx))
                            (list :macher-agent-context mock-ctx))))
                (dlet ((gptel--fsm nil)
                       (macher-agent--active-fsm nil)
                       (gptel--fsm-last fsm))
                  (expect (macher-agent-resolve-context) :to-be mock-ctx))))

          (it "ensures buffer persistent-context remains aligned with canonical active workspace instance"
              (let* ((proj-dir "/mock/aligned-proj/")
                     (ws (make-macher-agent-workspace :project-root proj-dir))
                     (canonical-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                     (stale-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                     (buf (generate-new-buffer "aligned-buf")))
                (puthash (expand-file-name proj-dir) canonical-ctx macher-agent-active-workspaces)
                (unwind-protect
                    (with-current-buffer buf
                      (setq-local default-directory proj-dir)
                      (setq-local macher-agent--is-workspace t)
                      (setq-local macher-agent--persistent-context stale-ctx)
                      (let ((resolved (macher-agent-resolve-context)))
                        (expect resolved :to-be stale-ctx)))
                  (kill-buffer buf))))

          (it "resolves context matching the active project root rather than selecting an arbitrary workspace"
              (let* ((proj1-dir "/mock/proj1/")
                     (proj2-dir "/mock/proj2/")
                     (ws1 (make-macher-agent-workspace :project-root proj1-dir))
                     (ws2 (make-macher-agent-workspace :project-root proj2-dir))
                     (ctx1 (macher-agent--make-vfs-context :workspace ws1 :contents nil))
                     (ctx2 (macher-agent--make-vfs-context :workspace ws2 :contents nil))
                     (buf1 (generate-new-buffer "buf1"))
                     (buf2 (generate-new-buffer "buf2")))
                (puthash (expand-file-name proj1-dir) ctx1 macher-agent-active-workspaces)
                (puthash (expand-file-name proj2-dir) ctx2 macher-agent-active-workspaces)
                (with-current-buffer buf1
                  (setq-local macher-agent--is-workspace t)
                  (setq-local macher-agent--persistent-context ctx1))
                (with-current-buffer buf2
                  (setq-local macher-agent--is-workspace t)
                  (setq-local macher-agent--persistent-context ctx2))
                (unwind-protect
                    (let ((default-directory proj2-dir))
                      (expect (macher-agent-resolve-context) :to-be ctx2))
                  (kill-buffer buf1)
                  (kill-buffer buf2))))

          (it "verifies macher-agent-context-pipeline-functions variable is removed"
              (expect (boundp 'macher-agent-context-pipeline-functions) :to-be nil))

          (it "does not call macher-agent-context-resolution-install at top-level of macher-agent-macher.el"
              (let* ((root (locate-dominating-file default-directory "macher-agent-macher.el"))
                     (file (expand-file-name "macher-agent-macher.el" (or root default-directory))))
                (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (let (top-level-forms)
                    (condition-case nil
                        (while t
                          (push (read (current-buffer)) top-level-forms))
                      (end-of-file nil))
                    (expect (member '(macher-agent-context-resolution-install) top-level-forms) :to-be nil)))))

          (it "registers all 10 context-resolution pipeline steps via macher-agent-get-pipeline-steps"
              (macher-agent-context-resolution-install)
              (expect (macher-agent-get-pipeline-steps 'context-resolution)
                      :to-equal '(macher-agent-ctx-pipe--explicit
                                  macher-agent-ctx-pipe--buffer
                                  macher-agent-ctx-pipe--payload-explicit
                                  macher-agent-ctx-pipe--subagent
                                  macher-agent-ctx-pipe--workspace-id
                                  macher-agent-ctx-pipe--payload-shared
                                  macher-agent-ctx-pipe--fsm
                                  macher-agent-ctx-pipe--canonical
                                  macher-agent-ctx-pipe--fsm-fallback
                                  macher-agent-ctx-pipe--lazy-init)))

          (it "short-circuits when :resolved is already non-nil in explicit step"
              (let* ((mock-ctx (macher--make-context))
                     (state (list :input "dummy" :resolved mock-ctx :expanded-root nil))
                     (res (macher-agent-ctx-pipe--explicit state)))
                (expect (plist-get res :resolved) :to-be mock-ctx)))

          (it "resolves context when :input is a valid macher-context"
              (let* ((mock-ctx (macher--make-context))
                     (state (list :input mock-ctx :resolved nil :expanded-root nil))
                     (res (macher-agent-ctx-pipe--explicit state)))
                (expect (plist-get res :resolved) :to-be mock-ctx)))

          (it "leaves :resolved nil when :input is not a macher-context"
              (let* ((state (list :input "not-a-context" :resolved nil :expanded-root nil))
                     (res (macher-agent-ctx-pipe--explicit state)))
                (expect (plist-get res :resolved) :to-be nil)))

          (describe "Buffer Step"
                    (it "short-circuits when :resolved is already non-nil"
                        (let* ((mock-ctx (macher--make-context))
                               (buf (generate-new-buffer "test-buf-step-short"))
                               (state (list :input buf :resolved mock-ctx :expanded-root nil))
                               (res (macher-agent-ctx-pipe--buffer state)))
                          (expect (plist-get res :resolved) :to-be mock-ctx)
                          (kill-buffer buf)))

                    (it "resolves context when :input is a live buffer with persistent context"
                        (let* ((mock-ctx (macher--make-context))
                               (buf (generate-new-buffer "test-buf-step-live")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf
                                  (setq-local macher-agent--persistent-context mock-ctx))
                                (let* ((state (list :input buf :resolved nil :expanded-root nil))
                                       (res (macher-agent-ctx-pipe--buffer state)))
                                  (expect (plist-get res :resolved) :to-be mock-ctx)))
                            (kill-buffer buf))))

                    (it "leaves :resolved nil when :input is a live buffer without persistent context"
                        (let* ((buf (generate-new-buffer "test-buf-step-empty")))
                          (unwind-protect
                              (let* ((state (list :input buf :resolved nil :expanded-root nil))
                                     (res (macher-agent-ctx-pipe--buffer state)))
                                (expect (plist-get res :resolved) :to-be nil))
                            (kill-buffer buf))))

                    (it "leaves :resolved nil when :input is not a buffer"
                        (let* ((state (list :input "not-a-buf" :resolved nil :expanded-root nil))
                               (res (macher-agent-ctx-pipe--buffer state)))
                          (expect (plist-get res :resolved) :to-be nil))))

          (describe "Payload Explicit Step"
                    (it "short-circuits when :resolved is already non-nil"
                        (let* ((mock-ctx (macher--make-context))
                               (state (list :input (list :context mock-ctx) :resolved mock-ctx :expanded-root nil))
                               (res (macher-agent-ctx-pipe--payload-explicit state)))
                          (expect (plist-get res :resolved) :to-be mock-ctx)))

                    (it "resolves context from :context property in payload"
                        (let* ((mock-ctx (macher--make-context))
                               (state (list :input (list :context mock-ctx :diff nil) :resolved nil :expanded-root nil))
                               (res (macher-agent-ctx-pipe--payload-explicit state)))
                          (expect (plist-get res :resolved) :to-be mock-ctx)))

                    (it "resolves context from :target-context property in payload"
                        (let* ((mock-ctx (macher--make-context))
                               (state (list :input (list :target-context mock-ctx) :resolved nil :expanded-root nil))
                               (res (macher-agent-ctx-pipe--payload-explicit state)))
                          (expect (plist-get res :resolved) :to-be mock-ctx)))

                    (it "resolves context from :ctx property in payload"
                        (let* ((mock-ctx (macher--make-context))
                               (state (list :input (list :ctx mock-ctx) :resolved nil :expanded-root nil))
                               (res (macher-agent-ctx-pipe--payload-explicit state)))
                          (expect (plist-get res :resolved) :to-be mock-ctx)))

                    (it "resolves context from :buffer-name live buffer persistent context"
                        (let* ((mock-ctx (macher--make-context))
                               (buf (generate-new-buffer "test-payload-buf")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf
                                  (setq-local macher-agent--persistent-context mock-ctx))
                                (let* ((state (list :input (list :buffer-name (buffer-name buf)) :resolved nil :expanded-root nil))
                                       (res (macher-agent-ctx-pipe--payload-explicit state)))
                                  (expect (plist-get res :resolved) :to-be mock-ctx)))
                            (kill-buffer buf))))

                    (it "leaves :resolved nil when payload contains no direct context"
                        (let* ((state (list :input (list :message "hello") :resolved nil :expanded-root nil))
                               (res (macher-agent-ctx-pipe--payload-explicit state)))
                          (expect (plist-get res :resolved) :to-be nil))))

          (describe "Workspace ID Step"
                    (it "short-circuits when :resolved is already non-nil"
                        (let* ((mock-ctx (macher--make-context))
                               (state (list :input (list :workspace-id "/some/path") :resolved mock-ctx :expanded-root nil))
                               (res (macher-agent-ctx-pipe--workspace-id state)))
                          (expect (plist-get res :resolved) :to-be mock-ctx)))

                    (it "resolves context from explicit string workspace-id in input"
                        (let* ((proj-dir (expand-file-name "/test/ws-id-proj/"))
                               (state (list :input proj-dir :resolved nil :expanded-root nil))
                               (res (macher-agent-ctx-pipe--workspace-id state)))
                          (expect (plist-get res :resolved) :not :to-be nil)
                          (let ((ws (macher-agent--get-context-workspace (plist-get res :resolved))))
                            (expect (car ws) :to-equal 'agent)
                            (expect (cdr ws) :to-equal proj-dir))))

                    (it "resolves context from plist :workspace-id in input"
                        (let* ((proj-dir (expand-file-name "/test/ws-id-plist/"))
                               (state (list :input (list :workspace-id proj-dir) :resolved nil :expanded-root nil))
                               (res (macher-agent-ctx-pipe--workspace-id state)))
                          (expect (plist-get res :resolved) :not :to-be nil)))

                    (it "resolves context from tagged cons (agent . path) workspace-id"
                        (let* ((proj-dir (expand-file-name "/test/ws-id-agent/"))
                               (tag-ws (cons 'agent proj-dir))
                               (state (list :input (list :workspace-id tag-ws) :resolved nil :expanded-root nil))
                               (res (macher-agent-ctx-pipe--workspace-id state)))
                          (expect (plist-get res :resolved) :not :to-be nil)
                          (expect (macher-agent--get-context-workspace (plist-get res :resolved)) :to-equal tag-ws)))

                    (it "resolves context from tagged cons (project . path) workspace-id"
                        (let* ((proj-dir (expand-file-name "/test/ws-id-project/"))
                               (tag-ws (cons 'project proj-dir))
                               (state (list :input (list :workspace-id tag-ws) :resolved nil :expanded-root nil))
                               (res (macher-agent-ctx-pipe--workspace-id state)))
                          (expect (plist-get res :resolved) :not :to-be nil)
                          (expect (macher-agent--get-context-workspace (plist-get res :resolved)) :to-equal tag-ws)))

                    (it "reuses existing context from macher-agent-active-workspaces"
                        (let* ((proj-dir (expand-file-name "/test/ws-id-reuse/"))
                               (mock-ctx (macher--make-context :workspace (cons 'project proj-dir)))
                               (macher-agent-active-workspaces (make-hash-table :test 'equal)))
                          (puthash proj-dir mock-ctx macher-agent-active-workspaces)
                          (let* ((state (list :input (list :workspace-id proj-dir) :resolved nil :expanded-root nil))
                                 (res (macher-agent-ctx-pipe--workspace-id state)))
                            (expect (plist-get res :resolved) :to-be mock-ctx))))

                    (it "reuses existing context from active workspaces by directory-file-name and file-name-as-directory"
                        (let* ((proj-dir (expand-file-name "/test/ws-id-reuse-dir/"))
                               (dir-name (directory-file-name proj-dir))
                               (mock-ctx (macher--make-context :workspace (cons 'project proj-dir)))
                               (macher-agent-active-workspaces (make-hash-table :test 'equal)))
                          (puthash dir-name mock-ctx macher-agent-active-workspaces)
                          (let* ((state (list :input (list :workspace-id proj-dir) :resolved nil :expanded-root nil))
                                 (res (macher-agent-ctx-pipe--workspace-id state)))
                            (expect (plist-get res :resolved) :to-be mock-ctx)))))

          (describe "Context Workspace Extraction"
                    (it "retrieves the workspace structure from a valid macher-context"
                        (let* ((ws-agent (cons 'agent "/mock/agent/path"))
                               (ctx (macher--make-context :workspace ws-agent :contents nil)))
                          (expect (macher-agent--get-context-workspace ctx) :to-equal ws-agent)))

                    (it "retrieves the workspace structure when workspace is project tagged cons"
                        (let* ((ws-proj (cons 'project "/mock/proj/path"))
                               (ctx (macher--make-context :workspace ws-proj :contents nil)))
                          (expect (macher-agent--get-context-workspace ctx) :to-equal ws-proj)))

                    (it "returns tagged cons directly if passed an agent cons"
                        (let ((ws-agent (cons 'agent "/mock/direct/agent")))
                          (expect (macher-agent--get-context-workspace ws-agent) :to-equal ws-agent)))

                    (it "returns tagged cons directly if passed a project cons"
                        (let ((ws-proj (cons 'project "/mock/direct/project")))
                          (expect (macher-agent--get-context-workspace ws-proj) :to-equal ws-proj)))

                    (it "returns workspace directly if passed a workspace struct"
                        (let ((ws (make-macher-agent-workspace :project-root "/mock/struct/path")))
                          (expect (macher-agent--get-context-workspace ws) :to-equal ws)))

                    (it "returns nil for invalid or unhandled objects"
                        (expect (macher-agent--get-context-workspace nil) :to-be nil)
                        (expect (macher-agent--get-context-workspace 123) :to-be nil)
                        (expect (macher-agent--get-context-workspace "some/string") :to-be nil)
                        (expect (macher-agent--get-context-workspace '(:key "val")) :to-be nil)
                        (expect (macher-agent--get-context-workspace '(unknown . "/path")) :to-be nil)))

          (describe "Payload Shared Step"
                    (it "short-circuits when :resolved is already non-nil"
                        (let* ((mock-ctx (macher--make-context))
                               (state (list :input (list :shared-state (list :context mock-ctx)) :resolved mock-ctx :expanded-root nil))
                               (res (macher-agent-ctx-pipe--payload-shared state)))
                          (expect (plist-get res :resolved) :to-be mock-ctx)))

                    (it "resolves context from :shared-state :context"
                        (let* ((mock-ctx (macher--make-context))
                               (state (list :input (list :shared-state (list :context mock-ctx)) :resolved nil :expanded-root nil))
                               (res (macher-agent-ctx-pipe--payload-shared state)))
                          (expect (plist-get res :resolved) :to-be mock-ctx)))

                    (it "resolves context from :shared-state :parent-buf persistent context"
                        (let* ((mock-ctx (macher--make-context))
                               (parent-buf (generate-new-buffer "test-shared-parent")))
                          (unwind-protect
                              (progn
                                (with-current-buffer parent-buf
                                  (setq-local macher-agent--persistent-context mock-ctx))
                                (let* ((state (list :input (list :shared-state (list :parent-buf parent-buf)) :resolved nil :expanded-root nil))
                                       (res (macher-agent-ctx-pipe--payload-shared state)))
                                  (expect (plist-get res :resolved) :to-be mock-ctx)))
                            (kill-buffer parent-buf))))

                    (it "leaves :resolved nil when shared state has no context"
                        (let* ((state (list :input (list :shared-state (list :foo "bar")) :resolved nil :expanded-root nil))
                               (res (macher-agent-ctx-pipe--payload-shared state)))
                          (expect (plist-get res :resolved) :to-be nil))))

          (it "short-circuits when :resolved is already non-nil in FSM step"
              (let* ((mock-ctx (macher--make-context))
                     (state (list :input "dummy" :resolved mock-ctx :expanded-root nil))
                     (res (macher-agent-ctx-pipe--fsm state)))
                (expect (plist-get res :resolved) :to-be mock-ctx)))

          (it "extracts context from FSM input when present"
              (let* ((mock-ctx (macher--make-context))
                     (fsm (list :macher-agent-context mock-ctx))
                     (state (list :input fsm :resolved nil :expanded-root nil)))
                (spy-on 'macher-agent--extract-fsm-info :and-return-value fsm)
                (let ((res (macher-agent-ctx-pipe--fsm state)))
                  (expect (plist-get res :resolved) :to-be mock-ctx))))

          (it "leaves :resolved nil when FSM input contains no context"
              (let* ((fsm '(:some-other-key 123))
                     (state (list :input fsm :resolved nil :expanded-root nil)))
                (spy-on 'macher-agent--extract-fsm-info :and-return-value fsm)
                (let ((res (macher-agent-ctx-pipe--fsm state)))
                  (expect (plist-get res :resolved) :to-be nil))))

          (it "short-circuits when :resolved is already non-nil without evaluating root"
              (let* ((mock-ctx (macher--make-context))
                     (state (list :input nil :resolved mock-ctx :expanded-root nil)))
                (spy-on 'macher-agent-root)
                (let ((res (macher-agent-ctx-pipe--subagent state)))
                  (expect (plist-get res :resolved) :to-be mock-ctx)
                  (expect 'macher-agent-root :not :to-have-been-called))))

          (it "resolves persistent context when buffer is a subagent"
              (let* ((proj-dir "/mock/root/")
                     (ws (make-macher-agent-workspace :project-root proj-dir))
                     (mock-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                     (state (list :input nil :resolved nil :expanded-root nil)))
                (let ((macher-agent--persistent-context mock-ctx)
                      (macher-agent--is-subagent t)
                      (default-directory proj-dir))
                  (spy-on 'macher-agent-root :and-return-value proj-dir)
                  (let ((res (macher-agent-ctx-pipe--subagent state)))
                    (expect (plist-get res :resolved) :to-be mock-ctx)
                    (expect (plist-get res :expanded-root) :to-equal (expand-file-name proj-dir))))))

          (it "resolves persistent context when it matches the active workspace root"
              (let* ((proj-dir "/mock/proj/")
                     (ws (make-macher-agent-workspace :project-root proj-dir))
                     (mock-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                     (state (list :input nil :resolved nil :expanded-root nil)))
                (let ((macher-agent--persistent-context mock-ctx)
                      (macher-agent--is-subagent nil)
                      (default-directory proj-dir))
                  (spy-on 'macher-agent-root :and-return-value proj-dir)
                  (let ((res (macher-agent-ctx-pipe--subagent state)))
                    (expect (plist-get res :resolved) :to-be mock-ctx)
                    (expect (plist-get res :expanded-root) :to-equal (expand-file-name proj-dir))))))

          (it "short-circuits when :resolved is already non-nil in canonical step"
              (let* ((mock-ctx (macher--make-context))
                     (state (list :input nil :resolved mock-ctx :expanded-root nil)))
                (spy-on 'macher-agent-root)
                (let ((res (macher-agent-ctx-pipe--canonical state)))
                  (expect (plist-get res :resolved) :to-be mock-ctx)
                  (expect 'macher-agent-root :not :to-have-been-called))))

          (it "resolves canonical context from active workspaces registry without mutating buffer persistent context"
              (let* ((proj-dir "/mock/canonical-proj/")
                     (ws (make-macher-agent-workspace :project-root proj-dir))
                     (canonical-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                     (state (list :input nil :resolved nil :expanded-root (expand-file-name proj-dir))))
                (puthash (expand-file-name proj-dir) canonical-ctx macher-agent-active-workspaces)
                (let ((macher-agent--is-subagent nil)
                      (macher-agent--persistent-context nil)
                      (default-directory proj-dir))
                  (spy-on 'macher-agent-root :and-return-value proj-dir)
                  (let ((res (macher-agent-ctx-pipe--canonical state)))
                    (expect (plist-get res :resolved) :to-be canonical-ctx)
                    ;; Idempotent lookup: does not set buffer-local persistent context
                    (expect macher-agent--persistent-context :to-be nil)))))

          (it "extracts target context from workspace-id payload in macher-agent-resolve-context"
              (let* ((proj-dir "/mock/payload-ws-id-proj/")
                     (ws (make-macher-agent-workspace :project-root proj-dir))
                     (target-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                     (ambient-ctx (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/unrelated") :contents nil))
                     (payload (list :workspace-id proj-dir :diff nil)))
                (puthash (expand-file-name proj-dir) target-ctx macher-agent-active-workspaces)
                (let ((macher-agent--persistent-context ambient-ctx))
                  (expect (macher-agent-resolve-context payload) :to-be target-ctx))))

          (it "populates :expanded-root from :workspace-id in state via macher-agent--get-expanded-root"
              (let* ((proj-dir "/mock/expanded-root-test/")
                     (state (list :input (list :workspace-id proj-dir) :resolved nil :expanded-root nil)))
                (spy-on 'macher-agent-root :and-return-value proj-dir)
                (let ((res (macher-agent--get-expanded-root state)))
                  (expect (plist-get res :expanded-root) :to-equal (expand-file-name proj-dir)))))

          (it "validates string paths in macher-agent--get-expanded-root against project root boundaries"
              (let* ((proj-dir "/mock/boundary-proj/")
                     (valid-sub (concat proj-dir "subdir/"))
                     (invalid-path "/outside/foreign-proj/"))
                (spy-on 'macher-agent-root :and-return-value proj-dir)
                ;; Valid subdirectory within project root boundary
                (let* ((state-valid (list :input valid-sub :resolved nil :expanded-root nil))
                       (res-valid (macher-agent--get-expanded-root state-valid)))
                  (expect (plist-get res-valid :expanded-root) :to-equal (expand-file-name valid-sub)))
                ;; Invalid foreign path outside project boundary
                (let* ((state-invalid (list :input invalid-path :resolved nil :expanded-root nil))
                       (res-invalid (macher-agent--get-expanded-root state-invalid)))
                  (expect (plist-get res-invalid :expanded-root) :to-be nil))))

          (it "prevents subagent context leakage when input specifies a foreign workspace"
              (let* ((local-proj "/mock/subagent-local/")
                     (foreign-proj "/mock/subagent-foreign/")
                     (ws-local (make-macher-agent-workspace :project-root local-proj))
                     (ws-foreign (make-macher-agent-workspace :project-root foreign-proj))
                     (local-ctx (macher-agent--make-vfs-context :workspace ws-local :contents nil))
                     (foreign-ctx (macher-agent--make-vfs-context :workspace ws-foreign :contents nil)))
                (puthash (expand-file-name foreign-proj) foreign-ctx macher-agent-active-workspaces)
                (let ((macher-agent--persistent-context local-ctx)
                      (macher-agent--is-subagent t)
                      (default-directory local-proj))
                  (spy-on 'macher-agent-root :and-return-value local-proj)
                  ;; Subagent step should not leak local-ctx when state specifies foreign workspace
                  (let* ((state (list :input (list :workspace-id foreign-proj) :resolved nil :expanded-root (expand-file-name foreign-proj)))
                         (res (macher-agent-ctx-pipe--subagent state)))
                    (expect (plist-get res :resolved) :to-be nil)))))

          (it "performs pure idempotent lookup in canonical step without modifying registry"
              (let* ((proj-dir "/mock/subagent-canonical/")
                     (ws (make-macher-agent-workspace :project-root proj-dir))
                     (canonical-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                     (state (list :input nil :resolved nil :expanded-root (expand-file-name proj-dir))))
                (puthash (expand-file-name proj-dir) canonical-ctx macher-agent-active-workspaces)
                (let ((macher-agent--is-subagent t)
                      (default-directory proj-dir))
                  (spy-on 'macher-agent-root :and-return-value proj-dir)
                  (let ((res (macher-agent-ctx-pipe--canonical state)))
                    (expect (plist-get res :resolved) :not :to-be canonical-ctx)
                    (expect (macher-agent-valid-context-p (plist-get res :resolved)) :to-be t)
                    (expect (gethash (expand-file-name proj-dir) macher-agent-active-workspaces) :to-be canonical-ctx)))))

          (it "clones canonical context for subagent buffer providing an isolated clone"
              (let* ((proj-dir "/mock/subagent-clone-proj/")
                     (ws (make-macher-agent-workspace :project-root proj-dir))
                     (canonical-ctx (macher-agent--make-vfs-context :workspace ws :contents '(("file.txt" . "init"))))
                     (state (list :input nil :resolved nil :expanded-root (expand-file-name proj-dir))))
                (puthash (expand-file-name proj-dir) canonical-ctx macher-agent-active-workspaces)
                (let ((macher-agent--is-subagent t)
                      (default-directory proj-dir))
                  (spy-on 'macher-agent-root :and-return-value proj-dir)
                  (let* ((res (macher-agent-ctx-pipe--canonical state))
                         (resolved-ctx (plist-get res :resolved)))
                    (expect resolved-ctx :not :to-be canonical-ctx)
                    (expect (macher-agent-valid-context-p resolved-ctx) :to-be t)
                    (expect (macher-agent--get-context-workspace resolved-ctx) :to-equal ws)
                    (expect (macher-agent--get-context-contents resolved-ctx) :to-equal '(("file.txt" . "init")))
                    ;; Verify isolation: mutating resolved-ctx does not affect canonical-ctx
                    (macher-agent--set-context-dirty-p resolved-ctx t)
                    (expect (macher-agent--get-context-dirty-p canonical-ctx) :to-be nil)))))

          (it "returns canonical context directly without cloning when buffer is not a subagent"
              (let* ((proj-dir "/mock/non-subagent-proj/")
                     (ws (make-macher-agent-workspace :project-root proj-dir))
                     (canonical-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                     (state (list :input nil :resolved nil :expanded-root (expand-file-name proj-dir))))
                (puthash (expand-file-name proj-dir) canonical-ctx macher-agent-active-workspaces)
                (let ((macher-agent--is-subagent nil)
                      (default-directory proj-dir))
                  (spy-on 'macher-agent-root :and-return-value proj-dir)
                  (let* ((res (macher-agent-ctx-pipe--canonical state))
                         (resolved-ctx (plist-get res :resolved)))
                    (expect resolved-ctx :to-be canonical-ctx)))))

          (it "verifies macher-agent-clone-context and macher-clone-context aliases are removed"
              (expect (fboundp 'macher-agent-clone-context) :to-be nil)
              (expect (fboundp 'macher-clone-context) :to-be nil))

          (it "does not mutate buffer-local macher-agent--persistent-context during canonical resolution"
              (with-temp-buffer
                (let* ((proj-dir "/mock/purity-proj/")
                       (ws (make-macher-agent-workspace :project-root proj-dir))
                       (canonical-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                       (stale-ctx (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/other") :contents nil))
                       (state (list :input nil :resolved nil :expanded-root (expand-file-name proj-dir))))
                  (puthash (expand-file-name proj-dir) canonical-ctx macher-agent-active-workspaces)
                  (setq-local macher-agent--persistent-context stale-ctx)
                  (let ((res (macher-agent-ctx-pipe--canonical state)))
                    (expect (plist-get res :resolved) :to-be canonical-ctx)
                    (expect (buffer-local-value 'macher-agent--persistent-context (current-buffer)) :to-be stale-ctx)
                    (expect macher-agent--persistent-context :to-be stale-ctx)))))

          (it "verifies macher-agent--resolve-context-from-ws and macher-agent-current-context are deleted"
              (expect (fboundp 'macher-agent--resolve-context-from-ws) :to-be nil)
              (expect (fboundp 'macher-agent-current-context) :to-be nil))

          (it "short-circuits when :resolved is already non-nil in FSM fallback step"
              (let* ((mock-ctx (macher--make-context))
                     (state (list :input nil :resolved mock-ctx :expanded-root nil)))
                (spy-on 'macher-agent--get-fsm-latest)
                (let ((res (macher-agent-ctx-pipe--fsm-fallback state)))
                  (expect (plist-get res :resolved) :to-be mock-ctx)
                  (expect 'macher-agent--get-fsm-latest :not :to-have-been-called))))

          (it "resolves context from latest FSM when available"
              (let* ((mock-ctx (macher--make-context))
                     (fsm (list :macher-agent-context mock-ctx))
                     (state (list :input nil :resolved nil :expanded-root nil)))
                (spy-on 'macher-agent--get-fsm-latest :and-return-value fsm)
                (spy-on 'macher-agent--extract-fsm-info :and-return-value fsm)
                (let ((res (macher-agent-ctx-pipe--fsm-fallback state)))
                  (expect (plist-get res :resolved) :to-be mock-ctx))))

          (it "short-circuits when :resolved is already non-nil in lazy-init step"
              (let* ((mock-ctx (macher--make-context))
                     (state (list :input nil :resolved mock-ctx :expanded-root nil)))
                (spy-on 'macher-agent--resolve-context-lazy-init)
                (let ((res (macher-agent-ctx-pipe--lazy-init state)))
                  (expect (plist-get res :resolved) :to-be mock-ctx)
                  (expect 'macher-agent--resolve-context-lazy-init :not :to-have-been-called))))

          (it "resolves context via lazy initialization when permitted"
              (let* ((mock-ctx (macher--make-context))
                     (state (list :input nil :resolved nil :expanded-root nil)))
                (spy-on 'macher-agent--resolve-context-lazy-init :and-return-value mock-ctx)
                (let ((res (macher-agent-ctx-pipe--lazy-init state)))
                  (expect (plist-get res :resolved) :to-be mock-ctx))))

          (it "leaves :resolved nil when lazy initialization fails or returns nil"
              (let ((state (list :input nil :resolved nil :expanded-root nil)))
                (spy-on 'macher-agent--resolve-context-lazy-init :and-return-value nil)
                (let ((res (macher-agent-ctx-pipe--lazy-init state)))
                  (expect (plist-get res :resolved) :to-be nil))))

          (it "passes initial state through seq-reduce over registered context-resolution pipeline steps"
              (let ((mock-ctx (macher--make-context)))
                (spy-on 'macher-agent-ctx-pipe--explicit :and-return-value (list :input mock-ctx :resolved mock-ctx :expanded-root nil))
                (spy-on 'macher-agent-ctx-pipe--buffer :and-call-through)
                (spy-on 'macher-agent-ctx-pipe--payload-explicit :and-call-through)
                (spy-on 'macher-agent-ctx-pipe--workspace-id :and-call-through)
                (spy-on 'macher-agent-ctx-pipe--payload-shared :and-call-through)
                (spy-on 'macher-agent-ctx-pipe--fsm :and-call-through)
                (spy-on 'macher-agent-ctx-pipe--subagent :and-call-through)
                (spy-on 'macher-agent-ctx-pipe--canonical :and-call-through)
                (spy-on 'macher-agent-ctx-pipe--fsm-fallback :and-call-through)
                (spy-on 'macher-agent-ctx-pipe--lazy-init :and-call-through)
                (let ((res (macher-agent-resolve-context mock-ctx)))
                  (expect res :to-be mock-ctx)
                  (expect 'macher-agent-ctx-pipe--explicit :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--buffer :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--payload-explicit :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--workspace-id :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--payload-shared :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--fsm :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--subagent :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--canonical :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--fsm-fallback :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--lazy-init :to-have-been-called))))

          (it "returns nil when context resolution fails across all steps"
              (spy-on 'macher-agent--resolve-context-lazy-init :and-return-value nil)
              (let ((macher-agent-active-workspaces (make-hash-table :test 'equal))
                    (macher-agent--persistent-context nil)
                    (macher--fsm-latest nil)
                    (gptel--fsm-last nil)
                    (macher-agent--active-fsm nil))
                (expect (macher-agent-resolve-context) :to-be nil)))

          (it "resolves context directly from a payload property list"
              (let* ((mock-ctx (macher--make-context))
                     (payload (list :context mock-ctx :diff '(("foo.el" "a" . "b")))))
                (expect (macher-agent-resolve-context payload) :to-be mock-ctx)))

          (it "extracts FSM context safely via macher-agent--extract-fsm-context in core"
              (let* ((mock-ctx (macher--make-context))
                     (fsm-plist (list :macher-agent-context mock-ctx :buffer nil))
                     (fsm-legacy-plist (list :macher--context mock-ctx :buffer nil))
                     (fsm-alist `((:macher-agent-context . ,mock-ctx) (:buffer . nil)))
                     (fsm-ht (let ((ht (make-hash-table :test 'equal)))
                               (puthash :macher-agent-context mock-ctx ht)
                               ht)))
                (expect (macher-agent--extract-fsm-context fsm-plist) :to-be mock-ctx)
                (expect (macher-agent--extract-fsm-context fsm-legacy-plist) :to-be mock-ctx)
                (expect (macher-agent--extract-fsm-context fsm-alist) :to-be mock-ctx)
                (expect (macher-agent--extract-fsm-context fsm-ht) :to-be mock-ctx)
                (expect (macher-agent--extract-fsm-context mock-ctx) :to-be mock-ctx)
                (expect (macher-agent--extract-fsm-context nil) :to-be nil)))

          (it "extracts FSM info and context safely without wrong-type-argument plistp for non-plists"
              (let* ((ws-cons (cons 'project "/mock/proj/"))
                     (ws-alist '((project . "/mock/proj/")))
                     (general-alist '((key1 . "val1") (key2 . "val2")))
                     (buf (generate-new-buffer "fsm-safe-buf"))
                     (vec [1 2 3]))
                (unwind-protect
                    (progn
                      (expect (macher-agent--extract-fsm-info ws-cons) :to-be nil)
                      (expect (macher-agent--extract-fsm-info ws-alist) :to-be nil)
                      (expect (macher-agent--extract-fsm-info general-alist) :to-be nil)
                      (expect (macher-agent--extract-fsm-info buf) :to-be nil)
                      (expect (macher-agent--extract-fsm-info vec) :to-be nil)
                      (expect (macher-agent--extract-fsm-info nil) :to-be nil)
                      (expect (macher-agent--extract-fsm-context ws-cons) :to-be nil)
                      (expect (macher-agent--extract-fsm-context ws-alist) :to-be nil)
                      (expect (macher-agent--extract-fsm-context general-alist) :to-be nil)
                      (expect (macher-agent--extract-fsm-context buf) :to-be nil)
                      (expect (macher-agent--extract-fsm-context vec) :to-be nil))
                  (kill-buffer buf))))

          (it "resolves context safely when input is an alist, cons cell, hash table, or context struct"
              (let* ((proj-dir "/mock/safe-res-proj/")
                     (ws (make-macher-agent-workspace :project-root proj-dir))
                     (canonical-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                     (ws-cons (cons 'project proj-dir))
                     (ws-alist `((project . ,proj-dir)))
                     (payload-alist `((:context . ,canonical-ctx)))
                     (payload-ht (let ((ht (make-hash-table :test 'equal)))
                                   (puthash :context canonical-ctx ht)
                                   ht)))
                (puthash (expand-file-name proj-dir) canonical-ctx macher-agent-active-workspaces)
                (expect (macher-agent-resolve-context canonical-ctx) :to-be canonical-ctx)
                (expect (macher-agent-resolve-context ws-cons) :to-be canonical-ctx)
                (expect (macher-agent-resolve-context ws-alist) :to-be canonical-ctx)
                (expect (macher-agent-resolve-context payload-alist) :to-be canonical-ctx)
                (expect (macher-agent-resolve-context payload-ht) :to-be canonical-ctx)
                (expect (macher-agent-resolve-context '((arbitrary . "data"))) :to-be nil)))

          (it "consolidates context validation with macher-agent-valid-context-p"
              (let ((ctx (macher--make-context :contents nil)))
                (expect (macher-agent-valid-context-p ctx) :to-be t)
                (expect (macher-agent-valid-context-p nil) :to-be nil)
                (expect (macher-agent-valid-context-p "invalid") :to-be nil)))

          (it "validates context predicates without infinite recursion or nesting overflow"
              (let ((ctx (macher--make-context :contents nil))
                    (task-ctx (when (fboundp 'make-macher-agent-task-context)
                                (make-macher-agent-task-context :workspace "/mock/proj")))
                    (ws (make-macher-agent-workspace :project-root "/mock/proj/"))
                    (ht (make-hash-table :test 'equal))
                    (max-lisp-eval-depth 300))
                (expect (macher-agent-valid-context-p ctx) :to-be t)
                (expect (macher-agent--context-p ctx) :to-be t)
                (when task-ctx
                  (expect (macher-agent-valid-context-p task-ctx) :to-be t)
                  (expect (macher-agent--context-p task-ctx) :to-be t))
                (expect (macher-agent-valid-context-p nil) :to-be nil)
                (expect (macher-agent--context-p nil) :to-be nil)
                (expect (macher-agent-valid-context-p "invalid") :to-be nil)
                (expect (macher-agent--context-p "invalid") :to-be nil)
                (expect (macher-agent-valid-context-p '(:foo "bar")) :to-be nil)
                (expect (macher-agent--context-p '(:foo "bar")) :to-be nil)
                (expect (macher-agent-valid-context-p ws) :to-be nil)
                (expect (macher-agent--context-p ws) :to-be nil)
                (expect (macher-agent-valid-context-p ht) :to-be nil)
                (expect (macher-agent--context-p ht) :to-be nil)
                (expect (macher-agent-valid-context-p [1 2 3]) :to-be nil)
                (expect (macher-agent--context-p [1 2 3]) :to-be nil)))

          (it "extracts workspace id via macher-agent-extract-workspace-id"
              (let* ((ws (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ht (make-hash-table :test 'equal)))
                (puthash :workspace-id "/mock/ht-proj/" ht)
                (expect (macher-agent-extract-workspace-id "/mock/proj/") :to-equal "/mock/proj/")
                (expect (macher-agent-extract-workspace-id ws) :to-equal ws)
                (expect (macher-agent-extract-workspace-id '(:workspace-id "/mock/plist/")) :to-equal "/mock/plist/")
                (expect (macher-agent-extract-workspace-id '((:workspace-id . "/mock/alist/"))) :to-equal "/mock/alist/")
                (expect (macher-agent-extract-workspace-id ht) :to-equal "/mock/ht-proj/")
                (expect (macher-agent-extract-workspace-id nil) :to-be nil)))

          (it "routes payload merging cleanly through macher-agent-context-update and triggers mutation hooks"
              (let* ((ws (make-macher-agent-workspace :project-root "/mock/fallback-proj/"))
                     (target-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                     (mutated-paths nil)
                     (payload (list :context target-ctx
                                    :diff (list (macher-agent-vfs-make-entry "file-fallback.el" nil "content-v1")))))
                (let ((macher-agent-context-mutated-hook
                       (list (lambda (path) (push path mutated-paths)))))
                  (macher-agent-vfs--merge-payload payload)
                  (expect (macher-agent--get-context-dirty-p target-ctx) :to-be t)
                  (expect (cl-find (macher-agent--normalize-path-key "file-fallback.el" target-ctx)
                                   mutated-paths :test #'equal)
                          :to-be-truthy)
                  (let ((entry (cl-find "file-fallback.el" (macher-agent--get-context-contents target-ctx) :key #'car :test #'equal)))
                    (expect (macher-agent-vfs-entry-curr entry) :to-equal "content-v1"))))))

(provide 'macher-agent-test-context-resolution)
;;; macher-agent-test-context-resolution.el ends here