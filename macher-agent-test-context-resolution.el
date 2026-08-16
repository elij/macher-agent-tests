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

          (it "returns nil when no FSM is active without accessing disk"
              (dlet ((gptel--fsm nil)
                     (macher-agent--active-fsm nil)
                     (gptel--fsm-last nil)
                     (macher-agent--persistent-context nil))
                (spy-on 'macher-agent-resolve-context :and-return-value nil)
                (expect (macher-agent--get-active-context) :to-be nil)
                (expect 'macher-agent-resolve-context :not :to-have-been-called)))

          (it "successfully mocks an active FSM context using with-macher-agent-mock-fsm"
              (let ((mock-ctx (macher--make-context :contents nil)))
                (with-macher-agent-mock-fsm mock-ctx
                                            (expect (macher-agent--get-active-context) :to-be mock-ctx))))

          (it "resolves context via macher-agent--resolve-context when gptel--fsm is bound"
              (let* ((mock-ctx (macher--make-context :contents nil))
                     (fsm (if (fboundp 'gptel-make-fsm)
                              (gptel-make-fsm :info (list :macher-agent-context mock-ctx))
                            (list :macher-agent-context mock-ctx))))
                (dlet ((gptel--fsm fsm)
                       (macher-agent--active-fsm nil)
                       (gptel--fsm-last nil))
                  (expect (macher-agent--get-active-context) :to-be mock-ctx))))

          (it "resolves context via macher-agent--resolve-context when gptel--fsm-last is bound"
              (let* ((mock-ctx (macher--make-context :contents nil))
                     (fsm (if (fboundp 'gptel-make-fsm)
                              (gptel-make-fsm :info (list :macher-agent-context mock-ctx))
                            (list :macher-agent-context mock-ctx))))
                (dlet ((gptel--fsm nil)
                       (macher-agent--active-fsm nil)
                       (gptel--fsm-last fsm))
                  (expect (macher-agent--get-active-context) :to-be mock-ctx))))

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

          (it "defines macher-agent-context-pipeline-functions with all 6 step functions in order"
              (expect macher-agent-context-pipeline-functions
                      :to-equal '(macher-agent-ctx-pipe--explicit
                                  macher-agent-ctx-pipe--fsm
                                  macher-agent-ctx-pipe--subagent
                                  macher-agent-ctx-pipe--canonical
                                  macher-agent-ctx-pipe--fsm-fallback
                                  macher-agent-ctx-pipe--lazy-init)))

          (it "short-circuits when :resolved is already non-nil"
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

          (it "short-circuits when :resolved is already non-nil"
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

          (it "short-circuits when :resolved is already non-nil"
              (let* ((mock-ctx (macher--make-context))
                     (state (list :input nil :resolved mock-ctx :expanded-root nil)))
                (spy-on 'macher-agent-root)
                (let ((res (macher-agent-ctx-pipe--canonical state)))
                  (expect (plist-get res :resolved) :to-be mock-ctx)
                  (expect 'macher-agent-root :not :to-have-been-called))))

          (it "resolves canonical context from active workspaces registry"
              (let* ((proj-dir "/mock/canonical-proj/")
                     (ws (make-macher-agent-workspace :project-root proj-dir))
                     (canonical-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                     (state (list :input nil :resolved nil :expanded-root (expand-file-name proj-dir))))
                (puthash (expand-file-name proj-dir) canonical-ctx macher-agent-active-workspaces)
                (let ((macher-agent--is-subagent nil)
                      (default-directory proj-dir))
                  (spy-on 'macher-agent-root :and-return-value proj-dir)
                  (let ((res (macher-agent-ctx-pipe--canonical state)))
                    (expect (plist-get res :resolved) :to-be canonical-ctx)
                    (expect macher-agent--persistent-context :to-be canonical-ctx)))))

          (it "clones canonical context when resolving in a subagent buffer"
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
                    (expect (macher-context-p (plist-get res :resolved)) :to-be t)))))

          (it "short-circuits when :resolved is already non-nil"
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

          (it "short-circuits when :resolved is already non-nil"
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

          (it "passes initial state through seq-reduce over macher-agent-context-pipeline-functions"
              (let ((mock-ctx (macher--make-context)))
                (spy-on 'macher-agent-ctx-pipe--explicit :and-return-value (list :input mock-ctx :resolved mock-ctx :expanded-root nil))
                (spy-on 'macher-agent-ctx-pipe--fsm :and-call-through)
                (spy-on 'macher-agent-ctx-pipe--subagent :and-call-through)
                (spy-on 'macher-agent-ctx-pipe--canonical :and-call-through)
                (spy-on 'macher-agent-ctx-pipe--fsm-fallback :and-call-through)
                (spy-on 'macher-agent-ctx-pipe--lazy-init :and-call-through)
                (let ((res (macher-agent-resolve-context mock-ctx)))
                  (expect res :to-be mock-ctx)
                  (expect 'macher-agent-ctx-pipe--explicit :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--fsm :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--subagent :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--canonical :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--fsm-fallback :to-have-been-called)
                  (expect 'macher-agent-ctx-pipe--lazy-init :to-have-been-called))))

          (it "throws error when context resolution fails across all steps"
              (spy-on 'macher-agent--resolve-context-lazy-init :and-return-value nil)
              (let ((macher-agent-active-workspaces (make-hash-table :test 'equal))
                    (macher-agent--persistent-context nil)
                    (macher--fsm-latest nil)
                    (gptel--fsm-last nil)
                    (macher-agent--active-fsm nil))
                (expect (macher-agent-resolve-context) :to-throw 'error)))

          (it "registers active workspace root when resolving context"
              (let* ((mock-ws (make-macher-agent-workspace :project-root "/tmp/test-workspace-resolve"))
                     (mock-ctx (macher-agent--make-vfs-context :workspace mock-ws :contents nil)))
                (spy-on 'macher-agent-ctx-pipe--explicit :and-return-value (list :input mock-ctx :resolved mock-ctx :expanded-root nil))
                (clrhash macher-agent-active-workspaces)
                (let ((res (macher-agent-resolve-context mock-ctx)))
                  (expect res :to-be mock-ctx)
                  (expect (gethash "/tmp/test-workspace-resolve" macher-agent-active-workspaces) :to-be mock-ctx)))))

(provide 'macher-agent-test-context-resolution)
;;; macher-agent-test-context-resolution.el ends here
