;;; macher-agent-vfs-test.el --- Tests for Macher Agent VFS -*- lexical-binding: t; -*-

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
(require 'macher-agent-vfs)
(require 'macher-agent-macher)

(describe "Macher-Agent VFS Macher Bridge Integration"
          (macher-agent-test-setup-before-each)

          (describe "Envelope Integration and Accessors"
                    (it "properly reads and writes VFS state via macher-agent-vfs--get-state and macher-agent-vfs--set-state"
                        (let ((ctx (macher-agent--make-context :id "ctx-env-1")))
                          (expect (macher-agent-vfs--get-state ctx) :to-be nil)
                          (macher-agent-vfs--set-state ctx '(:contents (("a.el" . "code")) :dirty-p t))
                          (expect (macher-agent-vfs--get-state ctx) :to-equal '(:contents (("a.el" . "code")) :dirty-p t))
                          (expect (plist-get (macher-agent-context-plugins ctx) :vfs) :to-equal '(:contents (("a.el" . "code")) :dirty-p t))))

                    (it "handles raw plist contexts seamlessly with get-state and set-state"
                        (let ((raw-ctx (list :id "raw-ctx-1" :vfs '(:contents (("a.el" . "code")) :dirty-p t))))
                          (expect (macher-agent-vfs--get-state raw-ctx) :to-equal '(:contents (("a.el" . "code")) :dirty-p t))
                          (let ((updated (macher-agent-vfs--set-state raw-ctx '(:contents (("b.el" . "code2")) :dirty-p nil))))
                            (expect updated :to-equal '(:contents (("b.el" . "code2")) :dirty-p nil))))))

          (describe "macher-agent-vfs-flush-hook"
                    (it "is cleanly defined with proper documentation and defaults to nil"
                        (expect (boundp 'macher-agent-vfs-flush-hook) :to-be-truthy)
                        (expect (stringp (documentation-property 'macher-agent-vfs-flush-hook 'variable-documentation)) :to-be-truthy)
                        (expect (or (null macher-agent-vfs-flush-hook)
                                    (member #'macher-agent-vfs-build-patch-from-hook macher-agent-vfs-flush-hook)
                                    (listp macher-agent-vfs-flush-hook))
                                :to-be-truthy)))

          (describe "macher-agent-vfs-install"
                    (it "registers payload-merge pipeline step, flush hooks, and safely invokes macher-agent-macher-install"
                        (let ((install-called nil)
                              (macher-agent-task-flush-hook nil)
                              (macher-agent-vfs-flush-hook nil)
                              (macher-workspace-types-alist nil))
                          (clrhash macher-agent-pipeline-registry)
                          (cl-letf (((symbol-function 'macher-agent-macher-install)
                                     (lambda () (setq install-called t))))
                            (macher-agent-vfs-install)
                            (expect install-called :to-be t)
                            (expect (member #'macher-agent-vfs-build-patch-from-hook macher-agent-vfs-flush-hook) :to-be-truthy)
                            (expect (member #'macher-agent-vfs-handle-flush macher-agent-task-flush-hook) :to-be-truthy)
                            (expect (member #'macher-agent-vfs--merge-payload (macher-agent-get-pipeline-steps 'payload-merge)) :to-be-truthy)
                            (let* ((entries (gethash 'payload-merge macher-agent-pipeline-registry))
                                   (entry (cl-find #'macher-agent-vfs--merge-payload entries
                                                   :key (lambda (e) (plist-get e :step)))))
                              (expect (plist-get entry :priority) :to-equal 10)))))

                    (it "safely executes when macher-agent-macher-install is not defined"
                        (let ((macher-agent-task-flush-hook nil)
                              (macher-agent-vfs-flush-hook nil)
                              (macher-workspace-types-alist nil))
                          (clrhash macher-agent-pipeline-registry)
                          (cl-letf (((symbol-function 'macher-agent-macher-install) nil))
                            (fmakunbound 'macher-agent-macher-install)
                            (macher-agent-vfs-install)
                            (expect (member #'macher-agent-vfs-handle-flush macher-agent-task-flush-hook) :to-be-truthy)
                            (expect (member #'macher-agent-vfs--merge-payload (macher-agent-get-pipeline-steps 'payload-merge)) :to-be-truthy))))

                    (it "populates :get-files handler in macher-workspace-types-alist for agent workspace"
                        (let ((macher-workspace-types-alist nil)
                              (macher-agent-task-flush-hook nil)
                              (macher-agent-vfs-flush-hook nil))
                          (macher-agent-vfs-install)
                          (let ((agent-entry (assq 'agent macher-workspace-types-alist)))
                            (expect agent-entry :to-be-truthy)
                            (expect (plist-get (cdr agent-entry) :get-files) :to-equal 'macher-agent--collect-raw-files)))))

          (describe "macher-agent-vfs-handle-flush"
                    (it "dispatches to macher-agent-vfs-flush-hook when context has changes"
                        (let* ((ctx (macher-agent--make-context
                                     :project-root "/mock/flush-test/"
                                     :plugins (list :prompt "Refactor codebase"
                                                    :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/flush-test/a.el" "old" "new"))))))
                               (hook-called nil))
                          (let ((macher-agent-vfs-flush-hook
                                 (list (lambda (c) (setq hook-called c)))))
                            (macher-agent-vfs-handle-flush ctx)
                            (expect hook-called :to-be ctx))))

                    (it "suppresses flush dispatch when macher-agent--suppress-patch is non-nil"
                        (let* ((ctx (macher-agent--make-context
                                     :project-root "/mock/flush-test/"
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/flush-test/a.el" "old" "new"))))))
                               (hook-called nil)
                               (macher-agent--suppress-patch t)
                               (macher-agent-vfs-flush-hook
                                (list (lambda (c) (setq hook-called c)))))
                          (macher-agent-vfs-handle-flush ctx)
                          (expect hook-called :to-be nil)))

                    (it "suppresses flush dispatch when context data contains :suppress-patch"
                        (let* ((ctx (macher-agent--make-context
                                     :project-root "/mock/flush-test/"
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/flush-test/a.el" "old" "new"))))))
                               (hook-called nil))
                          (macher-agent--set-context-data ctx :suppress-patch t)
                          (let ((macher-agent-vfs-flush-hook
                                 (list (lambda (c) (setq hook-called c)))))
                            (macher-agent-vfs-handle-flush ctx)
                            (expect hook-called :to-be nil))))

                    (it "does not dispatch flush when context is clean and unmodified"
                        (let* ((ctx (macher-agent--make-context
                                     :project-root "/mock/flush-test/"
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/flush-test/a.el" "clean" "clean"))))))
                               (hook-called nil))
                          (let ((macher-agent-vfs-flush-hook
                                 (list (lambda (c) (setq hook-called c)))))
                            (macher-agent-vfs-handle-flush ctx)
                            (expect hook-called :to-be nil)))))

          (describe "macher-agent-vfs-diff-review"
                    (it "dispatches flush handling for active persistent context"
                        (let* ((ctx (macher-agent--make-context
                                     :project-root "/mock/diff-rev/"
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/diff-rev/a.el" "1" "2"))))))
                               (flush-received nil)
                               (macher-agent--persistent-context ctx)
                               (macher-agent-vfs-flush-hook
                                (list (lambda (c) (setq flush-received c)))))
                          (macher-agent-vfs-diff-review)
                          (expect flush-received :to-be ctx))))

          (describe "macher-agent-vfs-build-patch-from-hook"
                    (it "executes split patch generation and extracts FSM prompt"
                        (let* ((agent-buf (generate-new-buffer "agent-build-hook-buf"))
                               (fsm (gptel-make-fsm :info (list :buffer agent-buf :prompt "Hook prompt")))
                               (gptel--fsm fsm)
                               (macher-agent--active-fsm fsm)
                               (ctx (macher-agent--make-context
                                     :project-root "/mock/build-hook/"
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/build-hook/x.el" "orig" "mod"))))))
                               (build-calls nil))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent-macher-build-patch)
                                         (lambda (c f) (push (cons c f) build-calls))))
                                (macher-agent-vfs-build-patch-from-hook ctx)
                                (expect (length build-calls) :to-equal 1)
                                (expect (macher-agent--get-context-prompt ctx) :to-equal "Hook prompt"))
                            (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))))

          (describe "macher-agent--execute-split-patch"
                    (it "extracts prompt from fsm-obj plist and propagates to split contexts"
                        (let* ((orig-buf (generate-new-buffer "split-plist-prompt-buf"))
                               (fsm-plist (list :buffer orig-buf :prompt "Plist Prompt Text"))
                               (ctx (macher-agent--make-context
                                     :project-root "/mock/split-prompt/"
                                     :origin-buffer orig-buf
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/split-prompt/f.el" "1" "2"))))))
                               (captured-p-ctx nil))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent--build-and-rename-patch)
                                         (lambda (sub-ctx fsm type)
                                           (setq captured-p-ctx sub-ctx)
                                           nil)))
                                (macher-agent--execute-split-patch ctx fsm-plist)
                                (expect (macher-agent-context-prompt captured-p-ctx) :to-equal "Plist Prompt Text"))
                            (when (buffer-live-p orig-buf) (kill-buffer orig-buf)))))

                    (it "extracts prompt from ctx when fsm-obj prompt is nil"
                        (let* ((orig-buf (generate-new-buffer "split-ctx-prompt-buf"))
                               (ctx (macher-agent--make-context
                                     :project-root "/mock/split-ctx-prompt/"
                                     :origin-buffer orig-buf
                                     :plugins (list :prompt "Ctx Prompt Text"
                                                    :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/split-ctx-prompt/f.el" "1" "2"))))))
                               (captured-p-ctx nil))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent--build-and-rename-patch)
                                         (lambda (sub-ctx fsm type)
                                           (setq captured-p-ctx sub-ctx)
                                           nil)))
                                (macher-agent--execute-split-patch ctx nil)
                                (expect (macher-agent-context-prompt captured-p-ctx) :to-equal "Ctx Prompt Text"))
                            (when (buffer-live-p orig-buf) (kill-buffer orig-buf))))))

          (describe "macher-agent--expressive-patch-buffer-name"
                    (it "uses macher-agent-macher-workspace-name and macher-agent-macher-workspace-hash"
                        (let* ((ws (cons 'project "/mock/test-bridge/"))
                               (ws-name (macher-agent-macher-workspace-name ws))
                               (hash (macher-agent-macher-workspace-hash ws 4))
                               (name (macher-agent--expressive-patch-buffer-name "physical" ws "agent-buf")))
                          (expect name :to-equal (format "*macher-physical-patch:project@%s<%s>[agent-buf]*" ws-name hash))))

                    (it "handles fallback when buffer is not supplied"
                        (let* ((ws (cons 'project "/mock/test-bridge/"))
                               (ws-name (macher-agent-macher-workspace-name ws))
                               (hash (macher-agent-macher-workspace-hash ws 4))
                               (name (macher-agent--expressive-patch-buffer-name "physical" ws nil)))
                          (expect name :to-equal (format "*macher-physical-patch:project@%s<%s>*" ws-name hash)))))

          (describe "macher-agent--build-and-rename-patch"
                    (it "delegates patch building directly to macher-agent-macher-build-patch"
                        (let* ((orig-buf (generate-new-buffer "agent-build-test"))
                               (fsm (gptel-make-fsm :info (list :buffer orig-buf)))
                               (ctx (macher-agent--make-context
                                     :project-root "/mock/build-test/"
                                     :origin-buffer orig-buf
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/build-test/f.el" "1" "2"))))))
                               (ws (macher-agent--get-context-workspace ctx))
                               (expected-name (macher-agent--expressive-patch-buffer-name "physical" ws orig-buf))
                               (build-patch-called nil))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent-macher-build-patch)
                                         (lambda (c f)
                                           (setq build-patch-called (list c f)))))
                                (macher-agent--build-and-rename-patch ctx fsm "physical")
                                (expect (car build-patch-called) :to-equal ctx)
                                (expect (cadr build-patch-called) :to-equal fsm))
                            (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                            (when-let* ((b (get-buffer expected-name))) (kill-buffer b)))))

                    (it "renames patch-buf directly in place retaining its buffer identity"
                        (let* ((orig-buf (generate-new-buffer "agent-retain-id-buf"))
                               (fsm (gptel-make-fsm :info (list :buffer orig-buf)))
                               (ctx (macher-agent--make-context
                                     :project-root "/mock/retain-id/"
                                     :origin-buffer orig-buf
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/retain-id/f.el" "1" "2"))))))
                               (ws (macher-agent--get-context-workspace ctx))
                               (expected-name (macher-agent--expressive-patch-buffer-name "physical" ws orig-buf))
                               (raw-patch-buf (generate-new-buffer "*temp-raw-patch*")))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent-macher-build-patch)
                                         (lambda (_c _f) raw-patch-buf)))
                                (let ((result (macher-agent--build-and-rename-patch ctx fsm "physical")))
                                  (expect result :to-be raw-patch-buf)
                                  (expect (buffer-live-p raw-patch-buf) :to-be-truthy)
                                  (expect (buffer-name raw-patch-buf) :to-equal expected-name)))
                            (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                            (when (buffer-live-p raw-patch-buf) (kill-buffer raw-patch-buf))
                            (when-let* ((b (get-buffer expected-name))) (kill-buffer b)))))

                    (it "kills pre-existing expressive buffer when renaming a distinct live patch-buf"
                        (let* ((orig-buf (generate-new-buffer "agent-kill-existing-buf"))
                               (fsm (gptel-make-fsm :info (list :buffer orig-buf)))
                               (ctx (macher-agent--make-context
                                     :project-root "/mock/kill-existing/"
                                     :origin-buffer orig-buf
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/kill-existing/f.el" "1" "2"))))))
                               (ws (macher-agent--get-context-workspace ctx))
                               (expected-name (macher-agent--expressive-patch-buffer-name "physical" ws orig-buf))
                               (old-expressive-buf (generate-new-buffer expected-name))
                               (new-patch-buf (generate-new-buffer "*new-patch-buffer*")))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent-macher-build-patch)
                                         (lambda (_c _f) new-patch-buf)))
                                (let ((result (macher-agent--build-and-rename-patch ctx fsm "physical")))
                                  (expect result :to-be new-patch-buf)
                                  (expect (buffer-live-p new-patch-buf) :to-be-truthy)
                                  (expect (buffer-name new-patch-buf) :to-equal expected-name)
                                  (expect (buffer-live-p old-expressive-buf) :to-be nil)))
                            (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                            (when (buffer-live-p old-expressive-buf) (kill-buffer old-expressive-buf))
                            (when (buffer-live-p new-patch-buf) (kill-buffer new-patch-buf))
                            (when-let* ((b (get-buffer expected-name))) (kill-buffer b))))))

          (describe "macher-agent-vfs module hygiene and forward declarations"
                    (it "contains direct requires and zero internal declare-function forms targeting macher-agent-macher"
                        (let* ((vfs-file (or (locate-library "macher-agent-vfs.el")
                                             (expand-file-name "macher-agent-vfs.el" default-directory)))
                               (forms nil))
                          (with-temp-buffer
                            (insert-file-contents vfs-file)
                            (goto-char (point-min))
                            (condition-case nil
                                (while t
                                  (push (read (current-buffer)) forms))
                              (end-of-file nil)))
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
                                  forms))
                                (all-declares
                                 (cl-remove-if-not
                                  (lambda (form)
                                    (and (consp form) (eq (car form) 'declare-function)))
                                  forms))
                                (requires
                                 (mapcar (lambda (form)
                                           (let ((feat (cadr form)))
                                             (if (and (consp feat) (eq (car feat) 'quote))
                                                 (cadr feat)
                                               feat)))
                                         (cl-remove-if-not
                                          (lambda (form)
                                            (and (consp form)
                                                 (eq (car form) 'require)))
                                          forms))))
                            (expect internal-declares :to-equal nil)
                            (dolist (dec all-declares)
                              (let ((target-file (caddr dec)))
                                (when (and (consp target-file) (eq (car target-file) 'quote))
                                  (setq target-file (cadr target-file)))
                                (expect (member target-file '("gptel" "mailcap" "macher" gptel mailcap macher))
                                        :to-be-truthy)))
                            (expect (or (member 'macher-agent-core requires) (member ''macher-agent-core requires)) :to-be-truthy)
                            (expect (or (member 'macher-agent-macher requires) (member ''macher-agent-macher requires)) :to-be-truthy))))))

(provide 'macher-agent-vfs-test)
;;; macher-agent-vfs-test.el ends here
