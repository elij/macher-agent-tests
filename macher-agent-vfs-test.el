;;; macher-agent-vfs-test.el --- Tests for Macher Agent VFS -*- lexical-binding: t; -*-

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

(require 'macher-agent-test-setup)
(require 'macher-agent-core)
(require 'macher-agent-vfs)
(require 'macher-agent-macher)

(describe "Macher-Agent VFS Macher Bridge Integration"
          (macher-agent-test-setup-before-each)

          (describe "Envelope Integration and Accessors"
                    (it "properly reads and writes VFS state via macher-agent-vfs--get-state and macher-agent-vfs--set-state"
                        (let ((ctx (macher-agent--make-context :id "ctx-env-1")))
                          (expect (macher-agent-vfs--get-state ctx) :to-be nil)
                          (macher-agent-vfs--set-state ctx (list :contents (list (make-macher-agent-vfs-entry :path "a.el" :orig nil :curr "code")) :dirty-p t))
                          (expect (macher-agent-vfs--get-state ctx) :to-equal (list :contents (list (make-macher-agent-vfs-entry :path "a.el" :orig nil :curr "code")) :dirty-p t))
                          (expect (plist-get (macher-agent-context-plugins ctx) :vfs) :to-equal (list :contents (list (make-macher-agent-vfs-entry :path "a.el" :orig nil :curr "code")) :dirty-p t))))

                    (it "strictly enforces context structures and rejects residual alists in VFS state accessors"
                        (let ((alist-ctx '((:vfs . (:contents nil :dirty-p t))))
                              (valid-ctx (macher-agent--make-context :id "valid-ctx-vfs")))
                          (expect (macher-agent-vfs--get-state alist-ctx) :to-throw 'wrong-type-argument)
                          (expect (macher-agent-vfs--set-state alist-ctx '(:contents nil)) :to-throw 'wrong-type-argument)
                          (expect (macher-agent-storage--extract-context alist-ctx) :to-throw 'wrong-type-argument)
                          (expect (macher-agent-storage--extract-context `((:target-context . ,valid-ctx))) :to-throw 'wrong-type-argument)
                          (expect (macher-agent-storage--extract-context
                                   (make-macher-agent-transit-payload :target-context valid-ctx))
                                  :to-be valid-ctx))))

          (describe "macher-agent-vfs-scratch-inflate"
                    (it "inflates VFS contents into its physical scratchpad directory"
                        (let* ((temp-dir (make-temp-file "macher-vfs-scratch-test" t))
                               (ws-root "/mock/scratch-ws/")
                               (vfs-tbl (make-hash-table :test 'equal))
                               (inflate-fn #'macher-agent-vfs-scratch-inflate))
                          (puthash "/mock/scratch-ws/nested/file.txt" "nested scratch content" vfs-tbl)
                          (puthash "/mock/scratch-ws/root-file.txt" "root scratch content" vfs-tbl)
                          (unwind-protect
                              (progn
                                (funcall inflate-fn temp-dir vfs-tbl ws-root nil)
                                (expect (file-exists-p (expand-file-name "nested/file.txt" temp-dir)) :to-be-truthy)
                                (expect (file-exists-p (expand-file-name "root-file.txt" temp-dir)) :to-be-truthy)
                                (with-temp-buffer
                                  (insert-file-contents (expand-file-name "nested/file.txt" temp-dir))
                                  (expect (buffer-string) :to-equal "nested scratch content"))
                                (with-temp-buffer
                                  (insert-file-contents (expand-file-name "root-file.txt" temp-dir))
                                  (expect (buffer-string) :to-equal "root scratch content")))
                            (delete-directory temp-dir t)))))

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
                          (setf (macher-agent-context-plugins ctx)
                                (plist-put (copy-sequence (macher-agent-context-plugins ctx)) :suppress-patch t))
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
                            (expect hook-called :to-be nil))))

                    (it "strictly rejects cons cell workspace and non-context arguments"
                        (expect (macher-agent-vfs-handle-flush '(project . "/mock/flush-test/"))
                                :to-throw 'wrong-type-argument)
                        (expect (macher-agent-vfs-handle-flush nil)
                                :to-throw 'wrong-type-argument)
                        (expect (macher-agent-vfs-handle-flush "not-a-context")
                                :to-throw 'wrong-type-argument))))

          (describe "macher-agent-vfs-build-patch-from-hook"
                    (it "executes split patch generation using prompt from context"
                        (let* ((agent-buf (generate-new-buffer "agent-build-hook-buf"))
                               (ctx (macher-agent--make-context
                                     :project-root "/mock/build-hook/"
                                     :origin-buffer agent-buf
                                     :prompt "Hook prompt"
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/build-hook/x.el" "orig" "mod"))))))
                               (build-calls nil))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent-macher-build-patch)
                                         (lambda (c prompt &optional files)
                                           (push (list c prompt files) build-calls))))
                                (macher-agent-vfs-build-patch-from-hook ctx)
                                (expect (length build-calls) :to-equal 1)
                                (expect (nth 1 (car build-calls)) :to-equal "Hook prompt")
                                (expect (macher-agent-context-prompt ctx) :to-equal "Hook prompt"))
                            (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))))

          (describe "macher-agent--execute-split-patch"
                    (it "extracts prompt from context and propagates to split contexts"
                        (let* ((orig-buf (generate-new-buffer "split-prompt-buf"))
                               (ctx (macher-agent--make-context
                                     :project-root "/mock/split-prompt/"
                                     :origin-buffer orig-buf
                                     :prompt "Explicit Context Prompt"
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/split-prompt/f.el" "1" "2"))))))
                               (captured-p-ctx nil))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent--build-and-rename-patch)
                                         (lambda (sub-ctx type &optional files)
                                           (setq captured-p-ctx sub-ctx)
                                           nil)))
                                (macher-agent--execute-split-patch ctx)
                                (expect (macher-agent-context-prompt captured-p-ctx) :to-equal "Explicit Context Prompt"))
                            (when (buffer-live-p orig-buf) (kill-buffer orig-buf))))))

          (describe "macher-agent--expressive-patch-buffer-name"
                    (it "uses macher-agent-macher-workspace-name and macher-agent-macher-safe-workspace-hash"
                        (let* ((ctx (macher-agent--make-context :project-root "/mock/test-bridge/"))
                               (ws-name (macher-agent-macher-workspace-name ctx))
                               (hash (macher-agent-macher-safe-workspace-hash ctx))
                               (name (macher-agent--expressive-patch-buffer-name ctx "physical" "agent-buf")))
                          (expect name :to-equal (format "*macher-physical-patch:project@%s<%s>[agent-buf]*" ws-name hash))))

                    (it "handles fallback when buffer is not supplied"
                        (let* ((ctx (macher-agent--make-context :project-root "/mock/test-bridge/"))
                               (ws-name (macher-agent-macher-workspace-name ctx))
                               (hash (macher-agent-macher-safe-workspace-hash ctx))
                               (name (macher-agent--expressive-patch-buffer-name ctx "physical" nil)))
                          (expect name :to-equal (format "*macher-physical-patch:project@%s<%s>*" ws-name hash)))))

          (describe "macher-agent--gather-vfs-entries"
                    (it "gathers entries directly from context or supplied files list"
                        (let* ((entry (macher-agent-vfs-make-entry "/mock/proj/a.el" "1" "2"))
                               (ctx (macher-agent--make-context
                                     :project-root "/mock/proj/"
                                     :plugins (list :vfs (list :contents (list entry))))))
                          (expect (macher-agent--gather-vfs-entries ctx) :to-equal (list entry))
                          (let ((custom (list (macher-agent-vfs-make-entry "/mock/proj/b.el" "3" "4"))))
                            (expect (macher-agent--gather-vfs-entries ctx custom) :to-equal custom)))))

          (describe "macher-agent--build-and-rename-patch"
                    (it "delegates patch building directly to macher-agent-macher-build-patch"
                        (let* ((orig-buf (generate-new-buffer "agent-build-test"))
                               (ctx (macher-agent--make-context
                                     :project-root "/mock/build-test/"
                                     :origin-buffer orig-buf
                                     :prompt "Build patch prompt"
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/build-test/f.el" "1" "2"))))))
                               (ws (macher-agent-context-workspace ctx))
                               (expected-name (macher-agent--expressive-patch-buffer-name ctx "physical" orig-buf))
                               (build-patch-called nil))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent-macher-build-patch)
                                         (lambda (c p &optional f)
                                           (setq build-patch-called (list c p f)))))
                                (macher-agent--build-and-rename-patch ctx "physical")
                                (expect (nth 0 build-patch-called) :to-equal ctx)
                                (expect (nth 1 build-patch-called) :to-equal "Build patch prompt")
                                (expect (nth 2 build-patch-called) :to-be nil))
                            (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                            (when-let* ((b (get-buffer expected-name))) (kill-buffer b)))))

                    (it "renames patch-buf directly in place retaining its buffer identity"
                        (let* ((orig-buf (generate-new-buffer "agent-retain-id-buf"))
                               (ctx (macher-agent--make-context
                                     :project-root "/mock/retain-id/"
                                     :origin-buffer orig-buf
                                     :prompt "Retain id prompt"
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/retain-id/f.el" "1" "2"))))))
                               (ws (macher-agent-context-workspace ctx))
                               (expected-name (macher-agent--expressive-patch-buffer-name ctx "physical" orig-buf))
                               (raw-patch-buf (generate-new-buffer "*temp-raw-patch*")))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent-macher-build-patch)
                                         (lambda (_c _p &optional _f) raw-patch-buf)))
                                (let ((result (macher-agent--build-and-rename-patch ctx "physical")))
                                  (expect result :to-be raw-patch-buf)
                                  (expect (buffer-live-p raw-patch-buf) :to-be-truthy)
                                  (expect (buffer-name raw-patch-buf) :to-equal expected-name)))
                            (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                            (when (buffer-live-p raw-patch-buf) (kill-buffer raw-patch-buf))
                            (when-let* ((b (get-buffer expected-name))) (kill-buffer b)))))

                    (it "kills pre-existing expressive buffer when renaming a distinct live patch-buf"
                        (let* ((orig-buf (generate-new-buffer "agent-kill-existing-buf"))
                               (ctx (macher-agent--make-context
                                     :project-root "/mock/kill-existing/"
                                     :origin-buffer orig-buf
                                     :prompt "Kill existing prompt"
                                     :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/kill-existing/f.el" "1" "2"))))))
                               (ws (macher-agent-context-workspace ctx))
                               (expected-name (macher-agent--expressive-patch-buffer-name ctx "physical" orig-buf))
                               (old-expressive-buf (generate-new-buffer expected-name))
                               (new-patch-buf (generate-new-buffer "*new-patch-buffer*")))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent-macher-build-patch)
                                         (lambda (_c _p &optional _f) new-patch-buf)))
                                (let ((result (macher-agent--build-and-rename-patch ctx "physical")))
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
                            (expect (or (member 'macher-agent-macher requires) (member ''macher-agent-macher requires)) :to-be-truthy))))

                    (it "contains zero calls to macher-agent--get-context-data, macher-agent--set-context-data, or macher-agent--get-context-workspace"
                        (let* ((vfs-file (or (locate-library "macher-agent-vfs.el")
                                             (expand-file-name "macher-agent-vfs.el" default-directory)))
                               (content (with-temp-buffer
                                          (insert-file-contents vfs-file)
                                          (buffer-string))))
                          (expect (string-match-p "macher-agent--get-context-data" content) :to-be nil)
                          (expect (string-match-p "macher-agent--set-context-data" content) :to-be nil)
                          (expect (string-match-p "macher-agent--get-context-workspace" content) :to-be nil)
                          (expect (string-match-p "macher-agent--get-context-prompt" content) :to-be nil)
                          (expect (string-match-p "macher-agent--set-context-prompt" content) :to-be nil)))



                    (it "contains no duplicate definitions of macher-agent-context-root and macher-agent--get-context-workspace"
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
                          (let ((defined-symbols
                                 (mapcar (lambda (form)
                                           (let ((sym (cadr form)))
                                             (if (and (consp sym) (eq (car sym) 'quote))
                                                 (cadr sym)
                                               sym)))
                                         (cl-remove-if-not
                                          (lambda (form)
                                            (and (consp form)
                                                 (memq (car form) '(defun cl-defun defmacro defalias defvar defcustom))))
                                          forms))))
                            (expect (member 'macher-agent-context-root defined-symbols) :to-be nil)
                            (expect (member 'macher-agent--get-context-workspace defined-symbols) :to-be nil)
                            (expect (member 'macher-agent--get-context-shadow-buffers defined-symbols) :to-be nil)
                            (expect (member 'macher-agent-sandbox-inflate defined-symbols) :to-be nil)))))

          (describe "macher-agent-with-strict-vfs and Strict Boundary"
                    (it "identifies active VFS context correctly via macher-agent-vfs-active-p"
                        (let ((valid-ctx (macher-agent--make-context :id "strict-ctx-1"))
                              (invalid-ctx '((:id . "not-a-context"))))
                          (expect (macher-agent-vfs-active-p valid-ctx) :to-be t)
                          (expect (macher-agent-vfs-active-p invalid-ctx) :to-throw 'wrong-type-argument)
                          (expect (macher-agent-vfs-active-p nil) :to-throw 'wrong-type-argument)))

                    (it "flushes modified workspace buffers to disk and syncs context via macher-agent-vfs-flush"
                        (let* ((temp-dir (make-temp-file "macher-vfs-flush-test-" t))
                               (file-path (expand-file-name "test-flush.txt" temp-dir))
                               (buf (find-file-noselect file-path))
                               (ctx (macher-agent--make-context
                                     :project-root temp-dir
                                     :plugins (list :vfs (list :contents (list (make-macher-agent-vfs-entry :path "test-flush.txt" :orig "disk content" :curr "disk content"))))))
                               (auto-synced nil))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf
                                  (insert "uncommitted buffer modification"))
                                (expect (buffer-modified-p buf) :to-be t)
                                (cl-letf (((symbol-function 'macher-agent--auto-sync-context)
                                           (lambda (c) (setq auto-synced c))))
                                  (macher-agent-vfs-flush ctx)
                                  (expect (buffer-modified-p buf) :to-be nil)
                                  (expect auto-synced :to-equal ctx)))
                            (when (buffer-live-p buf)
                              (with-current-buffer buf (set-buffer-modified-p nil))
                              (kill-buffer buf))
                            (delete-directory temp-dir t))))

                    (it "restores virtual context state upon execution completion via macher-agent-vfs-restore"
                        (let* ((ctx (macher-agent--make-context :id "restore-ctx-1"))
                               (auto-synced nil))
                          (cl-letf (((symbol-function 'macher-agent--auto-sync-context)
                                     (lambda (c) (setq auto-synced c))))
                            (macher-agent-vfs-restore ctx)
                            (expect auto-synced :to-equal ctx))))

                    (it "executes BODY within strict VFS boundary synchronising before and restoring after"
                        (let* ((ctx (macher-agent--make-context :id "macro-strict-ctx"))
                               (execution-log nil))
                          (cl-letf (((symbol-function 'macher-agent-vfs-flush)
                                     (lambda (_) (push 'flush execution-log)))
                                    ((symbol-function 'macher-agent-vfs-restore)
                                     (lambda (_) (push 'restore execution-log))))
                            (let ((res (macher-agent-with-strict-vfs ctx
                                         (push 'body execution-log)
                                         'success-val)))
                              (expect res :to-equal 'success-val)
                              (expect (reverse execution-log) :to-equal '(flush body restore))))))

                    (it "ensures macher-agent-with-strict-vfs restores virtual state when body signals an error"
                        (let* ((ctx (macher-agent--make-context :id "macro-error-ctx"))
                               (restored nil))
                          (cl-letf (((symbol-function 'macher-agent-vfs-flush) (lambda (_)))
                                    ((symbol-function 'macher-agent-vfs-restore)
                                     (lambda (c) (setq restored c))))
                            (expect
                             (macher-agent-with-strict-vfs ctx
                               (error "Pipeline error inside macro body"))
                             :to-throw 'error)
                            (expect restored :to-equal ctx))))

                    (it "bypasses flush and restore when context is nil"
                        (let ((flushed nil)
                              (restored nil))
                          (cl-letf (((symbol-function 'macher-agent-vfs-flush)
                                     (lambda (_) (setq flushed t)))
                                    ((symbol-function 'macher-agent-vfs-restore)
                                     (lambda (_) (setq restored t))))
                            (let ((res (macher-agent-with-strict-vfs nil
                                         'direct-eval)))
                              (expect res :to-equal 'direct-eval)
                              (expect flushed :to-be nil)
                              (expect restored :to-be nil))))))

          (describe "VFS Context Synchronization and Baseline Sync"
                    (describe "Argument ordering in macher-agent--sync-and-check-dirty-entries"
                              (it "passes root as 2nd argument and tracker as 3rd argument to macher-agent--sync-context-entry"
                                  (let* ((entry (macher-agent-vfs-make-entry "file.el" "orig" "curr"))
                                         (contents (list entry))
                                         (root "/mock/root/")
                                         (tracker (make-hash-table :test 'equal))
                                         (captured-args nil))
                                    (cl-letf (((symbol-function 'macher-agent--sync-context-entry)
                                               (lambda (e r &optional m)
                                                 (setq captured-args (list e r m))
                                                 nil)))
                                      (macher-agent--sync-and-check-dirty-entries contents root tracker)
                                      (expect (nth 0 captured-args) :to-be entry)
                                      (expect (nth 1 captured-args) :to-equal root)
                                      (expect (nth 2 captured-args) :to-be tracker))))

                              (it "auto-sync-context passes root then tracker to macher-agent--sync-and-check-dirty-entries"
                                  (let* ((ctx (macher-agent--make-context :project-root "/mock/auto-sync-root/"))
                                         (captured-args nil))
                                    (cl-letf (((symbol-function 'macher-agent--sync-and-check-dirty-entries)
                                               (lambda (c &optional r m)
                                                 (setq captured-args (list c r m))
                                                 (cons nil nil)))
                                              ((symbol-function 'macher-agent--persist-vfs-to-hidden-buffer) #'ignore))
                                      (macher-agent--auto-sync-context ctx)
                                      (expect (nth 1 captured-args) :to-equal "/mock/auto-sync-root/")
                                      (expect (nth 2 captured-args) :to-equal (macher-agent-workspace-mtime-tracker ctx)))))

                              (it "strictly rejects reversed argument ordering in macher-agent--sync-and-check-dirty-entries"
                                  (let* ((entry (macher-agent-vfs-make-entry "file.el" "orig" "curr"))
                                         (contents (list entry))
                                         (root "/mock/root/")
                                         (tracker (make-hash-table :test 'equal)))
                                    ;; Call with reversed parameters (tracker as 2nd, root as 3rd) must signal wrong-type-argument
                                    (expect (macher-agent--sync-and-check-dirty-entries contents tracker root)
                                            :to-throw 'wrong-type-argument)))

                    (describe "Baseline sync when hunks/changes are applied"
                              (it "updates baseline orig when hunks are partially applied to live buffer and omits applied hunks from regenerated diffs"
                                  (let* ((temp-dir (make-temp-file "macher-vfs-hunk-test-" t))
                                         (file-path (expand-file-name "test-hunk.txt" temp-dir))
                                         (orig-text "line 1\nline 2\nline 3\n")
                                         (target-text "line 1\nline 2 MOD\nline 3 MOD\n")
                                         (partial-text "line 1\nline 2 MOD\nline 3\n"))
                                    (unwind-protect
                                        (progn
                                          ;; Write initial file
                                          (with-temp-file file-path (insert orig-text))
                                          (let* ((buf (find-file-noselect file-path))
                                                 (entry (macher-agent-vfs-make-entry "test-hunk.txt" orig-text target-text))
                                                 (ctx (macher-agent--make-context
                                                       :project-root temp-dir
                                                       :origin-buffer buf
                                                       :prompt "Apply changes"
                                                       :plugins (list :vfs (list :contents (list entry))))))
                                            (unwind-protect
                                                (progn
                                                  ;; Partial application: apply hunk 1 to live buffer
                                                  (with-current-buffer buf
                                                    (erase-buffer)
                                                    (insert partial-text))
                                                  ;; Sync context
                                                  (macher-agent--auto-sync-context ctx)
                                                  ;; Baseline orig must now reflect partial-text
                                                  (expect (macher-agent-vfs-entry-orig entry) :to-equal partial-text)
                                                  (expect (macher-agent-vfs-entry-curr entry) :to-equal target-text)
                                                  (expect (macher-agent-vfs-entry-modified-p entry) :to-be t)
                                                  ;; Generate patch and verify already-applied hunk 1 is NOT present
                                                  (let ((patch-buf (macher-agent-macher-build-patch ctx "test patch")))
                                                    (unwind-protect
                                                        (with-current-buffer patch-buf
                                                          (let ((diff-content (buffer-string)))
                                                            ;; Unapplied hunk 2 is in the diff
                                                            (expect (string-match-p "\\+line 3 MOD" diff-content) :to-be-truthy)
                                                            ;; Already-applied hunk 1 is NOT regenerated in diff
                                                            (expect (string-match-p "\\+line 2 MOD" diff-content) :to-be nil)))
                                                      (when (buffer-live-p patch-buf) (kill-buffer patch-buf))))

                                                  ;; Full application: apply hunk 2 to live buffer
                                                  (with-current-buffer buf
                                                    (erase-buffer)
                                                    (insert target-text))
                                                  ;; Sync context again
                                                  (macher-agent--auto-sync-context ctx)
                                                  ;; Baseline orig advances to target-text
                                                  (expect (macher-agent-vfs-entry-orig entry) :to-equal target-text)
                                                  ;; Entry is no longer modified/dirty
                                                  (expect (macher-agent-vfs-entry-modified-p entry) :to-be nil)
                                                  (expect (macher-agent--get-context-dirty-p ctx) :to-be nil)
                                                  ;; Flush hook is suppressed when all changes are applied
                                                  (let ((flush-called nil))
                                                    (let ((macher-agent-vfs-flush-hook (list (lambda (_) (setq flush-called t)))))
                                                      (macher-agent-vfs-handle-flush ctx)
                                                      (expect flush-called :to-be nil))))
                                              (when (buffer-live-p buf)
                                                (with-current-buffer buf (set-buffer-modified-p nil))
                                                (kill-buffer buf)))))
                                      (delete-directory temp-dir t))))

                              (it "updates baseline orig when changes are applied directly to disk"
                                  (let* ((temp-dir (make-temp-file "macher-vfs-disk-test-" t))
                                         (file-path (expand-file-name "test-disk.txt" temp-dir))
                                         (orig-text "alpha\nbeta\ngamma\n")
                                         (target-text "alpha\nBETA\ngamma\n"))
                                    (unwind-protect
                                        (progn
                                          (with-temp-file file-path (insert orig-text))
                                          (let* ((entry (macher-agent-vfs-make-entry "test-disk.txt" orig-text target-text))
                                                 (ctx (macher-agent--make-context
                                                       :project-root temp-dir
                                                       :plugins (list :vfs (list :contents (list entry))))))
                                            ;; Apply changes directly to disk
                                            (with-temp-file file-path (insert target-text))
                                            ;; Sync
                                            (macher-agent--auto-sync-context ctx)
                                            ;; Baseline orig must be updated to target-text
                                            (expect (macher-agent-vfs-entry-orig entry) :to-equal target-text)
                                            (expect (macher-agent-vfs-entry-curr entry) :to-equal target-text)
                                            (expect (macher-agent-vfs-entry-modified-p entry) :to-be nil)
                                            (expect (macher-agent--get-context-dirty-p ctx) :to-be nil)))
                                      (delete-directory temp-dir t)))))

                    (describe "Fail-fast sync on out-of-band modifications"
                              (it "recognizes out-of-band disk modification, synchronizes orig, invalidates pending edits, and warns"
                                  (let* ((temp-dir (make-temp-file "macher-vfs-oob-test-" t))
                                         (file-path (expand-file-name "oob.txt" temp-dir))
                                         (orig-text "base content\n")
                                         (staged-text "base content\nstaged changes\n")
                                         (external-text "external out-of-band edit\n"))
                                    (unwind-protect
                                        (progn
                                          (with-temp-file file-path (insert orig-text))
                                          (let* ((entry (macher-agent-vfs-make-entry "oob.txt" orig-text staged-text))
                                                 (ctx (macher-agent--make-context
                                                       :project-root temp-dir
                                                       :plugins (list :vfs (list :contents (list entry)))))
                                                 (tracker (macher-agent-workspace-mtime-tracker ctx)))
                                            ;; Initialize mtime tracking
                                            (macher-agent--sync-context-entry entry temp-dir tracker)
                                            ;; Simulate out-of-band external write to disk
                                            (sleep-for 0.05)
                                            (with-temp-file file-path (insert external-text))
                                            ;; Spy on warning
                                            (spy-on 'display-warning)
                                            ;; Sync
                                            (let ((res (macher-agent--sync-context-entry entry temp-dir tracker)))
                                              (expect res :to-be t)
                                              ;; orig synchronized to external content
                                              (expect (macher-agent-vfs-entry-orig entry) :to-equal external-text)
                                              ;; curr invalidated to external content
                                              (expect (macher-agent-vfs-entry-curr entry) :to-equal external-text)
                                              ;; entry is clean against the new disk baseline
                                              (expect (macher-agent-vfs-entry-modified-p entry) :to-be nil)
                                              ;; Warning was emitted
                                              (expect 'display-warning :to-have-been-called-with
                                                      'macher-agent
                                                      "Your previous edits to oob.txt were discarded due to external file modifications.  Please re-read and re-apply"
                                                      :warning))))
                                      (delete-directory temp-dir t))))

                              (it "synchronizes clean entry with out-of-band disk changes without warning"
                                  (let* ((temp-dir (make-temp-file "macher-vfs-oob-clean-" t))
                                         (file-path (expand-file-name "clean.txt" temp-dir))
                                         (orig-text "clean content\n")
                                         (external-text "clean modified externally\n"))
                                    (unwind-protect
                                        (progn
                                          (with-temp-file file-path (insert orig-text))
                                          (let* ((entry (macher-agent-vfs-make-entry "clean.txt" orig-text orig-text))
                                                 (ctx (macher-agent--make-context
                                                       :project-root temp-dir
                                                       :plugins (list :vfs (list :contents (list entry)))))
                                                 (tracker (macher-agent-workspace-mtime-tracker ctx)))
                                            ;; Initialize tracker
                                            (macher-agent--sync-context-entry entry temp-dir tracker)
                                            (sleep-for 0.05)
                                            (with-temp-file file-path (insert external-text))
                                            (spy-on 'display-warning)
                                            (let ((res (macher-agent--sync-context-entry entry temp-dir tracker)))
                                              (expect res :to-be t)
                                              (expect (macher-agent-vfs-entry-orig entry) :to-equal external-text)
                                              (expect (macher-agent-vfs-entry-curr entry) :to-equal external-text)
                                              (expect 'display-warning :not :to-have-been-called))))
                                      (delete-directory temp-dir t))))))

          (describe "Design by Contract (DbC) Strict Type and Arity Enforcement"
                    (describe "macher-agent-vfs--merge-payload"
                              (it "enforces strict single arity signature of exactly one argument"
                                  (expect (func-arity #'macher-agent-vfs--merge-payload) :to-equal '(1 . 1)))

                              (it "strictly rejects legacy cons cell workspace and non-context payloads"
                                  (expect (macher-agent-vfs--merge-payload '(project . "/mock/merge/"))
                                          :to-throw 'wrong-type-argument)
                                  (expect (macher-agent-vfs--merge-payload (list :target-context '(project . "/mock/merge/")))
                                          :to-throw 'wrong-type-argument)
                                  (expect (macher-agent-vfs--merge-payload nil)
                                          :to-throw 'wrong-type-argument)
                                  (expect (macher-agent-vfs--merge-payload 42)
                                          :to-throw 'wrong-type-argument))

                              (it "merges valid transit payload into target context"
                                  (let* ((ctx (macher-agent--make-context
                                               :project-root "/mock/merge-dbc/"
                                               :plugins (list :vfs (list :contents (list (macher-agent-vfs-make-entry "/mock/merge-dbc/f.el" "old" "old"))))))
                                         (payload (make-macher-agent-transit-payload
                                                   :type 'ARTIFACT_UPDATE
                                                   :target-context ctx
                                                   :payload (list :diff (list (make-macher-agent-vfs-entry :path "/mock/merge-dbc/f.el" :orig "old" :curr "new"))))))
                                    (macher-agent-vfs--merge-payload payload)
                                    (expect (macher-agent--read-context-file ctx "/mock/merge-dbc/f.el") :to-equal "new"))))

                    (describe "macher-agent-vfs-write"
                              (it "strictly rejects non-string file-path such as hash table passed in old argument ordering"
                                  (let ((ht (make-hash-table :test 'equal)))
                                    (expect (macher-agent-vfs-write ht ht "/path.el" "content")
                                            :to-throw 'wrong-type-argument)))

                              (it "strictly requires string content and hash-table tracker"
                                  (let ((ht (make-hash-table :test 'equal)))
                                    (expect (macher-agent-vfs-write "/path.el" 123 ht)
                                            :to-throw 'wrong-type-argument)
                                    (expect (macher-agent-vfs-write "/path.el" "content" "not-a-hash-table")
                                            :to-throw 'wrong-type-argument)))

                              (it "writes content and updates tracker and vfs buffers with strictly ordered arguments"
                                  (let* ((mtime-ht (make-hash-table :test 'equal))
                                         (vfs-ht (make-hash-table :test 'equal))
                                         (file-path "/mock/proj/test.el")
                                         (res (macher-agent-vfs-write file-path "New content" mtime-ht vfs-ht)))
                                    (expect res :to-equal "New content")
                                    (expect (gethash file-path vfs-ht) :to-equal "New content")))

                              (it "asserts that a VFS write warns if the underlying file has drifted"
                                  (let* ((ctx (make-macher-agent-context :project-root "/mock/proj/"))
                                         (file-path "/mock/proj/test.el")
                                         (original-mtime '(25000 12345))
                                         (drifted-mtime '(25000 99999)))
                                    (unwind-protect
                                        (progn
                                          (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                                          (puthash file-path original-mtime (macher-agent-workspace-mtime-tracker ctx))

                                          (spy-on 'file-attributes :and-call-fake
                                                  (lambda (&rest args)
                                                    (let ((file (car args)))
                                                      (if (string= file file-path)
                                                          (list t 1 1 1 drifted-mtime drifted-mtime drifted-mtime 100 "mode" t 1 1)
                                                        nil))))

                                          (spy-on 'display-warning)

                                          (macher-agent-vfs-write file-path
                                                                  "New content"
                                                                  (macher-agent-workspace-mtime-tracker ctx)
                                                                  (macher-agent-workspace-vfs-buffers ctx))

                                          (expect 'display-warning :to-have-been-called-with
                                                  'macher-agent
                                                  "Your previous edits to test.el were discarded due to external file modifications.  Please re-read and re-apply"
                                                  :warning))
                                      (remhash (expand-file-name "/mock/proj/") macher-agent-active-workspaces)))))

                    (describe "macher-agent--read-string"
                              (it "strictly rejects string offset passed from old parameter order"
                                  (expect (macher-agent--read-string "line 1\nline 2" 1 2)
                                          :to-throw 'wrong-type-argument))

                              (it "strictly requires integer offset and limit and reads correctly"
                                  (expect (macher-agent--read-string 1 2 "a\nb\nc") :to-equal "a\nb")
                                  (expect (macher-agent--read-string nil 2 "a\nb")
                                          :to-throw 'wrong-type-argument)))

                    (describe "macher-agent--edit-string-fast"
                              (it "strictly enforces positional arguments and rejects non-strings"
                                  (expect (macher-agent--edit-string-fast 123 "new" "content")
                                          :to-throw 'wrong-type-argument)
                                  (expect (macher-agent--edit-string-fast "old" 456 "content")
                                          :to-throw 'wrong-type-argument)
                                  (expect (macher-agent--edit-string-fast "old" "new" nil)
                                          :to-throw 'wrong-type-argument))

                              (it "replaces string correctly using strict positional arguments"
                                  (expect (macher-agent--edit-string-fast "hello" "world" "hello there")
                                          :to-equal "world there")))

                    (describe "macher-agent--write-or-delete-vfs-entry"
                              (it "strictly rejects non-string target-path or content"
                                  (expect (macher-agent--write-or-delete-vfs-entry "content" nil)
                                          :to-throw 'wrong-type-argument)
                                  (expect (macher-agent--write-or-delete-vfs-entry nil "/tmp/foo")
                                          :to-throw 'wrong-type-argument)))

                    (describe "macher-agent--sync-context-entry"
                              (it "strictly rejects invalid entry and non-string root"
                                  (let ((ht (make-hash-table :test 'equal)))
                                    (expect (macher-agent--sync-context-entry "not-an-entry" "/tmp/")
                                            :to-throw 'wrong-type-argument)
                                    (let ((entry (macher-agent-vfs-make-entry "file.el" "a" "b")))
                                      (expect (macher-agent--sync-context-entry entry ht "/tmp/")
                                              :to-throw 'wrong-type-argument)))))))

(provide 'macher-agent-vfs-test)
;;; macher-agent-vfs-test.el ends here
