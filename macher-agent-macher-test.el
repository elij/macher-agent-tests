;;; macher-agent-macher-test.el --- Tests for macher-agent-macher bridge -*- lexical-binding: t; -*-

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
(require 'cl-lib)
(require 'macher-agent-test-setup)
(require 'macher-agent)
(require 'macher-agent-core)
(require 'macher-agent-vfs)
(require 'macher-agent-macher)

(describe "Macher-Agent Macher bridge suite"
  (macher-agent-test-setup-before-each)

  (describe "Bridge function definitions and dead code removal"
    (it "provides required bridge functions and removes dead code"
      (expect (fboundp 'macher-agent-macher-build-patch) :to-be t)
      (expect (fboundp 'macher-agent-macher-patch-buffer) :to-be t)
      (expect (fboundp 'macher-agent-macher-workspace-name) :to-be t)
      (expect (fboundp 'macher-agent-macher-workspace-hash) :to-be t)
      (expect (fboundp 'macher-agent-macher-safe-workspace-hash) :to-be t)
      (expect (fboundp 'macher-agent-macher-install) :to-be t)
      (expect (fboundp 'macher-agent-context-resolution-install) :to-be t)
      (expect (fboundp 'macher-agent-context-lookup) :to-be t)
      (expect (fboundp 'macher-agent-trigger-patch) :to-be t)
      (expect (fboundp 'macher-agent-apply-patch) :to-be t)
      (expect (fboundp 'macher-agent-macher-register-workspace-type) :to-be nil)
      (expect (fboundp 'macher-agent-macher-build-patch-from-vfs) :to-be nil)
      (expect (fboundp 'macher-agent--create-and-tag-vfs-context) :to-be nil)
      (expect (fboundp 'macher-agent--def-context-accessor) :to-be nil)
      (expect (fboundp 'macher-agent--with-protected-context-contents) :to-be nil)
      (expect (fboundp 'macher-agent--context-p) :to-be nil)
      (expect (fboundp 'macher-agent-macher-run-with-context) :to-be nil)
      (expect (fboundp 'macher-agent-ctx-pipe--canonical) :to-be nil)
      (expect (fboundp 'macher-agent-ctx-pipe--workspace-id) :to-be nil)
      (expect (fboundp 'macher-agent-ctx-pipe--subagent) :to-be nil)
      (expect (fboundp 'macher-agent-ctx-pipe--fsm-fallback) :to-be nil)
      (expect (fboundp 'macher-agent-ctx-pipe--fsm) :to-be nil)
      (expect (fboundp 'macher-agent--get-fsm-latest) :to-be nil)
      (expect (fboundp 'macher-agent--inject-context-into-fsm-info) :to-be nil)
      (expect (fboundp 'macher-agent--safe-workspace-hash) :to-be nil)
      (expect (fboundp 'macher-agent--find-active-workspace-in-ancestors) :to-be nil)
      (expect (fboundp 'macher-agent--match-persistent-context) :to-be nil)
      (expect (fboundp 'macher-agent--input-specifies-workspace-p) :to-be nil)
      (expect (fboundp 'macher-agent--get-expanded-root) :to-be nil)
      (expect (fboundp 'macher-agent--get-context-data) :to-be nil)
      (expect (fboundp 'macher-agent--set-context-data) :to-be nil)
      (expect (fboundp 'macher-agent--get-context-prompt) :to-be nil)
      (expect (fboundp 'macher-agent--set-context-prompt) :to-be nil))

    (it "contains zero calls to obsolete context helpers in macher-agent-macher.el"
      (let* ((file (or (locate-library "macher-agent-macher.el")
                       (expand-file-name "macher-agent-macher.el" default-directory)))
             (content (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string))))
        (expect (string-match-p "macher-agent--get-context-data" content) :to-be nil)
        (expect (string-match-p "macher-agent--set-context-data" content) :to-be nil)
        (expect (string-match-p "macher-agent--get-context-prompt" content) :to-be nil)
        (expect (string-match-p "macher-agent--set-context-prompt" content) :to-be nil)
        (expect (string-match-p "macher-agent--get-context-workspace" content) :to-be nil)))

    (it "contains zero internal declare-function forms in macher-agent-macher.el"
      (let* ((file (or (locate-library "macher-agent-macher.el")
                       (expand-file-name "macher-agent-macher.el" default-directory)))
             (forms nil))
        (with-temp-buffer
          (insert-file-contents file)
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
                forms)))
          (expect internal-declares :to-equal nil)
          (dolist (dec all-declares)
            (let ((target-file (caddr dec)))
              (when (and (consp target-file) (eq (car target-file) 'quote))
                (setq target-file (cadr target-file)))
              (expect (member target-file '("project" "macher" project macher))
                      :to-be-truthy)))))))

  (describe "macher-agent-macher-build-patch and ephemeral translation"
    (it "passes canonical relative paths to make-macher-context fallback"
      (let* ((proj-root "/mock/fallback-project")
             (canonical-root (file-truename (expand-file-name proj-root)))
             (abs-file (expand-file-name "pkg/feature.el" canonical-root))
             (files (list (make-macher-agent-vfs-entry :path abs-file :orig "a" :curr "b")))
             (expected-upstream '(("pkg/feature.el" "a" . "b")))
             (agent-ctx (make-macher-agent-context :project-root proj-root))
             (make-macher-context-calls nil))
        (cl-letf (((symbol-function 'macher--make-context) nil)
                  ((symbol-function 'make-macher-context)
                   (lambda (&rest args)
                     (setq make-macher-context-calls (append make-macher-context-calls (list args)))
                     (list :mock-context-fallback t)))
                  ((symbol-function 'macher--build-patch)
                   (lambda (_c _f) 'patch-fallback-ok)))
          (let ((res (macher-agent-macher-build-patch agent-ctx "Fallback test" files)))
            (expect res :to-equal 'patch-fallback-ok)
            (expect (length make-macher-context-calls) :to-equal 1)
            (let ((mc-args (car make-macher-context-calls)))
              (expect (plist-get mc-args :workspace) :to-equal (cons 'project canonical-root))
              (expect (plist-get mc-args :contents) :to-equal expected-upstream)
              (expect (plist-get mc-args :prompt) :to-equal "Fallback test")))))))

  (describe "macher-agent-macher-workspace-name"
    (it "resolves workspace name cleanly from macher-agent-context"
      (let ((ctx (make-macher-agent-context :project-root "/tmp/my-awesome-agent-project")))
        (expect (macher-agent-macher-workspace-name ctx) :to-equal "my-awesome-agent-project")))

    (it "resolves workspace name when context has workspace plugin"
      (let ((ctx (make-macher-agent-context
                  :plugins '(:workspace (project . "/tmp/test-proj")))))
        (expect (macher-agent-macher-workspace-name ctx) :to-equal "workspace")))

    (it "falls back gracefully to workspace when project-root is nil"
      (let ((ctx (make-macher-agent-context)))
        (expect (macher-agent-macher-workspace-name ctx) :to-equal "workspace"))))

  (describe "macher-agent-macher-safe-workspace-hash and macher-agent-macher-workspace-hash"
    (it "computes deterministic hash for macher-agent-context"
      (let* ((ctx (make-macher-agent-context :project-root "/path/to/my/project"))
             (hash1 (macher-agent-macher-safe-workspace-hash ctx))
             (hash2 (macher-agent-macher-safe-workspace-hash ctx)))
        (expect (stringp hash1) :to-be t)
        (expect (equal hash1 hash2) :to-be t)
        (expect (length hash1) :to-equal 16)))

    (it "signals wrong-type-argument when passed non-context"
      (let ((ws (cons 'project "/path/to/my/project")))
        (expect (macher-agent-macher-safe-workspace-hash ws) :to-throw 'wrong-type-argument)))

    (it "respects optional length argument in macher-agent-macher-safe-workspace-hash"
      (let* ((ctx (make-macher-agent-context :project-root "/path/to/my/project"))
             (full-hash (macher-agent-macher-safe-workspace-hash ctx))
             (hash-4 (macher-agent-macher-safe-workspace-hash ctx 4))
             (hash-8 (macher-agent-macher-safe-workspace-hash ctx 8))
             (hash-32 (macher-agent-macher-safe-workspace-hash ctx 32)))
        (expect (length hash-4) :to-equal 4)
        (expect hash-4 :to-equal (substring full-hash 0 4))
        (expect (length hash-8) :to-equal 8)
        (expect hash-8 :to-equal (substring full-hash 0 8))
        (expect (length hash-32) :to-equal 32)
        (expect (substring hash-32 0 16) :to-equal full-hash)))

    (it "computes workspace hash with default length 16 and custom length for workspace cons"
      (let* ((ws (cons 'project "/path/to/my/project"))
             (hash-default (macher-agent-macher-workspace-hash ws))
             (hash-4 (macher-agent-macher-workspace-hash ws 4))
             (hash-8 (macher-agent-macher-workspace-hash ws 8))
             (hash-large (macher-agent-macher-workspace-hash ws 100)))
        (expect (length hash-default) :to-equal 16)
        (expect (length hash-4) :to-equal 4)
        (expect (length hash-8) :to-equal 8)
        (expect (<= (length hash-large) 32) :to-be-truthy))))

  (describe "macher-agent-macher-install"
    (it "configures upstream types alist and workspace functions without overriding macher--workspace-hash"
      (let ((macher-workspace-types-alist nil)
            (macher-workspace-functions nil))
        (macher-agent-macher-install)
        (expect (assq 'agent macher-workspace-types-alist) :to-be-truthy)
        (let ((entry (assq 'agent macher-workspace-types-alist)))
          (expect (plist-get (cdr entry) :get-root) :to-equal 'macher-agent-workspace-project-root)
          (expect (plist-get (cdr entry) :get-name) :to-equal 'macher-agent--get-name)
          (expect (plist-get (cdr entry) :get-files) :to-equal 'macher-agent--collect-raw-files))
        (expect (member #'macher-agent-workspace-agent macher-workspace-functions) :to-be-truthy)))

    (it "merges agent workspace type without overwriting other properties"
      (let ((macher-workspace-types-alist
             (list (cons 'agent (list :get-files 'my-custom-files-fn))))
            (macher-workspace-functions nil))
        (macher-agent-macher-install)
        (let ((entry (assq 'agent macher-workspace-types-alist)))
          (expect entry :to-be-truthy)
          (expect (plist-get (cdr entry) :get-root) :to-equal 'macher-agent-workspace-project-root)
          (expect (plist-get (cdr entry) :get-name) :to-equal 'macher-agent--get-name)
          (expect (plist-get (cdr entry) :get-files) :to-equal 'my-custom-files-fn)))))

  (describe "Workspace tagging, unwrapping, and accessors with macher-agent-context"
    (it "retrieves tagged workspace structure from macher-agent-context"
      (let* ((proj-root "/tmp/test-proj-dir")
             (ws-cons (cons 'project (expand-file-name proj-root)))
             (ctx (make-macher-agent-context :project-root proj-root
                                             :plugins (list :workspace ws-cons))))
        (expect (macher-agent-context-workspace ctx)
                :to-equal ws-cons)))

    (it "unwraps macher-agent-context cleanly via macher-agent--unwrap-workspace"
      (let* ((proj-root "/tmp/test-proj-dir")
             (ctx (make-macher-agent-context :project-root proj-root)))
        (expect (macher-agent--unwrap-workspace ctx) :to-equal proj-root)))

    (it "generates display name cleanly via macher-agent--get-name for macher-agent-context"
      (let ((ctx (make-macher-agent-context :project-root "/tmp/my-project"))
            (ctx-slash (make-macher-agent-context :project-root "/tmp/my-project/")))
        (expect (macher-agent--get-name ctx) :to-equal "Agent: my-project")
        (expect (macher-agent--get-name ctx-slash) :to-equal "Agent: my-project")))

    (it "retrieves active patch buffer for upstream workspace cons"
      (let* ((proj-root "/tmp/patch-proj")
             (ws-cons (cons 'project (expand-file-name proj-root)))
             (called-ws nil))
        (cl-letf (((symbol-function 'macher-patch-buffer)
                   (lambda (ws &optional _live)
                     (setq called-ws ws)
                     'mock-patch-buffer)))
          (expect (macher-agent-macher-patch-buffer ws-cons) :to-equal 'mock-patch-buffer)
          (expect called-ws :to-equal ws-cons)))))

  (describe "Context lookup and resolution pipeline without obsolete steps"
    (it "deterministically resolves macher-agent-context via macher-agent-context-lookup"
      (let* ((ctx (make-macher-agent-context :project-root "/mock/lookup/project"))
             (buf (generate-new-buffer "test-ctx-buf")))
        (unwind-protect
            (progn
              (puthash (expand-file-name "/mock/lookup/project") ctx macher-agent-active-workspaces)
              (expect (macher-agent-context-lookup (expand-file-name "/mock/lookup/project")) :to-be ctx)
              (with-current-buffer buf
                (setq-local macher-agent--persistent-context ctx))
              (expect (macher-agent-context-lookup (buffer-name buf)) :to-be ctx)
              (expect (macher-agent-context-from-buffer buf) :to-be ctx)
              (expect (macher-agent-context-lookup "unknown-ref") :to-be nil)
              (expect (macher-agent-context-lookup ctx) :to-throw 'wrong-type-argument))
          (remhash (expand-file-name "/mock/lookup/project") macher-agent-active-workspaces)
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "installs clean context-resolution pipeline steps and excludes obsolete steps"
      (clrhash macher-agent-pipeline-registry)
      (macher-agent-context-resolution-install)
      (let ((steps (macher-agent-get-pipeline-steps 'context-resolution)))
        (expect (member #'macher-agent-ctx-pipe--explicit steps) :to-be-truthy)
        (expect (member #'macher-agent-ctx-pipe--buffer steps) :to-be-truthy)
        (expect (member #'macher-agent-resolve-from-transit-payload steps) :to-be-truthy)
        (expect (member 'macher-agent-ctx-pipe--fsm steps) :to-be nil)
        (expect (member 'macher-agent-ctx-pipe--fsm-fallback steps) :to-be nil)
        (expect (member #'macher-agent-ctx-pipe--lazy-init steps) :to-be-truthy)))

    (it "resolves context in pipeline step macher-agent-ctx-pipe--explicit"
      (let* ((ctx (make-macher-agent-context :project-root "/mock/pipeline-proj"))
             (state (list :input ctx :resolved nil))
             (updated (macher-agent-ctx-pipe--explicit state)))
        (expect (plist-get updated :resolved) :to-be ctx)))

    (it "resolves context in pipeline step macher-agent-ctx-pipe--buffer"
      (let* ((ctx (make-macher-agent-context :project-root "/mock/buf-proj"))
             (buf (generate-new-buffer "test-pipe-buf")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (setq-local macher-agent--persistent-context ctx))
              (let* ((state (list :input buf :resolved nil))
                     (updated (macher-agent-ctx-pipe--buffer state)))
                (expect (plist-get updated :resolved) :to-be ctx)))
          (when (buffer-live-p buf)
            (kill-buffer buf))))))

  (describe "Context mutation, data/prompt setters, and cloning with macher-agent-context"
    (it "safely mutates data and prompt on macher-agent-context"
      (let ((ctx (make-macher-agent-context :project-root "/tmp/test")))
        (setf (macher-agent-context-plugins ctx)
              (list :vfs (list :contents (list (make-macher-agent-vfs-entry :path "a.el" :orig nil :curr "1")))))
        (expect (plist-get (macher-agent-context-plugins ctx) :vfs)
                :to-equal (list :contents (list (make-macher-agent-vfs-entry :path "a.el" :orig nil :curr "1"))))
        (setf (macher-agent-context-prompt ctx) "new agent prompt")
        (expect (macher-agent-context-prompt ctx) :to-equal "new agent prompt")))

    (it "clones macher-agent-context preserving plugins, vfs, and hash-tables"
      (let* ((orig (make-macher-agent-context
                    :id "orig-1"
                    :project-root "/mock/clone-proj"
                    :plugins (list :vfs (list :contents (list (make-macher-agent-vfs-entry :path "f1" :orig nil :curr "v1")) :dirty-p t)
                                   :vfs-buffers (make-hash-table :test 'equal)
                                   :mtime-tracker (make-hash-table :test 'equal))))
             (cloned (macher-agent--clone-context orig)))
        (expect (macher-agent-context-p cloned) :to-be t)
        (expect (eq cloned orig) :to-be nil)
        (expect (macher-agent-context-id cloned) :to-equal "orig-1")
        (expect (macher-agent-context-project-root cloned) :to-equal "/mock/clone-proj")
        (expect (plist-get (plist-get (macher-agent-context-plugins cloned) :vfs) :contents)
                :to-equal (list (make-macher-agent-vfs-entry :path "f1" :orig nil :curr "v1")))
        (expect (hash-table-p (plist-get (macher-agent-context-plugins cloned) :vfs-buffers)) :to-be t)
        (expect (eq (plist-get (macher-agent-context-plugins cloned) :vfs-buffers)
                    (plist-get (macher-agent-context-plugins orig) :vfs-buffers))
                :to-be nil)))

    (it "manages vfs-buffers and mtime-tracker hash-tables on macher-agent-context"
      (let ((ctx (make-macher-agent-context :project-root "/tmp/test")))
        (let ((vfs-ht (macher-agent-workspace-vfs-buffers ctx))
              (mtime-ht (macher-agent-workspace-mtime-tracker ctx)))
          (expect (hash-table-p vfs-ht) :to-be t)
          (expect (hash-table-p mtime-ht) :to-be t)
          (expect (macher-agent-workspace-vfs-buffers ctx) :to-equal vfs-ht)
          (expect (macher-agent-workspace-mtime-tracker ctx) :to-equal mtime-ht))))

    (it "manages workspace active subagents and skills on macher-agent-context"
      (let ((ctx (make-macher-agent-context :project-root "/tmp/test")))
        (expect (macher-agent-workspace-active-subagents ctx) :to-be nil)
        (setf (macher-agent-workspace-active-subagents ctx) '("sub-1" "sub-2"))
        (expect (macher-agent-workspace-active-subagents ctx) :to-equal '("sub-1" "sub-2"))
        (setf (macher-agent-workspace-skills-alist ctx) '((skill-1 . "desc1")))
        (expect (macher-agent-workspace-skills-alist ctx) :to-equal '((skill-1 . "desc1")))))))

(provide 'macher-agent-macher-test)
;;; macher-agent-macher-test.el ends here
