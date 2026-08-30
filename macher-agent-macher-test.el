;;; macher-agent-macher-test.el --- Tests for macher-agent-macher bridge -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'macher-agent-test-setup)
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
      (expect (fboundp 'macher-agent--safe-workspace-hash) :to-be nil)
      (expect (fboundp 'macher-agent--find-active-workspace-in-ancestors) :to-be nil)
      (expect (fboundp 'macher-agent--match-persistent-context) :to-be nil)
      (expect (fboundp 'macher-agent--input-specifies-workspace-p) :to-be nil)
      (expect (fboundp 'macher-agent--get-expanded-root) :to-be nil))

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
              (expect (member target-file '("gptel" "project" "macher" gptel project macher))
                      :to-be-truthy)))))))

  (describe "macher-agent-macher-build-patch and ephemeral translation"
    (it "accepts (ctx fsm) with macher-agent-context and ephemerally instantiates upstream macher-context"
      (let* ((proj-root "/mock/agent-project")
             (files (list (make-macher-agent-vfs-entry :path "main.el" :orig nil :curr "(message \"hello\")")
                          (make-macher-agent-vfs-entry :path "test.el" :orig nil :curr "(message \"test\")")))
             (expected-upstream '(("main.el" nil . "(message \"hello\")") ("test.el" nil . "(message \"test\")")))
             (agent-ctx (make-macher-agent-context
                         :id "test-agent-1"
                         :project-root proj-root
                         :plugins (list :vfs (list :contents files :dirty-p t))))
             (dummy-fsm (make-symbol "mock-fsm"))
             (make-context-calls nil)
             (build-patch-calls nil)
             (dummy-ephemeral-ctx (list :mock-upstream-context t)))
        (cl-letf (((symbol-function 'macher--make-context)
                   (lambda (&rest args)
                     (setq make-context-calls (append make-context-calls (list args)))
                     dummy-ephemeral-ctx))
                  ((symbol-function 'macher--build-patch)
                   (lambda (ctx fsm)
                     (setq build-patch-calls (append build-patch-calls (list (list ctx fsm))))
                     'patch-result-agent-ctx)))
          (let ((result (macher-agent-macher-build-patch agent-ctx dummy-fsm)))
            (expect result :to-equal 'patch-result-agent-ctx)
            (expect (length make-context-calls) :to-equal 1)
            (let ((mc-args (car make-context-calls)))
              (expect (plist-get mc-args :workspace) :to-equal (cons 'agent (expand-file-name proj-root)))
              (expect (plist-get mc-args :contents) :to-equal expected-upstream))
            (expect (length build-patch-calls) :to-equal 1)
            (expect (car build-patch-calls) :to-equal (list dummy-ephemeral-ctx dummy-fsm))))))

    (it "accepts (project-root vfs-files fsm) and ephemerally instantiates upstream macher-context"
      (let* ((proj-root "/mock/project-direct")
             (files (list (make-macher-agent-vfs-entry :path "app.js" :orig nil :curr "console.log('hi');")))
             (expected-upstream '(("app.js" nil . "console.log('hi');")))
             (dummy-fsm (make-symbol "mock-fsm-direct"))
             (make-context-calls nil)
             (build-patch-calls nil)
             (dummy-ephemeral-ctx (list :mock-upstream-context-direct t)))
        (cl-letf (((symbol-function 'macher--make-context)
                   (lambda (&rest args)
                     (setq make-context-calls (append make-context-calls (list args)))
                     dummy-ephemeral-ctx))
                  ((symbol-function 'macher--build-patch)
                   (lambda (ctx fsm)
                     (setq build-patch-calls (append build-patch-calls (list (list ctx fsm))))
                     'patch-result-3-args)))
          (let ((result (macher-agent-macher-build-patch proj-root files dummy-fsm)))
            (expect result :to-equal 'patch-result-3-args)
            (expect (length make-context-calls) :to-equal 1)
            (let ((mc-args (car make-context-calls)))
              (expect (plist-get mc-args :workspace) :to-equal (cons 'agent (expand-file-name proj-root)))
              (expect (plist-get mc-args :contents) :to-equal expected-upstream))
            (expect (length build-patch-calls) :to-equal 1)
            (expect (car build-patch-calls) :to-equal (list dummy-ephemeral-ctx dummy-fsm)))))))

  (describe "macher-agent-macher-workspace-name"
    (it "resolves workspace name via macher--workspace-name when available"
      (let ((ws (cons 'project "/tmp/test-proj")))
        (expect (stringp (macher-agent-macher-workspace-name ws)) :to-be t)))

    (it "handles agent workspace tagged cons cell"
      (let ((ws (cons 'agent (cons 'project "/tmp/test-agent-proj"))))
        (expect (stringp (macher-agent-macher-workspace-name ws)) :to-be t)))

    (it "resolves workspace name cleanly from macher-agent-context"
      (let ((ctx (make-macher-agent-context :project-root "/tmp/my-awesome-agent-project")))
        (expect (macher-agent-macher-workspace-name ctx) :to-equal "my-awesome-agent-project")))

    (it "resolves workspace name cleanly from string root"
      (expect (macher-agent-macher-workspace-name "/tmp/path-to-workspace") :to-equal "path-to-workspace"))

    (it "falls back gracefully when workspace cannot be resolved upstream"
      (expect (macher-agent-macher-workspace-name nil) :to-equal "workspace")
      (expect (macher-agent-macher-workspace-name (cons 'unknown "/tmp/foo/bar")) :to-equal "bar")))

  (describe "macher-agent-macher-safe-workspace-hash and macher-agent-macher-workspace-hash"
    (it "computes deterministic MD5 hash for macher-agent-context"
      (let* ((ctx (make-macher-agent-context :project-root "/path/to/my/project"))
             (hash1 (macher-agent-macher-safe-workspace-hash ctx))
             (hash2 (macher-agent-macher-safe-workspace-hash ctx)))
        (expect (stringp hash1) :to-be t)
        (expect (equal hash1 hash2) :to-be t)
        (expect (length hash1) :to-equal 32)))

    (it "computes deterministic MD5 hash for workspace cons"
      (let* ((ws (cons 'project "/path/to/my/project"))
             (hash1 (macher-agent-macher-safe-workspace-hash ws))
             (hash2 (macher-agent-macher-safe-workspace-hash ws)))
        (expect (stringp hash1) :to-be t)
        (expect (equal hash1 hash2) :to-be t)
        (expect (length hash1) :to-equal 32)))

    (it "handles unwrapping of agent tagged workspace"
      (let* ((ws (cons 'project "/path/to/my/project"))
             (tagged (cons 'agent ws)))
        (expect (macher-agent-macher-safe-workspace-hash tagged)
                :to-equal (macher-agent-macher-safe-workspace-hash ws))))

    (it "respects optional length argument in macher-agent-macher-safe-workspace-hash"
      (let* ((ctx (make-macher-agent-context :project-root "/path/to/my/project"))
             (full-hash (macher-agent-macher-safe-workspace-hash ctx))
             (hash-4 (macher-agent-macher-safe-workspace-hash ctx 4))
             (hash-8 (macher-agent-macher-safe-workspace-hash ctx 8))
             (hash-large (macher-agent-macher-safe-workspace-hash ctx 100))
             (hash-zero (macher-agent-macher-safe-workspace-hash ctx 0))
             (hash-neg (macher-agent-macher-safe-workspace-hash ctx -1)))
        (expect (length hash-4) :to-equal 4)
        (expect hash-4 :to-equal (substring full-hash 0 4))
        (expect (length hash-8) :to-equal 8)
        (expect hash-8 :to-equal (substring full-hash 0 8))
        (expect (length hash-large) :to-equal (length full-hash))
        (expect hash-large :to-equal full-hash)
        (expect hash-zero :to-equal full-hash)
        (expect hash-neg :to-equal full-hash)))

    (it "computes workspace hash with default length 16 and custom length for macher-agent-context"
      (let* ((ctx (make-macher-agent-context :project-root "/path/to/my/project"))
             (hash-default (macher-agent-macher-workspace-hash ctx))
             (hash-4 (macher-agent-macher-workspace-hash ctx 4))
             (hash-8 (macher-agent-macher-workspace-hash ctx 8))
             (hash-large (macher-agent-macher-workspace-hash ctx 100)))
        (expect (length hash-default) :to-equal 16)
        (expect (length hash-4) :to-equal 4)
        (expect (length hash-8) :to-equal 8)
        (expect (<= (length hash-large) 32) :to-be-truthy))))

  (describe "macher-agent-macher-install"
    (it "configures upstream alias, types alist, and workspace functions"
      (let ((macher-workspace-types-alist nil)
            (macher-workspace-functions nil))
        (macher-agent-macher-install)
        (expect (symbol-function 'macher--workspace-hash) :to-equal #'macher-agent-macher-safe-workspace-hash)
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
             (ctx (make-macher-agent-context :project-root proj-root)))
        (expect (macher-agent--get-context-workspace ctx)
                :to-equal (cons 'project (expand-file-name proj-root)))))

    (it "unwraps macher-agent-context cleanly via macher-agent--unwrap-workspace"
      (let* ((proj-root "/tmp/test-proj-dir")
             (ctx (make-macher-agent-context :project-root proj-root)))
        (expect (macher-agent--unwrap-workspace ctx) :to-equal proj-root)))

    (it "generates display name cleanly via macher-agent--get-name for macher-agent-context"
      (let ((ctx (make-macher-agent-context :project-root "/tmp/my-project"))
            (ctx-slash (make-macher-agent-context :project-root "/tmp/my-project/")))
        (expect (macher-agent--get-name ctx) :to-equal "Agent: my-project")
        (expect (macher-agent--get-name ctx-slash) :to-equal "Agent: my-project")))

    (it "retrieves active patch buffer for macher-agent-context"
      (let* ((proj-root "/tmp/patch-proj")
             (ctx (make-macher-agent-context :project-root proj-root))
             (called-ws nil))
        (cl-letf (((symbol-function 'macher-patch-buffer)
                   (lambda (ws)
                     (setq called-ws ws)
                     'mock-patch-buffer)))
          (expect (macher-agent-macher-patch-buffer ctx) :to-equal 'mock-patch-buffer)
          (expect called-ws :to-equal (cons 'project (expand-file-name proj-root)))))))

  (describe "Context lookup and resolution pipeline without obsolete steps"
    (it "deterministically resolves macher-agent-context via macher-agent-context-lookup"
      (let* ((ctx (make-macher-agent-context :project-root "/mock/lookup/project"))
             (buf (generate-new-buffer "test-ctx-buf")))
        (unwind-protect
            (progn
              (expect (macher-agent-context-lookup ctx) :to-be ctx)
              (with-current-buffer buf
                (setq-local macher-agent--persistent-context ctx))
              (expect (macher-agent-context-lookup buf) :to-be ctx)
              (cl-letf (((symbol-function 'macher-agent-resolve-context)
                         (lambda (&optional input) (and (equal input 'custom-ref) ctx))))
                (expect (macher-agent-context-lookup 'custom-ref) :to-be ctx)
                (expect (macher-agent-context-lookup 'unknown-ref) :to-be nil)))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))

    (it "installs clean context-resolution pipeline steps and excludes obsolete steps"
      (clrhash macher-agent-pipeline-registry)
      (macher-agent-context-resolution-install)
      (let ((steps (macher-agent-get-pipeline-steps 'context-resolution)))
        (expect (member #'macher-agent-ctx-pipe--explicit steps) :to-be-truthy)
        (expect (member #'macher-agent-ctx-pipe--buffer steps) :to-be-truthy)
        (expect (member #'macher-agent-ctx-pipe--fsm steps) :to-be-truthy)
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
            (kill-buffer buf)))))

    (it "resolves context in pipeline step macher-agent-ctx-pipe--fsm"
      (let* ((ctx (make-macher-agent-context :project-root "/mock/fsm-proj"))
             (dummy-fsm (make-symbol "mock-pipeline-fsm")))
        (cl-letf (((symbol-function 'macher-agent--extract-fsm-context)
                   (lambda (fsm) (when (eq fsm dummy-fsm) ctx))))
          (let* ((state (list :input dummy-fsm :resolved nil))
                 (updated (macher-agent-ctx-pipe--fsm state)))
            (expect (plist-get updated :resolved) :to-be ctx))))))

  (describe "Context mutation, data/prompt setters, and cloning with macher-agent-context"
    (it "safely mutates data and prompt on macher-agent-context"
      (let ((ctx (make-macher-agent-context :project-root "/tmp/test")))
        (macher-agent--set-context-data ctx :vfs (list :contents (list (make-macher-agent-vfs-entry :path "a.el" :orig nil :curr "1"))))
        (expect (plist-get (macher-agent-context-plugins ctx) :vfs) :to-equal (list :contents (list (make-macher-agent-vfs-entry :path "a.el" :orig nil :curr "1"))))
        (macher-agent--set-context-prompt ctx "new agent prompt")
        (expect (plist-get (macher-agent-context-plugins ctx) :prompt) :to-equal "new agent prompt")))

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
