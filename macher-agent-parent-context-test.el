;;; macher-agent-parent-context-test.el --- Tests for Parent Context Resolution -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Buttercup test suite for parent buffer and parent context resolution
;; in `macher-agent-zero-mem.el' and orchestration pipelines.

;;; Code:

(let* ((file (or load-file-name buffer-file-name))
       (root-dir (locate-dominating-file (or file default-directory) "macher-agent.el"))
       (test-dir (cond
                  ((and file (file-exists-p (expand-file-name "macher-agent-test-setup.el" (file-name-directory (expand-file-name file)))))
                   (file-name-directory (expand-file-name file)))
                  ((file-exists-p (expand-file-name "macher-agent-test-setup.el" default-directory))
                   (expand-file-name default-directory))
                  ((file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" default-directory))
                   (expand-file-name "tests" default-directory))
                  ((and root-dir (file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" root-dir)))
                   (expand-file-name "tests" root-dir))
                  (t (or (locate-dominating-file default-directory "tests") default-directory)))))
  (when root-dir
    (add-to-list 'load-path (expand-file-name root-dir)))
  (add-to-list 'load-path (expand-file-name test-dir))
  (add-to-list 'load-path (expand-file-name "helpers" test-dir)))

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-test-setup)
(require 'macher-agent)
(require 'macher-agent-zero-mem)
(require 'macher-agent-gptel)
(require 'macher-agent-orchestration)

(describe "Parent Context and Buffer Resolution Suite"
  (macher-agent-test-setup-before-each)

  (it "spec 1: resolves parent buffer from macher-agent-context struct origin-buffer slot"
    (let* ((parent-buf (generate-new-buffer "*test-parent-origin*"))
           (child-buf (generate-new-buffer "*test-child-origin*"))
           (ctx (make-macher-agent-context :id "ctx-origin" :origin-buffer parent-buf)))
      (unwind-protect
          (progn
            (expect (macher-agent-zero-mem--resolve-parent-buffer ctx) :to-equal parent-buf)
            (expect (macher-agent-zero-mem--resolve-parent-buffer child-buf ctx) :to-equal parent-buf)
            (expect (macher-agent-zero-mem--resolve-parent-buffer ctx child-buf) :to-equal parent-buf))
        (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
        (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

  (it "spec 2: resolves parent buffer from macher-agent-context plugins :origin-buffer and :originator-buffer"
    (let* ((parent-buf1 (generate-new-buffer "*test-parent-plugin-orig*"))
           (parent-buf2 (generate-new-buffer "*test-parent-plugin-origtr*"))
           (ctx1 (make-macher-agent-context :id "ctx-p1" :plugins (list :origin-buffer parent-buf1)))
           (ctx2 (make-macher-agent-context :id "ctx-p2" :plugins (list :originator-buffer parent-buf2))))
      (unwind-protect
          (progn
            (expect (macher-agent-zero-mem--resolve-parent-buffer ctx1) :to-equal parent-buf1)
            (expect (macher-agent-zero-mem--resolve-parent-buffer ctx2) :to-equal parent-buf2))
        (when (buffer-live-p parent-buf1) (kill-buffer parent-buf1))
        (when (buffer-live-p parent-buf2) (kill-buffer parent-buf2)))))

  (it "spec 3: resolves parent buffer from routing stack frame with :parent-buffer and :originator-name"
    (let* ((parent-buf (generate-new-buffer "*test-parent-plugin-name*"))
           (child-buf (generate-new-buffer "*test-child-plugin-name*")))
      (unwind-protect
          (progn
            (with-current-buffer child-buf
              (setq-local macher-agent--routing-stack
                          (list (list :originator-name (buffer-name parent-buf))))
              (expect (macher-agent-zero-mem--resolve-parent-buffer child-buf) :to-equal parent-buf)
              (setq-local macher-agent--routing-stack
                          (list (list :parent-buffer (buffer-name parent-buf))))
              (expect (macher-agent-zero-mem--resolve-parent-buffer child-buf) :to-equal parent-buf)
              (setq-local macher-agent--routing-stack
                          (list (list :origin-buffer parent-buf)))
              (expect (macher-agent-zero-mem--resolve-parent-buffer child-buf) :to-equal parent-buf)))
        (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
        (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

  (it "spec 4: resolves parent buffer when context or target buffer is passed in any argument position"
    (let* ((parent-buf (generate-new-buffer "*test-parent-plist-arg*"))
           (child-buf (generate-new-buffer "*test-child-plist-arg*"))
           (ctx (make-macher-agent-context :id "ctx-pos" :origin-buffer parent-buf)))
      (unwind-protect
          (progn
            (expect (macher-agent-zero-mem--resolve-parent-buffer child-buf ctx) :to-equal parent-buf)
            (expect (macher-agent-zero-mem--resolve-parent-buffer ctx child-buf) :to-equal parent-buf)
            (with-current-buffer child-buf
              (setq-local macher-agent--routing-stack (list parent-buf)))
            (expect (macher-agent-zero-mem--resolve-parent-buffer (buffer-name child-buf) nil) :to-equal parent-buf)
            (expect (macher-agent-zero-mem--resolve-parent-buffer nil (buffer-name child-buf)) :to-equal parent-buf))
        (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
        (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

  (it "spec 5: resolves parent buffer from routing stack frame containing buffer object"
    (let* ((parent-buf (generate-new-buffer "*test-parent-stack-buf*"))
           (child-buf (generate-new-buffer "*test-child-stack-buf*")))
      (unwind-protect
          (progn
            (with-current-buffer child-buf
              (setq-local macher-agent--routing-stack (list parent-buf))
              (expect (macher-agent-zero-mem--resolve-parent-buffer nil) :to-equal parent-buf))
            (expect (macher-agent-zero-mem--resolve-parent-buffer child-buf nil) :to-equal parent-buf))
        (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
        (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

  (it "spec 6: resolves parent buffer from routing stack frame containing buffer name string"
    (let* ((parent-buf (generate-new-buffer "*test-parent-stack-name*"))
           (child-buf (generate-new-buffer "*test-child-stack-name*")))
      (unwind-protect
          (progn
            (with-current-buffer child-buf
              (macher-agent--push-routing "task-test-6" (buffer-name parent-buf)))
            (expect (macher-agent-zero-mem--resolve-parent-buffer child-buf nil) :to-equal parent-buf)
            (expect (macher-agent-zero-mem--resolve-parent-buffer nil child-buf) :to-equal parent-buf))
        (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
        (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

  (it "spec 7: resolves parent buffer from routing stack frame with :originator-buffer plist"
    (let* ((parent-buf (generate-new-buffer "*test-parent-stack-orig*"))
           (child-buf (generate-new-buffer "*test-child-stack-orig*")))
      (unwind-protect
          (progn
            (with-current-buffer child-buf
              (setq-local macher-agent--routing-stack
                          (list (list :task-id "task-test-7" :originator-buffer parent-buf))))
            (expect (macher-agent-zero-mem--resolve-parent-buffer child-buf nil) :to-equal parent-buf))
        (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
        (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

  (it "spec 8: resolves parent buffer from context passed as argument"
    (let* ((parent-buf (generate-new-buffer "*test-parent-local-ctx*"))
           (child-buf (generate-new-buffer "*test-child-local-ctx*"))
           (child-ctx (make-macher-agent-context :id "ctx-child-loc" :origin-buffer parent-buf)))
      (unwind-protect
          (progn
            (expect (macher-agent-zero-mem--resolve-parent-buffer child-buf child-ctx) :to-equal parent-buf)
            (expect (macher-agent-zero-mem--resolve-parent-buffer child-ctx child-buf) :to-equal parent-buf))
        (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
        (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

  (it "spec 9: returns nil safely when parent buffer is killed or when routing stack is unbound"
    (let* ((parent-buf (generate-new-buffer "*test-parent-dead*"))
           (parent-name (buffer-name parent-buf))
           (child-buf (generate-new-buffer "*test-child-dead*"))
           (saved-stack (when (boundp 'macher-agent--routing-stack)
                          (default-value 'macher-agent--routing-stack))))
      (kill-buffer parent-buf)
      (unwind-protect
          (progn
            (with-current-buffer child-buf
              (macher-agent--push-routing "task-test-9" parent-name))
            (expect (macher-agent-zero-mem--resolve-parent-buffer child-buf nil) :to-be nil)
            (makunbound 'macher-agent--routing-stack)
            (expect (macher-agent-zero-mem--resolve-parent-buffer child-buf nil) :to-be nil))
        (when (buffer-live-p child-buf) (kill-buffer child-buf))
        (when saved-stack
          (set-default 'macher-agent--routing-stack saved-stack)))))

  (it "spec 10: injects parent context directive when resolving parent buffer in transmission pipeline"
    (let* ((parent-buf (generate-new-buffer "*test-parent-pipeline*"))
           (child-buf (generate-new-buffer "*test-child-pipeline*")))
      (unwind-protect
          (progn
            (with-current-buffer parent-buf
              (insert "Step 1: Parent orchestrator configured Postgres Database.\nStep 2: Parent orchestrator set up Redis Cluster caching.\n"))
            (with-current-buffer child-buf
              (macher-agent--push-routing "task-test-10" (buffer-name parent-buf))
              (insert "Inspect Postgres Database configuration."))
            (let* ((state (make-macher-agent-transmission-state
                           :target-buffer child-buf
                           :directives nil))
                   (updated-state (macher-agent-pipe--inject-parent-context state))
                   (dirs (macher-agent-transmission-state-directives updated-state)))
              (expect (length dirs) :to-be-greater-than 0)
              (let ((full-text (string-join dirs "\n\n")))
                (expect full-text :to-match "<parent_conversation_context>")
                (expect full-text :to-match "</parent_conversation_context>")
                (expect full-text :to-match "Postgres Database"))))
        (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
        (when (buffer-live-p child-buf) (kill-buffer child-buf))))))

(provide 'macher-agent-parent-context-test)
;;; macher-agent-parent-context-test.el ends here
