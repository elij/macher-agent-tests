;;; macher-agent-audit-log-test.el --- Tests for macher-agent audit logging -*- lexical-binding: t; -*-

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
(require 'json)
(require 'cl-lib)
(require 'macher-agent-test-setup)
(require 'macher-agent)
(require 'macher-agent-test-harness)

(load (expand-file-name "skills/scripts/read_context_audit_log.el") nil t)

(describe
 "Macher-Agent Audit Logging"
 
 (describe
  "macher-agent-log-tool-intent"
  (it "appends entries with timestamp buffer preset type target args into context data audit log"
      (let ((ctx (macher-agent--make-context))
            (macher-agent--active-skill-sym 'alpha-preset))
        (macher-agent-log-tool-intent ctx "gptel-tool" "tool-one" '(:key "value-one"))
        (let* ((log (plist-get (macher-agent-context-plugins ctx) :audit-log))
               (entry (car log)))
          (expect (length log) :to-equal 1)
          (expect (cdr (assoc 'buffer entry)) :to-equal (buffer-name))
          (expect (cdr (assoc 'preset entry)) :to-equal "alpha-preset")
          (expect (cdr (assoc 'type entry)) :to-equal "gptel-tool")
          (expect (cdr (assoc 'target entry)) :to-equal "tool-one")
          (expect (cdr (assoc 'args entry)) :to-equal '(:key "value-one"))
          (expect (stringp (cdr (assoc 'timestamp entry))) :to-be t))
        (macher-agent-log-tool-intent ctx "gptel-tool" "tool-two" '(:key "value-two"))
        (expect (length (plist-get (macher-agent-context-plugins ctx) :audit-log)) :to-equal 2))))

 (describe "macher-agent--log-gptel-pre-tool"
           (it "logs standard gptel tool calls into active context audit log"
               (let ((ctx (macher-agent--make-context)))
                 (setq-local macher-agent--persistent-context ctx)
                 (macher-agent--log-gptel-pre-tool 'test-gptel-tool nil :param "val")
                 (let* ((log (plist-get (macher-agent-context-plugins ctx) :audit-log))
                        (entry (car log)))
                   (expect (length log) :to-equal 1)
                   (expect (cdr (assoc 'type entry)) :to-equal "gptel-tool")
                   (expect (cdr (assoc 'target entry)) :to-equal "test-gptel-tool")
                   (expect (cdr (assoc 'args entry)) :to-equal '(:param "val"))))))

 (describe "macher-agent-sandbox-run"
           (it "logs PTC tool yields into active context audit log"
               (let ((ctx (macher-agent--make-context))
                     (buf (current-buffer))
                     (macher-agent--active-ptc-primitives '(test-ptc-tool)))
                 (macher-agent-sandbox-run '(test-ptc-tool :path "file.txt") nil ctx buf)
                 (let* ((log (plist-get (macher-agent-context-plugins ctx) :audit-log))
                        (entry (car log)))
                   (expect (length log) :to-equal 1)
                   (expect (cdr (assoc 'type entry)) :to-equal "ptc")
                   (expect (cdr (assoc 'target entry)) :to-equal 'test-ptc-tool)
                   (expect (cdr (assoc 'args entry)) :to-equal '(:path "file.txt")))))

           (it "logs PTC tool yields into explicitly passed context"
               (let ((ctx (macher-agent--make-context))
                     (buf (current-buffer))
                     (macher-agent--active-ptc-primitives '(custom-ptc-tool)))
                 (macher-agent-sandbox-run '(custom-ptc-tool :name "foo" :count 42) nil ctx buf)
                 (let* ((log (plist-get (macher-agent-context-plugins ctx) :audit-log))
                        (entry (car log)))
                   (expect (length log) :to-equal 1)
                   (expect (cdr (assoc 'type entry)) :to-equal "ptc")
                   (expect (cdr (assoc 'target entry)) :to-equal 'custom-ptc-tool)
                   (expect (cdr (assoc 'args entry)) :to-equal '(:name "foo" :count 42)))))

           (it "does not log audit entries for non-tool expressions"
               (let ((ctx (macher-agent--make-context))
                     (buf (current-buffer))
                     (macher-agent--active-ptc-primitives '(test-ptc-tool)))
                 (macher-agent-sandbox-run '(+ 1 2) '(+) ctx buf)
                 (let ((log (plist-get (macher-agent-context-plugins ctx) :audit-log)))
                   (expect (length log) :to-equal 0))))))

(provide 'macher-agent-audit-log-test)
;;; macher-agent-audit-log-test.el ends here
