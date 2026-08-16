;;; macher-agent-test-setup.el --- Shared Setup for Macher-Agent Tests -*- lexical-binding: t; -*-

(let* ((file (or load-file-name buffer-file-name))
       (test-dir (cond
                  (file (file-name-directory (expand-file-name file)))
                  ((file-exists-p (expand-file-name "helpers" default-directory))
                   (expand-file-name default-directory))
                  ((file-exists-p (expand-file-name "tests/helpers" default-directory))
                   (expand-file-name "tests" default-directory))
                  (t (or (locate-dominating-file default-directory "tests") default-directory))))
       (root-dir (locate-dominating-file (or file default-directory) "macher-agent.el")))
  (when root-dir
    (add-to-list 'load-path (expand-file-name root-dir)))
  (add-to-list 'load-path (expand-file-name test-dir))
  (add-to-list 'load-path (expand-file-name "helpers" test-dir)))

(require 'subr-x)
(require 'buttercup)
(require 'macher-agent-macher)
(require 'macher-agent)
(require 'macher-agent-vfs)
(require 'macher-agent-gptel)
(require 'macher-agent-orchestration)
(require 'macher-agent-test-harness)

(defvar gptel--fsm)
(defvar macher-agent--active-fsm)
(defvar gptel--fsm-last)

(defmacro macher-agent-test-setup-before-each ()
  `(before-each
    (spy-on 'macher-action)
    (spy-on 'gptel-send)
    (spy-on 'macher--add-termination-handler)
    (setq macher-agent--persistent-context nil)
    (let* ((ctx (ignore-errors (macher-agent-resolve-context)))
           (ws (when ctx (macher-agent--get-context-workspace ctx))))
      (when ws (setf (macher-agent-workspace-active-subagents ws) nil)))))

(provide 'macher-agent-test-setup)
;;; macher-agent-test-setup.el ends here