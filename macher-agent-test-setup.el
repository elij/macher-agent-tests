;;; macher-agent-test-setup.el --- Test Setup Adapter -*- lexical-binding: t; -*-

(let* ((this-file (or load-file-name buffer-file-name))
       (this-dir (if this-file
                     (file-name-directory (expand-file-name this-file))
                   (expand-file-name default-directory)))
       (root-dir (or (locate-dominating-file this-dir "macher-agent.el")
                     (locate-dominating-file default-directory "macher-agent.el")
                     this-dir)))
  (when (and root-dir (file-directory-p root-dir))
    (add-to-list 'load-path (file-name-as-directory (expand-file-name root-dir)))
    (let ((macher-dir (expand-file-name "macher" root-dir)))
      (when (file-directory-p macher-dir)
        (add-to-list 'load-path (file-name-as-directory macher-dir))))
    (let ((gptel-dir (expand-file-name "gptel" root-dir)))
      (when (file-directory-p gptel-dir)
        (add-to-list 'load-path (file-name-as-directory gptel-dir))))
    (let ((skills-dir (expand-file-name "skills/scripts" root-dir)))
      (when (file-directory-p skills-dir)
        (add-to-list 'load-path (file-name-as-directory skills-dir)))))
  (add-to-list 'load-path (expand-file-name "tests" root-dir)))

(require 'macher-agent-test-helpers)

(provide 'macher-agent-test-setup)
;;; macher-agent-test-setup.el ends here
