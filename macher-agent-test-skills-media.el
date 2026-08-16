;;; macher-agent-test-skills-media.el --- Skills media tests -*- lexical-binding: t; -*-

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

(describe "Skills Media Support"
          (macher-agent-test-setup-before-each)

          (it "detects media file extensions"
              (expect (macher-agent-media-file-p "image.png") :to-be-truthy)
              (expect (macher-agent-media-file-p "photo.jpg") :to-be-truthy)
              (expect (macher-agent-media-file-p "code.el") :to-be nil)))

(provide 'macher-agent-test-skills-media)
;;; macher-agent-test-skills-media.el ends here
