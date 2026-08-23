;;; macher-agent-skills-media-test.el --- Skills media test suite -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))
(require 'buttercup)
(require 'macher-agent-test-setup)

(describe "Macher-Agent Skills Media Suite"
  (it "is consolidated into macher-agent-skills-test.el"
    (expect t :to-be t)))

(provide 'macher-agent-skills-media-test)
;;; macher-agent-skills-media-test.el ends here
