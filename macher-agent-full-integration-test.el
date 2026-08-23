;;; macher-agent-full-integration-test.el --- Consolidated integration suite -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))
(require 'buttercup)
(require 'macher-agent-test-setup)

(describe "Macher-Agent Full Integration Suite"
  (it "is consolidated into macher-agent-integration-test.el"
    (expect t :to-be t)))

(provide 'macher-agent-full-integration-test)
;;; macher-agent-full-integration-test.el ends here
