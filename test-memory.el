;;; test-memory.el --- Zero-Mem conversation memory and drift preservation -*- lexical-binding: t; -*-

;;; Commentary:
;; Specification 9: Zero-Mem conversation memory, source-vs-index drift, and truncation preservation
;; MEM-01, MEM-02, MEM-03

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-test-helpers)
(require 'macher-agent-core)
(require 'macher-agent-gptel)
(require 'macher-agent-zero-mem)

(describe "9. Zero-Mem conversation memory and source-vs-index drift"

  (before-each
    (macher-agent-test-reset-env))

  ;; --------------------------------------------------------------------------
  ;; MEM-01: Content hash tracking and cache invalidation on buffer drift
  ;; --------------------------------------------------------------------------
  (it "MEM-01: detects content changes via content-hash and rebuilds graph when modified"
    (let ((buf (generate-new-buffer "*test-mem-01*")))
      (unwind-protect
          (with-current-buffer buf
            (insert "User: Refactor auth module\n")
            (insert "Assistant: Auth refactoring started\n")
            ;; 1. Initial indexing
            (let ((g1 (macher-agent-zero-mem--get-graph buf)))
              (expect (macher-agent-zero-mem-graph-p g1) :to-be t)
              (expect (macher-agent-zero-mem-graph-content-hash g1) :not :to-be nil)
              (expect (macher-agent-zero-mem-graph-modified-tick g1)
                      :to-equal (buffer-chars-modified-tick buf))

              ;; 2. Calling get-graph again without modifications returns cached graph
              (let ((g-cached (macher-agent-zero-mem--get-graph buf)))
                (expect (eq g1 g-cached) :to-be t))

              ;; 3. Modifying buffer content updates content-hash and rebuilds graph
              (insert "Assistant: Auth updated successfully\n")
              (let ((g2 (macher-agent-zero-mem--get-graph buf)))
                (expect (macher-agent-zero-mem-graph-p g2) :to-be t)
                (expect (eq g1 g2) :to-be nil)
                (expect (eql (macher-agent-zero-mem-graph-content-hash g1)
                             (macher-agent-zero-mem-graph-content-hash g2))
                        :to-be nil)
                (expect (macher-agent-zero-mem-graph-content-hash g2)
                        :to-equal (with-current-buffer buf
                                    (sxhash-equal (buffer-substring-no-properties (point-min) (point-max))))))))
        (when (buffer-live-p buf)
          (kill-buffer buf)))))

  ;; --------------------------------------------------------------------------
  ;; MEM-02: Preserving archived trace nodes when earlier history is truncated
  ;; --------------------------------------------------------------------------
  (it "MEM-02: preserves archived trace nodes when earlier buffer history is truncated or omitted"
    (let ((buf (generate-new-buffer "*test-mem-02*")))
      (unwind-protect
          (with-current-buffer buf
            (insert "User: Refactor auth module\n")
            (insert "Assistant: Checking auth tokens\n")
            (insert "Assistant: Auth updated\n")
            ;; 1. Build initial graph
            (let ((g-initial (macher-agent-zero-mem--get-graph buf)))
              (expect (macher-agent-zero-mem-graph-p g-initial) :to-be t)

              ;; 2. Simulate truncation of earlier turn
              (goto-char (point-min))
              (forward-line 1)
              (delete-region (point-min) (point))
              (goto-char (point-min))
              (insert "[... SYSTEM ALERT: macher-agent truncated 1 lines of early history to conserve tokens. If you need this context, use the `search_conversation_history` tool ...]\n\n")

              ;; 3. Re-indexing detects drift and preserves archived trace nodes
              (let ((g-truncated (macher-agent-zero-mem--get-graph buf)))
                (expect (macher-agent-zero-mem-graph-p g-truncated) :to-be t)
                (expect (eq g-initial g-truncated) :to-be nil)

                ;; Refactor auth trace is archived and searchable even though deleted from live buffer
                (let ((retrieved-auth (macher-agent-zero-mem-retrieve "Refactor auth" g-truncated)))
                  (expect (length retrieved-auth) :to-be-greater-than 0)
                  (expect (cl-some (lambda (tr)
                                     (string-match-p "Refactor auth module"
                                                     (macher-agent-zero-mem-trace-text tr)))
                                   retrieved-auth)
                          :to-be-truthy))

                ;; Live turns remain searchable
                (let ((retrieved-live (macher-agent-zero-mem-retrieve "Auth updated" g-truncated)))
                  (expect (length retrieved-live) :to-be-greater-than 0)
                  (expect (cl-some (lambda (tr)
                                     (string-match-p "Auth updated"
                                                     (macher-agent-zero-mem-trace-text tr)))
                                   retrieved-live)
                          :to-be-truthy))

                ;; Recall via macher-agent-memory-search-zero-mem finds the archived turn
                (let ((search-res (macher-agent-memory-search-zero-mem buf "Refactor auth")))
                  (expect search-res :to-match "Refactor auth module")))))
        (when (buffer-live-p buf)
          (kill-buffer buf)))))

  ;; --------------------------------------------------------------------------
  ;; MEM-03: Truncate-buffer pipeline indexes target-buf before deletion
  ;; --------------------------------------------------------------------------
  (it "MEM-03: indexes target-buf in vector storage before deleting wire-buf region"
    (let* ((target-buf (generate-new-buffer "*test-mem-03-target*"))
           (wire-buf (generate-new-buffer "*test-mem-03-wire*"))
           (macher-agent-max-context-chars '((nil . 80))))
      (unwind-protect
          (progn
            ;; Populate target buffer with history
            (with-current-buffer target-buf
              (insert "---\n")
              (insert "Turn 1: Initial user prompt about database migrations\n")
              (let ((pt (point)))
                (insert "Turn 2: Response with migration details\n")
                (put-text-property pt (point) 'gptel 'response))
              (insert "Turn 3: Follow-up question on schema\n")
              (let ((pt (point)))
                (insert "Turn 4: Final migration plan\n")
                (put-text-property pt (point) 'gptel 'response)))

            ;; Populate wire buffer with identical text exceeding max-chars 80
            (with-current-buffer wire-buf
              (insert-buffer-substring target-buf)
              (expect (> (buffer-size wire-buf) 80) :to-be t)

              (let ((state (make-macher-agent-transmission-state
                            :target-buffer target-buf
                            :tools nil)))
                ;; Execute truncation step
                (macher-agent-memory-pipe--truncate-buffer state)

                ;; Wire buffer was truncated and system alert was inserted
                (expect (buffer-string) :to-match "SYSTEM ALERT: macher-agent truncated")
                ;; Event horizon was set on target-buf
                (let ((horizon (buffer-local-value 'macher-agent-zero-mem--event-horizon target-buf)))
                  (expect horizon :not :to-be nil)
                  (expect (plist-get horizon :offset) :to-be-greater-than (point-min)))
                ;; target-buf's graph was indexed in vector storage
                (expect (gethash (buffer-name target-buf) macher-agent-memory-vector-storage)
                        :not :to-be nil)
                ;; Truncated early history is retrievable via memory search
                (let ((recall (macher-agent-memory-search-zero-mem target-buf "database migrations")))
                  (expect recall :to-match "database migrations")))))
        (when (buffer-live-p target-buf) (kill-buffer target-buf))
        (when (buffer-live-p wire-buf) (kill-buffer wire-buf))))))

(provide 'test-memory)
;;; test-memory.el ends here
