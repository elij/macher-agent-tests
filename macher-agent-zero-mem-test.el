;;; macher-agent-zero-mem-test.el --- Tests for Macher Agent Zero-Mem -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Buttercup test suite for `macher-agent-zero-mem.el' covering PageRank
;; graph diffusion, retrieval, turn demarcation, event horizon filtering,
;; and context plugin state isolation.

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-zero-mem)

;;;; 0. Reference Float PageRank Implementation for Equivalence Checks

(defun macher-agent-zero-mem-pagerank-float (query graph &optional iterations)
  "Compute Stationary Personalised PageRank for QUERY over GRAPH using float arithmetic.
ITERATIONS defaults to 15.  Return a hash table mapping nodes to PageRank scores."
  (let* ((iters (or iterations 15))
         (gamma macher-agent-zero-mem-damping-factor)
         (pi-table (make-hash-table :test 'equal))
         (reset-vector (make-hash-table :test 'equal))
         (nodes nil)
         (adj-list (macher-agent-zero-mem-graph-adj-list graph)))
    (maphash (lambda (node _trans) (push node nodes)) adj-list)
    (let* ((alignments (macher-agent-zero-mem-align-query query graph))
           (align-sum (cl-loop for (_node . val) in alignments sum val)))
      (if (> align-sum 0.0)
          (dolist (align alignments)
            (puthash (car align) (/ (cdr align) align-sum) reset-vector))
        (let* ((doc-count 0)
               (doc-nodes nil))
          (dolist (node nodes)
            (when (eq (car node) :doc)
              (push node doc-nodes)
              (setq doc-count (1+ doc-count))))
          (if (> doc-count 0)
              (dolist (dn doc-nodes)
                (puthash dn (/ 1.0 (float doc-count)) reset-vector))
            (let ((uniform-prob (/ 1.0 (float (length nodes)))))
              (dolist (node nodes) (puthash node uniform-prob reset-vector)))))))
    (maphash (lambda (node val) (puthash node val pi-table)) reset-vector)
    (cl-loop repeat iters do
             (let ((next-pi (make-hash-table :test 'equal)))
               (maphash (lambda (node r-val)
                          (puthash node (* (- 1.0 gamma) r-val) next-pi))
                        reset-vector)
               (maphash
                (lambda (u-node u-val)
                  (let ((transitions (gethash u-node adj-list)))
                    (dolist (trans transitions)
                      (let* ((v-node (car trans))
                             (weight (cdr trans))
                             (current-v (gethash v-node next-pi 0.0)))
                        (puthash v-node (+ current-v (* gamma u-val weight)) next-pi)))))
                pi-table)
               (setq pi-table next-pi)))
    pi-table))

(cl-defun macher-agent-zero-mem-test-retrieve-float (query graph &key (top-k 5) (iterations 15))
  "Execute float-based retrieval for QUERY on GRAPH."
  (let* ((pi-table (macher-agent-zero-mem-pagerank-float query graph iterations))
         (doc-scores nil)
         (traces-ht (macher-agent-zero-mem-graph-traces graph)))
    (maphash
     (lambda (node score)
       (when (eq (car node) :doc)
         (push (cons (cdr node) score) doc-scores)))
     pi-table)
    (setq doc-scores (sort doc-scores (lambda (a b) (> (cdr a) (cdr b)))))
    (let ((top-ids (cl-loop for (id . _score) in doc-scores repeat top-k collect id))
          (results nil))
      (dolist (id top-ids)
        (let ((trace (gethash id traces-ht)))
          (when trace (push trace results))))
      (nreverse results))))

;;;; 1. Synthetic Fixture Helpers

(defun macher-agent-zero-mem-test-make-synthetic-traces (count)
  "Generate COUNT synthetic raw trace plists dynamically."
  (let ((vocab '("Zero-Mem" "PageRank" "Entity-Context" "BipartiteGraph" "TraceGraph"
                 "TemporalTrace" "QueryAlignment" "DualViewRetrieval" "MacherAgent"
                 "GptelBridge" "EmacsLisp" "FixedPointArithmetic"))
        (traces nil))
    (dotimes (i count)
      (let* ((ent1 (nth (% (* i 3) (length vocab)) vocab))
             (ent2 (nth (% (+ (* i 3) 1) (length vocab)) vocab))
             (text (format "Document trace #%d regarding %s and %s." i ent1 ent2)))
        (push (list :text text
                    :timestamp (float i)
                    :metadata (list :session "synth-session" :index i))
              traces)))
    (nreverse traces)))

(defun macher-agent-zero-mem-test-make-synthetic-graph (count)
  "Generate a synthetic `macher-agent-zero-mem-graph' with COUNT nodes."
  (macher-agent-zero-mem-build-graph
   (macher-agent-zero-mem-test-make-synthetic-traces count)))

;;;; 2. Consolidated Test Suites

(describe "Macher Agent Zero-Mem Test Suite"
  (before-each
    (setq macher-agent-search-backend-function #'macher-agent-search-glob))
  (after-each
    (when (fboundp 'macher-agent-zero-mem-uninstall)
      (macher-agent-zero-mem-uninstall))
    (setq macher-agent-search-backend-function #'macher-agent-search-glob))
  (after-all
    (when (fboundp 'macher-agent-zero-mem-uninstall)
      (macher-agent-zero-mem-uninstall))
    (setq macher-agent-search-backend-function #'macher-agent-search-glob))

  (describe "1. Graph Diffusion and PageRank Retrieval"
    (it "builds bipartite entity-document graphs from raw traces"
      (let* ((traces (macher-agent-zero-mem-test-make-synthetic-traces 15))
             (graph (macher-agent-zero-mem-build-graph traces)))
        (expect (length traces) :to-equal 15)
        (expect (hash-table-p (macher-agent-zero-mem-graph-traces graph)) :to-equal t)
        (expect (hash-table-p (macher-agent-zero-mem-graph-entity-index graph)) :to-equal t)
        (expect (hash-table-p (macher-agent-zero-mem-graph-adj-list graph)) :to-equal t)
        (expect (hash-table-count (macher-agent-zero-mem-graph-traces graph)) :to-equal 15)
        (expect (> (hash-table-count (macher-agent-zero-mem-graph-entity-index graph)) 0) :to-be t)))

    (it "preserves ranking order between float and fixed-point PageRank algorithms"
      (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 25))
             (query "Zero-Mem PageRank")
             (float-res (macher-agent-zero-mem-pagerank-float query graph 15))
             (fixed-res (macher-agent-zero-mem-pagerank-fixed-point query graph 15))
             (float-scores nil)
             (fixed-scores nil))
        (maphash (lambda (k v) (push (cons k v) float-scores)) float-res)
        (maphash (lambda (k v) (push (cons k v) fixed-scores)) fixed-res)
        (setq float-scores (sort float-scores (lambda (a b) (> (cdr a) (cdr b)))))
        (setq fixed-scores (sort fixed-scores (lambda (a b) (> (cdr a) (cdr b)))))
        (expect (car (nth 0 float-scores)) :to-equal (car (nth 0 fixed-scores)))
        (expect (car (nth 1 float-scores)) :to-equal (car (nth 1 fixed-scores)))
        (expect (car (nth 2 float-scores)) :to-equal (car (nth 2 fixed-scores)))))

    (it "retrieves top-k ranked traces with default and custom arguments"
      (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 20))
             (query "PageRank EmacsLisp")
             (results-default (macher-agent-zero-mem-retrieve query graph))
             (results-k3 (macher-agent-zero-mem-retrieve query graph :top-k 3))
             (results-float (macher-agent-zero-mem-test-retrieve-float query graph :top-k 5 :iterations 10))
             (results-fp (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 10)))
        (expect (length results-default) :to-equal 5)
        (expect (length results-k3) :to-equal 3)
        (expect (length results-float) :to-equal 5)
        (expect (length results-fp) :to-equal 5)))

    (it "bridges semantic entity gaps through bipartite graph traversal"
      (let* ((traces (list
                      '(:text "System alert: The Hyperion bridge module handles Websocket protocol." :timestamp 1.0)
                      '(:text "User: We observe Latency spikes during Websocket data transfer." :timestamp 2.0)
                      '(:text "Agent: Unrelated background maintenance." :timestamp 3.0)))
             (graph (macher-agent-zero-mem-build-graph traces))
             (results (macher-agent-zero-mem-retrieve "Hyperion Latency" graph :top-k 2)))
        (expect (length results) :to-equal 2)
        (let ((texts (mapcar #'macher-agent-zero-mem-trace-text results)))
          (expect (cl-some (lambda (txt) (string-match-p "Hyperion bridge" txt)) texts) :to-be-truthy)
          (expect (cl-some (lambda (txt) (string-match-p "Latency spikes" txt)) texts) :to-be-truthy)))))

  (describe "2. Turn Demarcation, Wire Pruning, and Event Horizon Filtering"
    (it "demarcates prompt and response turns with line and offset metadata"
      (let ((buf (generate-new-buffer " *test-turn-demarcate*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (insert "User turn 1 line\n")
                (let ((p (point)))
                  (insert "Assistant response line\n")
                  (put-text-property p (point) 'gptel 'response))
                (insert "User turn 2 line\n"))
              (let ((traces (macher-agent-zero-mem--buffer-to-traces buf)))
                (expect (length traces) :to-equal 3)
                (let ((t1 (nth 0 traces))
                      (t2 (nth 1 traces))
                      (t3 (nth 2 traces)))
                  (expect (plist-get (plist-get t1 :metadata) :type) :to-equal :prompt)
                  (expect (plist-get (plist-get t2 :metadata) :type) :to-equal :response)
                  (expect (plist-get (plist-get t3 :metadata) :type) :to-equal :prompt)
                  (expect (plist-get (plist-get t1 :metadata) :turn) :to-equal 1)
                  (expect (plist-get (plist-get t2 :metadata) :turn) :to-equal 2)
                  (expect (plist-get (plist-get t3 :metadata) :turn) :to-equal 3)
                  (expect (plist-get (plist-get t1 :metadata) :line) :to-equal 1)
                  (expect (plist-get (plist-get t2 :metadata) :line) :to-equal 2)
                  (expect (plist-get (plist-get t3 :metadata) :line) :to-equal 3))))
          (when (buffer-live-p buf) (kill-buffer buf)))))

    (it "prunes transmission wire buffer non-destructively without modifying orig-buf"
      (let* ((orig-buf (generate-new-buffer " *test-live-orig-buf*"))
             (tx-buf (generate-new-buffer " *test-ephemeral-tx-buf*"))
             (initial-text "---\nkey: val\n---\nPrompt 1\n")
             (resp-text "Response from model 1\n")
             (query-text "Latest user query content\n"))
        (unwind-protect
            (progn
              (with-current-buffer orig-buf
                (insert initial-text)
                (let ((start (point)))
                  (insert resp-text)
                  (put-text-property start (point) 'gptel 'response))
                (insert query-text))
              (let ((orig-content (with-current-buffer orig-buf (buffer-string))))
                (with-current-buffer tx-buf
                  (insert orig-content))
                (with-current-buffer tx-buf
                  (let ((macher-agent-max-context-chars '((nil . 25))))
                    (macher-agent-memory-pipe--truncate-buffer nil orig-buf nil nil nil)))
                (expect (with-current-buffer tx-buf (buffer-string))
                        :to-match "SYSTEM ALERT: macher-agent truncated")
                (expect (with-current-buffer tx-buf (buffer-string))
                        :to-match "Latest user query content")
                (expect (with-current-buffer orig-buf (buffer-string))
                        :to-equal orig-content)))
          (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
          (when (buffer-live-p tx-buf) (kill-buffer tx-buf)))))

    (it "filters search conversation history to only return traces before event horizon"
      (let* ((orig-buf (generate-new-buffer " *test-horizon-filter*"))
             (tx-buf (generate-new-buffer " *test-tx-horizon*")))
        (unwind-protect
            (progn
              (with-current-buffer orig-buf
                (insert "Early historical trace with UniqueTokenAlpha\n")
                (let ((p (point)))
                  (insert "Previous response boundary with UniqueTokenBeta\n")
                  (put-text-property p (point) 'gptel 'response))
                (insert "Active context window with UniqueTokenGamma\n"))
              (with-current-buffer tx-buf
                (insert (with-current-buffer orig-buf (buffer-string))))
              (with-current-buffer tx-buf
                (let ((macher-agent-max-context-chars '((nil . 45))))
                  (macher-agent-memory-pipe--truncate-buffer nil orig-buf nil nil nil)))
              ;; Early token before event horizon is retrieved
              (let ((res-early (macher-agent-memory-search-zero-mem "UniqueTokenAlpha" orig-buf 2)))
                (expect res-early :to-match "Match near line 1")
                (expect res-early :to-match "UniqueTokenAlpha"))
              ;; Active context window token past event horizon is filtered out
              (let ((res-active (macher-agent-memory-search-zero-mem "UniqueTokenGamma" orig-buf 2)))
                (expect res-active :to-match "^No matches found in history for:"))
              ;; Search dispatch routing
              (let ((macher-agent-search-backend-function #'macher-agent-memory-search-zero-mem))
                (expect (macher-agent-search-dispatch "UniqueTokenAlpha" orig-buf 2) :to-match "UniqueTokenAlpha")
                (expect (macher-agent-search-dispatch "UniqueTokenGamma" orig-buf 2) :to-match "^No matches found in history for:")))
          (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
          (when (buffer-live-p tx-buf) (kill-buffer tx-buf))))))

  (describe "3. Autonomous Plugin Lifecycle and Context State Isolation"
    (it "installs and uninstalls pipeline steps dynamically"
      (let ((saved-registry (copy-hash-table macher-agent-pipeline-registry)))
        (unwind-protect
            (progn
              (clrhash macher-agent-pipeline-registry)
              (macher-agent-zero-mem-install)
              (let ((steps (macher-agent-get-pipeline-steps 'transmission)))
                (expect (member #'macher-agent-memory-pipe--inject-tool steps) :to-be-truthy)
                (expect (member #'macher-agent-parent-memory-pipe--inject-tool steps) :to-be-truthy)
                (expect (member #'macher-agent-memory-pipe--truncate-buffer steps) :to-be-truthy)
                (expect (member #'macher-agent-pipe--inject-zero-mem steps) :to-be-truthy)
                (expect (member #'macher-agent-pipe--inject-parent-context steps) :to-be-truthy)
                (expect (member #'macher-agent-memory-pipe--inject-directive steps) :to-be-truthy)
                (expect (member #'macher-agent-parent-memory-pipe--inject-directive steps) :to-be-truthy))
              (expect (default-value 'macher-agent-search-backend-function) :to-equal #'macher-agent-memory-search-zero-mem)
              (expect (member #'macher-agent-memory--persist-interaction macher-agent-task-flush-hook) :to-be-truthy)
              (macher-agent-zero-mem-uninstall)
              (let ((steps (macher-agent-get-pipeline-steps 'transmission)))
                (expect (member #'macher-agent-memory-pipe--inject-tool steps) :to-be nil)
                (expect (member #'macher-agent-parent-memory-pipe--inject-tool steps) :to-be nil)
                (expect (member #'macher-agent-memory-pipe--truncate-buffer steps) :to-be nil)
                (expect (member #'macher-agent-pipe--inject-zero-mem steps) :to-be nil)
                (expect (member #'macher-agent-pipe--inject-parent-context steps) :to-be nil)
                (expect (member #'macher-agent-memory-pipe--inject-directive steps) :to-be nil)
                (expect (member #'macher-agent-parent-memory-pipe--inject-directive steps) :to-be nil))
              (expect (member #'macher-agent-memory--persist-interaction macher-agent-task-flush-hook) :to-be nil)
              (expect (default-value 'macher-agent-search-backend-function) :to-equal #'macher-agent-search-glob))
          (setq macher-agent-pipeline-registry saved-registry))))

    (it "manages zero-mem state strictly inside context plugins plist"
      (expect (fboundp 'macher-agent-context-zero-mem) :to-be nil)
      (expect (fboundp 'macher-agent--buffer-to-traces) :to-be nil)
      (let ((ctx (make-macher-agent-context :id "test-ctx" :plugins '(:existing-key "val"))))
        (expect (macher-agent-zero-mem-get-state ctx) :to-be nil)
        (macher-agent-zero-mem-set-state ctx '(:traces ((:id 1 :text "node1"))))
        (expect (macher-agent-zero-mem-get-state ctx) :to-equal '(:traces ((:id 1 :text "node1"))))
        (expect (plist-get (macher-agent-context-plugins ctx) :zero-mem) :to-equal '(:traces ((:id 1 :text "node1"))))
        (expect (plist-get (macher-agent-context-plugins ctx) :existing-key) :to-equal "val")))

    (it "persists interaction graph into context plugins and uses stationary snapshot"
      (let* ((parent-buf (generate-new-buffer " *test-parent-snap*"))
             (child-buf (generate-new-buffer " *test-child-snap*"))
             (parent-ctx (make-macher-agent-context :id "parent-ctx"))
             (child-ctx (make-macher-agent-context :id "child-ctx")))
        (unwind-protect
            (progn
              (with-current-buffer parent-buf
                (setq-local macher-agent--persistent-context parent-ctx)
                (insert "Turn 1: Database credentials stored under DB_SECRET_KEY\n")
                (insert "Turn 2: Service discovery on port 8080\n"))
              (let ((parent-graph (macher-agent-memory--persist-interaction parent-buf)))
                (expect parent-graph :not :to-be nil)
                (expect (macher-agent-zero-mem-get-state parent-ctx) :to-equal parent-graph)
                (expect (plist-get (macher-agent-context-plugins parent-ctx) :zero-mem) :to-equal parent-graph)
                (with-current-buffer child-buf
                  (setq-local macher-agent--persistent-context child-ctx)
                  (macher-agent--push-routing "task-snap-1" (buffer-name parent-buf))
                  (insert "Retrieve DB_SECRET_KEY from parent."))
                ;; Modify parent buffer after delegation
                (with-current-buffer parent-buf
                  (insert "Turn 3: Post-delegation live buffer modification\n"))
                ;; Searching uses stationary snapshot without re-indexing
                (spy-on 'macher-agent-zero-mem--buffer-to-traces :and-call-through)
                (let ((res (macher-agent-memory-search-zero-mem "DB_SECRET_KEY" parent-buf 2)))
                  (expect res :to-match "DB_SECRET_KEY")
                  (expect 'macher-agent-zero-mem--buffer-to-traces :not :to-have-been-called))
                ;; Subagent parent context injection
                (let* ((state (make-macher-agent-transmission-state
                               :target-buffer child-buf
                               :directives nil))
                       (updated-state (macher-agent-pipe--inject-parent-context
                                       state child-buf nil nil nil))
                       (dirs (macher-agent-transmission-state-directives updated-state)))
                  (expect (length dirs) :to-be-greater-than 0)
                  (let ((text (string-join dirs "\n\n")))
                    (expect text :to-match "<parent_conversation_context>")
                    (expect text :to-match "DB_SECRET_KEY")))))
          (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
          (when (buffer-live-p child-buf) (kill-buffer child-buf)))))))

(provide 'macher-agent-zero-mem-test)
;;; macher-agent-zero-mem-test.el ends here
