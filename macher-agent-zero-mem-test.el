;;; macher-agent-zero-mem-test.el --- Benchmark & Unit Tests for Macher Agent Zero-Mem -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Buttercup test suite for `macher-agent-zero-mem.el' benchmarking the
;; refactored fixed-point PageRank algorithm against the baseline float-based PageRank.

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-zero-mem)

;;;; 0. Baseline Floating-Point PageRank and Retrieval Implementation

(defun macher-agent-zero-mem-pagerank-float (query graph &optional iterations)
  "Compute Stationary Personalised PageRank for QUERY over GRAPH.
ITERATIONS defaults to 15.  Return a hash table mapping nodes to
PageRank scores."
  (let* ((iters (or iterations 15))
         (gamma macher-agent-zero-mem-damping-factor)
         (pi-table (make-hash-table :test 'equal))
         (reset-vector (make-hash-table :test 'equal))
         (nodes nil)
         (adj-list (macher-agent-zero-mem-graph-adj-list graph)))

    ;; 1. Collect all valid nodes
    (maphash (lambda (node _trans) (push node nodes)) adj-list)

    ;; 2. Establish the normalised Reset Distribution r_q [37]
    (let* ((alignments (macher-agent-zero-mem-align-query query graph))
           (align-sum (cl-loop for (_node . val) in alignments sum val)))
      (if (> align-sum 0.0)
          ;; Distribute reset probability over aligned query entity nodes
          (dolist (align alignments)
            (puthash (car align) (/ (cdr align) align-sum) reset-vector))
        ;; Fallback: uniform reset distribution over all Document nodes
        (let* ((doc-count 0)
               (doc-nodes nil))
          (dolist (node nodes)
            (when (eq (car node) :doc)
              (push node doc-nodes)
              (setq doc-count (1+ doc-count))))
          (if (> doc-count 0)
              (dolist (dn doc-nodes)
                (puthash dn (/ 1.0 (float doc-count)) reset-vector))
            ;; Ultimate fallback: absolute uniform over all nodes
            (let ((uniform-prob (/ 1.0 (float (length nodes)))))
              (dolist (node nodes) (puthash node uniform-prob reset-vector)))))))

    ;; 3. Initialise pi distribution to match the reset vector
    (maphash (lambda (node val) (puthash node val pi-table)) reset-vector)

    ;; 4. Iterative Power Method for PageRank: pi = (1-gamma)*r + gamma * P^T * pi [37]
    (cl-loop repeat iters do
             (let ((next-pi (make-hash-table :test 'equal)))
               ;; Add the (1-gamma)*r_q prior to next state
               (maphash (lambda (node r-val)
                          (puthash node (* (- 1.0 gamma) r-val) next-pi))
                        reset-vector)
               ;; Propagate state over transitions: gamma * P^T * pi
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
  "Execute float-based Dual-View Evidence retrieval for QUERY on GRAPH for benchmarking."
  (let* ((pi-table (macher-agent-zero-mem-pagerank-float query graph iterations))
         (doc-scores nil)
         (traces-ht (macher-agent-zero-mem-graph-traces graph)))

    ;; 1. Filter and normalise document scores [41]
    (maphash
     (lambda (node score)
       (when (eq (car node) :doc)
         (push (cons (cdr node) score) doc-scores)))
     pi-table)

    (setq doc-scores (sort doc-scores (lambda (a b) (> (cdr a) (cdr b)))))

    ;; 2. Retrieve top-K document nodes [55]
    (let ((top-ids (cl-loop for (id . _score) in doc-scores
                            repeat top-k
                            collect id))
          (results nil))
      (dolist (id top-ids)
        (let ((trace (gethash id traces-ht)))
          (when trace
            (push trace results))))
      (nreverse results))))


;;;; 1. Environment Isolation Utilities

(defun macher-agent-zero-mem-test-isolate-environment ()
  "Isolate test environment before each benchmark run.
Cancels active asynchronous timers, wipes background buffers, and
forces an immediate garbage collection cycle."
  ;; 1. Cancel active timers
  (when (boundp 'timer-list)
    (mapc #'cancel-timer (copy-sequence timer-list)))
  (when (boundp 'timer-idle-list)
    (mapc #'cancel-timer (copy-sequence timer-idle-list)))

  ;; 2. Wipe non-essential background buffers
  (dolist (buf (buffer-list))
    (unless (or (eq buf (current-buffer))
                (member (buffer-name buf) '("*scratch*" "*Messages*" "*stdout*" "*stderr*"))
                (string-prefix-p " *" (buffer-name buf))
                (string-match-p "buttercup" (buffer-name buf)))
      (ignore-errors (kill-buffer buf))))

  ;; 3. Reset search backend function to default glob backend
  (setq macher-agent-search-backend-function #'macher-agent-search-glob)

  ;; 4. Explicitly force a garbage collection cycle
  (garbage-collect))


;;;; 2. Benchmarking Metrics Extraction and Logging

(defun macher-agent-zero-mem-test-measure-execution (thunk)
  "Execute THUNK and extract benchmarking metrics using native Emacs utilities.
Captures total execution time, total garbage collection cycles, and total GC processing time.
Returns a plist containing :result, :total-time, :gc-cycles, and :gc-time."
  (let* ((gc-cycles-before gcs-done)
         (gc-time-before gc-elapsed)
         (start-time (float-time))
         (result (funcall thunk))
         (end-time (float-time))
         (gc-cycles-after gcs-done)
         (gc-time-after gc-elapsed)
         (total-time (- end-time start-time))
         (gc-cycles (- gc-cycles-after gc-cycles-before))
         (gc-time (- gc-time-after gc-time-before)))
    (list :result result
          :total-time total-time
          :gc-cycles gc-cycles
          :gc-time gc-time)))

(defun macher-agent-zero-mem-test-log-metrics (label metrics)
  "Log benchmark METRICS plist for LABEL to output console using native utilities."
  (let ((msg (format "[BENCHMARK] %-32s | Total Time: %9.6f s | GC Cycles: %3d | GC Time: %9.6f s"
                     label
                     (plist-get metrics :total-time)
                     (plist-get metrics :gc-cycles)
                     (plist-get metrics :gc-time))))
    (message "%s" msg)
    (ignore-errors
      (princ (concat msg "\n") 'external-debugging-output))))


;;;; 3. Side-by-Side Custom Assertion Macro

(defmacro expect-benchmark-faster (baseline-form optimised-form &optional label)
  "Custom assertion macro running BASELINE-FORM and OPTIMISED-FORM side-by-side.
Isolates environment before each run, captures and logs three native metrics
\(total time, GC cycles, GC time), and asserts OPTIMISED-FORM executes faster
than BASELINE-FORM within a mathematical tolerance of either a 10ms floor or 15% buffer."
  `(let* ((tag (or ,label "Benchmark Comparison"))
          ;; Step 1: Isolate environment and measure baseline control group
          (_ (macher-agent-zero-mem-test-isolate-environment))
          (base-metrics (macher-agent-zero-mem-test-measure-execution (lambda () ,baseline-form)))
          (_ (macher-agent-zero-mem-test-log-metrics (concat tag " [Baseline Float]") base-metrics))
          ;; Step 2: Isolate environment and measure optimised candidate
          (_ (macher-agent-zero-mem-test-isolate-environment))
          (opt-metrics (macher-agent-zero-mem-test-measure-execution (lambda () ,optimised-form)))
          (_ (macher-agent-zero-mem-test-log-metrics (concat tag " [Optimised Fixed-Point]") opt-metrics))
          ;; Step 3: Extract timing and calculate mathematical tolerance
          (base-time (plist-get base-metrics :total-time))
          (opt-time (plist-get opt-metrics :total-time))
          ;; Mathematical tolerance: either 10-millisecond floor (0.010s) or 15% buffer (* 0.15 base-time)
          (tolerance-buffer (max 0.010 (* 0.15 base-time)))
          (threshold (+ base-time tolerance-buffer)))
     ;; Step 4: Summary logging
     (message "[BENCHMARK SUMMARY] %s: Baseline=%.6fs | Optimised=%.6fs | Max Threshold=%.6fs | Speedup=%.2fx"
              tag base-time opt-time threshold
              (if (> opt-time 0.0) (/ base-time opt-time) 1.0))
     ;; Step 5: Assert that optimised execution time is under threshold
     (expect opt-time :to-be-less-than threshold)))


;;;; 4. Synthetic Payload and Mock Data Generators

(defcustom macher-agent-zero-mem-test-vocabulary
  '("Zero-Mem" "PageRank" "Entity-Context" "BipartiteGraph" "TraceGraph"
    "StationaryDistribution" "DampingFactor" "PersonalizedPageRank"
    "TemporalTrace" "QueryAlignment" "DualViewRetrieval" "ScoreFusion"
    "MacherAgent" "GptelBridge" "EmacsLisp" "FixedPointArithmetic"
    "PowerIteration" "VectorEmbedding" "JaroWinkler" "AdjacencyMatrix"
    "ChronologicalTrace" "EntityReverseIndex" "ContextNode" "DocumentNode")
  "Vocabulary pool for synthetic payload generation."
  :type '(repeat string)
  :group 'macher-agent-zero-mem)

(defun macher-agent-zero-mem-test-make-synthetic-traces (count &optional entities-per-trace)
  "Generate COUNT synthetic raw trace plists dynamically.
ENTITIES-PER-TRACE controls noun density per trace document."
  (let ((traces nil)
        (density (or entities-per-trace 4))
        (vocab-len (length macher-agent-zero-mem-test-vocabulary)))
    (dotimes (i count)
      (let ((ents nil)
            (text-parts nil))
        (dotimes (j density)
          (let ((idx (% (+ (* i 7) (* j 13)) vocab-len)))
            (push (nth idx macher-agent-zero-mem-test-vocabulary) ents)))
        (setq ents (delete-dups (delq nil ents)))
        (setq text-parts
              (list (format "Document trace #%d recording session activity." i)
                    (format "Contains entities: %s." (string-join ents ", "))
                    (format "Code identifier `macher-agent-zero-mem-node-%d`." i)
                    (format "\"quoted-parameter-%d\"" i)))
        (push (list :text (string-join text-parts " ")
                    :timestamp (float i)
                    :metadata (list :session "synth-session" :index i))
              traces)))
    (nreverse traces)))

(defun macher-agent-zero-mem-test-make-synthetic-graph (trace-count &optional entities-per-trace)
  "Generate a synthetic `macher-agent-zero-mem-graph' with TRACE-COUNT nodes."
  (let ((raw-traces (macher-agent-zero-mem-test-make-synthetic-traces trace-count entities-per-trace)))
    (macher-agent-zero-mem-build-graph raw-traces)))

(defun macher-agent-zero-mem-test-make-synthetic-query (index)
  "Generate a synthetic query string targeting entities around INDEX."
  (let* ((vocab-len (length macher-agent-zero-mem-test-vocabulary))
         (ent1 (nth (% index vocab-len) macher-agent-zero-mem-test-vocabulary))
         (ent2 (nth (% (+ index 3) vocab-len) macher-agent-zero-mem-test-vocabulary)))
    (format "Retrieve traces concerning %s and %s in macher-agent" ent1 ent2)))

(defun macher-agent-zero-mem-test-generate-corpus (num-traces target-buffer)
  "Generate NUM-TRACES for testing memory recall and performance.
Populates TARGET-BUFFER with raw text and returns a list of raw trace plists."
  (let ((traces nil)
        (vocab '("configuration" "module" "socket" "database" "query"
                 "API" "endpoint" "token" "session" "authentication" "timeout"
                 "Hyperion" "bridge" "protocol" "transfer" "spikes"))
        (speakers '("User" "Agent" "System")))
    
    (with-current-buffer target-buffer
      (erase-buffer)
      (dotimes (i num-traces)
        (let* ((speaker (nth (random (length speakers)) speakers))
               (word1 (nth (random (length vocab)) vocab))
               (word2 (nth (random (length vocab)) vocab))
               ;; Inject specific semantic gaps using capitalised Proper Nouns for the NER
               (text (cond
                      ((= i 15) "System alert: The Hyperion bridge module is responsible for the Websocket protocol.")
                      ((= i 20) "User: We are seeing massive Latency spikes during data transfer.")
                      ((= i (- num-traces 10)) "User: Can you check the logs for the bridge transfer?")
                      (t (format "%s processed the %s and the %s." speaker word1 word2))))
               (ts (float i))
               (trace (list :text text :timestamp ts :metadata (list :speaker speaker))))
          
          (insert text "\n")
          (push trace traces))))
    (nreverse traces)))

(defun macher-agent-zero-mem-test-naive-glob (query target-buffer ctx-lines)
  "Extract lines matching QUERY from TARGET-BUFFER with CTX-LINES of context [7]."
  (let ((results nil))
    (with-current-buffer target-buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward query nil t)
          (let* ((match-pt (point))
                 (start-pt (save-excursion 
                             (forward-line (- ctx-lines)) 
                             (line-beginning-position)))
                 (end-pt (save-excursion 
                           (forward-line ctx-lines) 
                           (line-end-position)))
                 (snippet (buffer-substring-no-properties start-pt end-pt)))
            (push (format "--- Match near line %d ---\n%s\n" 
                          (line-number-at-pos match-pt) snippet) 
                  results)))))
    (if results
        (string-join (nreverse results) "\n")
      "")))

;;;; 5. Test Suite Specifications

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

          (describe "1. Environment Isolation and Synthetic Generators"
                    (it "successfully isolates environment by cancelling timers, wiping buffers, and running GC"
                        (let ((timer (run-with-timer 100 nil #'ignore))
                              (test-buf (generate-new-buffer "*macher-test-dummy*")))
                          (expect (memq timer timer-list) :to-be-truthy)
                          (expect (buffer-live-p test-buf) :to-be t)
                          (macher-agent-zero-mem-test-isolate-environment)
                          (expect (memq timer timer-list) :to-be nil)
                          (expect (buffer-live-p test-buf) :to-be nil)))

                    (it "dynamically generates synthetic trace payloads and bipartite graphs"
                        (let* ((traces (macher-agent-zero-mem-test-make-synthetic-traces 20 4))
                               (graph (macher-agent-zero-mem-build-graph traces)))
                          (expect (length traces) :to-equal 20)
                          (expect (listp graph) :to-be t)
                          (expect (hash-table-count (macher-agent-zero-mem-graph-traces graph)) :to-equal 20)
                          (expect (> (hash-table-count (macher-agent-zero-mem-graph-entity-index graph)) 0) :to-be t))))

          (describe "2. Fixed-Point PageRank Score Equivalence & Order Preservation"
                    (it "preserves node ranking order between float and fixed-point PageRank"
                        (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 30 5))
                               (query "Zero-Mem PageRank test")
                               (float-res (macher-agent-zero-mem-pagerank-float query graph 15))
                               (fixed-res (macher-agent-zero-mem-pagerank-fixed-point query graph 15))
                               (float-scores nil)
                               (fixed-scores nil))
                          (maphash (lambda (k v) (push (cons k v) float-scores)) float-res)
                          (maphash (lambda (k v) (push (cons k v) fixed-scores)) fixed-res)
                          (setq float-scores (sort float-scores (lambda (a b) (> (cdr a) (cdr b)))))
                          (setq fixed-scores (sort fixed-scores (lambda (a b) (> (cdr a) (cdr b)))))
                          ;; Check top 3 nodes match between float and fixed-point PageRank
                          (expect (car (nth 0 float-scores)) :to-equal (car (nth 0 fixed-scores)))
                          (expect (car (nth 1 float-scores)) :to-equal (car (nth 1 fixed-scores)))
                          (expect (car (nth 2 float-scores)) :to-equal (car (nth 2 fixed-scores))))))

          (describe "3. Side-by-Side Performance Benchmarks (Fixed-Point vs Float PageRank)"
                    (it "benchmarks fixed-point PageRank vs float baseline on small graph (30 traces)"
                        (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 30 5))
                               (query (macher-agent-zero-mem-test-make-synthetic-query 0)))
                          (expect-benchmark-faster
                           (macher-agent-zero-mem-pagerank-float query graph 15)
                           (macher-agent-zero-mem-pagerank-fixed-point query graph 15)
                           "Small Graph PageRank (30 traces)")))

                    (it "benchmarks fixed-point PageRank vs float baseline on medium graph (100 traces)"
                        (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 100 8))
                               (query (macher-agent-zero-mem-test-make-synthetic-query 5)))
                          (expect-benchmark-faster
                           (macher-agent-zero-mem-pagerank-float query graph 20)
                           (macher-agent-zero-mem-pagerank-fixed-point query graph 20)
                           "Medium Graph PageRank (100 traces)")))

                    (it "benchmarks fixed-point PageRank vs float baseline on stressed graph (250 traces)"
                        (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 250 10))
                               (query (macher-agent-zero-mem-test-make-synthetic-query 10)))
                          (expect-benchmark-faster
                           (macher-agent-zero-mem-pagerank-float query graph 25)
                           (macher-agent-zero-mem-pagerank-fixed-point query graph 25)
                           "Stressed Graph PageRank (250 traces)")))

                    (it "benchmarks full dual-view retrieval pipeline (fixed-point vs float)"
                        (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 120 6))
                               (query (macher-agent-zero-mem-test-make-synthetic-query 3)))
                          (expect-benchmark-faster
                           (macher-agent-zero-mem-test-retrieve-float query graph :top-k 5 :iterations 20)
                           (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 20)
                           "Dual-View Retrieve Pipeline"))))

          (describe "4. Dual-View Evidence Retrieval Keyword Arguments & Operations"
                    (it "retrieves top-k results correctly with default and explicit keyword arguments"
                        (let* ((graph (macher-agent-zero-mem-test-make-synthetic-graph 30 5))
                               (query (macher-agent-zero-mem-test-make-synthetic-query 0))
                               (results-default (macher-agent-zero-mem-retrieve query graph))
                               (results-k3 (macher-agent-zero-mem-retrieve query graph :top-k 3))
                               (results-float (macher-agent-zero-mem-test-retrieve-float query graph :top-k 5 :iterations 10))
                               (results-fp (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 10)))
                          (expect (length results-default) :to-equal 5)
                          (expect (length results-k3) :to-equal 3)
                          (expect (length results-float) :to-equal 5)
                          (expect (length results-fp) :to-equal 5))))
          (describe "5. Memory recall comparison"
                    (it "bridges semantic gaps in the graph that the naive glob misses"
                        (let* ((test-buf (generate-new-buffer " *macher-test-corpus*"))
                               (traces (macher-agent-zero-mem-test-generate-corpus 50 test-buf))
                               (graph (macher-agent-zero-mem-build-graph traces))
                               (query "Websocket Latency")
                               (glob-result (macher-agent-zero-mem-test-naive-glob query test-buf 5))
                               (graph-results (macher-agent-zero-mem-retrieve query graph :top-k 5)))
                          
                          ;; The naive glob searches for the exact string or regex [7] and fails
                          (expect (string-empty-p glob-result) :to-be t)
                          
                          ;; The dual-view graph traverses edges to retrieve both traces
                          (let ((found-15 nil)
                                (found-20 nil))
                            (dolist (res graph-results)
                              (when (= (macher-agent-zero-mem-trace-id res) 15) (setq found-15 t))
                              (when (= (macher-agent-zero-mem-trace-id res) 20) (setq found-20 t)))
                            (expect found-15 :to-be t)
                            (expect found-20 :to-be t))
                          
                          (kill-buffer test-buf))))

          (describe "6. Three-way execution and memory benchmark"
                    (it "benchmarks float PageRank, fixed-point PageRank, and naive glob on a shared corpus"
                        (let* ((test-buf (generate-new-buffer " *macher-test-benchmark*"))
                               (traces (macher-agent-zero-mem-test-generate-corpus 500 test-buf))
                               (graph (macher-agent-zero-mem-build-graph traces))
                               (query "Websocket Latency"))
                          
                          (message "\n--- Three-Way Corpus Benchmark (500 Traces) ---")
                          
                          ;; Measure Baseline Float PageRank
                          (macher-agent-zero-mem-test-isolate-environment)
                          (let ((float-metrics (macher-agent-zero-mem-test-measure-execution 
                                                (lambda () (macher-agent-zero-mem-test-retrieve-float query graph :top-k 5 :iterations 15)))))
                            (macher-agent-zero-mem-test-log-metrics "1. Dual-View (Float PPR)" float-metrics))
                          
                          ;; Measure Optimised Fixed-Point PageRank
                          (macher-agent-zero-mem-test-isolate-environment)
                          (let ((fp-metrics (macher-agent-zero-mem-test-measure-execution 
                                             (lambda () (macher-agent-zero-mem-retrieve query graph :top-k 5 :iterations 15)))))
                            (macher-agent-zero-mem-test-log-metrics "2. Dual-View (Fixed-Point PPR)" fp-metrics))
                          
                          ;; Measure Naive Regular Expression Glob [7]
                          (macher-agent-zero-mem-test-isolate-environment)
                          (let ((glob-metrics (macher-agent-zero-mem-test-measure-execution 
                                               (lambda () (macher-agent-zero-mem-test-naive-glob query test-buf 5)))))
                            (macher-agent-zero-mem-test-log-metrics "3. Naive Buffer Glob" glob-metrics))
                          
                          (message "-----------------------------------------------")
                          
                          ;; Assert the test concludes successfully
                          (expect (buffer-live-p test-buf) :to-be t)
                          (kill-buffer test-buf))))

          (describe "7. Non-Destructive Wire Pruning"
                    (it "truncates transmission buffer without modifying live orig-buf"
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
                                  ;; Populate transmission wire buffer with same content
                                  (with-current-buffer tx-buf
                                    (insert orig-content))
                                  ;; Run wire pruning in transmission buffer context with tight limit
                                  (with-current-buffer tx-buf
                                    (let ((macher-agent-max-context-chars '((nil . 25))))
                                      (macher-agent-memory-pipe--truncate-buffer nil orig-buf nil nil nil)))
                                  ;; Transmission wire buffer is truncated with alert
                                  (expect (with-current-buffer tx-buf (buffer-string))
                                          :to-match "SYSTEM ALERT: macher-agent truncated")
                                  (expect (with-current-buffer tx-buf (buffer-string))
                                          :to-match "Latest user query content")
                                  ;; Live orig-buf is completely unmodified and intact
                                  (expect (with-current-buffer orig-buf (buffer-string))
                                          :to-equal orig-content)))
                            (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                            (when (buffer-live-p tx-buf) (kill-buffer tx-buf))))))

          (describe "8. Event Horizon Query Filtering and Turn Demarcation"
                    (it "demarcates prompt and response turns and records line and offset metadata"
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
                                    (expect (plist-get (plist-get t3 :metadata) :line) :to-equal 3)
                                    (expect (plist-get (plist-get t1 :metadata) :offset) :to-equal 1)
                                    (expect (> (plist-get (plist-get t2 :metadata) :offset) 1) :to-be t)
                                    (expect (> (plist-get (plist-get t3 :metadata) :offset)
                                               (plist-get (plist-get t2 :metadata) :offset)) :to-be t))))
                            (when (buffer-live-p buf) (kill-buffer buf)))))

                    (it "filters search_conversation_history to only return traces before event horizon"
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
                                ;; Truncate tx-buf to establish event horizon on orig-buf
                                (with-current-buffer tx-buf
                                  (insert (with-current-buffer orig-buf (buffer-string))))
                                (with-current-buffer tx-buf
                                  (let ((macher-agent-max-context-chars '((nil . 45))))
                                    (macher-agent-memory-pipe--truncate-buffer nil orig-buf nil nil nil)))
                                ;; Search for earlier token before event horizon
                                (let ((res-early (macher-agent-memory-search-zero-mem "UniqueTokenAlpha" orig-buf 2)))
                                  (expect res-early :to-match "Match near line 1")
                                  (expect res-early :to-match "UniqueTokenAlpha"))
                                ;; Search for active window token located past event horizon -> filtered out
                                (let ((res-active (macher-agent-memory-search-zero-mem "UniqueTokenGamma" orig-buf 2)))
                                  (expect res-active :to-match "^No matches found in history for:"))
                                ;; Verify search dispatch backend routing
                                (let ((macher-agent-search-backend-function #'macher-agent-memory-search-zero-mem))
                                  (expect (macher-agent-search-dispatch "UniqueTokenAlpha" orig-buf 2) :to-match "UniqueTokenAlpha")
                                  (expect (macher-agent-search-dispatch "UniqueTokenGamma" orig-buf 2) :to-match "^No matches found in history for:")))
                            (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                            (when (buffer-live-p tx-buf) (kill-buffer tx-buf))))))

          (describe "9. Stationary Calculated Parent Graph Snapshot"
                    (it "uses stationary parent graph snapshot at delegation time without re-indexing parent buffer"
                        (let* ((parent-buf (generate-new-buffer " *test-parent-snapshot-buf*"))
                               (child-buf (generate-new-buffer " *test-child-snapshot-buf*")))
                          (unwind-protect
                              (progn
                                (with-current-buffer parent-buf
                                  (insert "Turn 1: Database credentials stored under DB_SECRET_KEY\n")
                                  (insert "Turn 2: Service discovery on port 8080\n"))
                                ;; Persist parent interaction at delegation time
                                (let ((parent-graph (macher-agent-memory--persist-interaction parent-buf)))
                                  (expect parent-graph :not :to-be nil)
                                  (with-current-buffer child-buf
                                    (macher-agent--push-routing "task-snapshot-101" (buffer-name parent-buf)))
                                  ;; Modify parent buffer after delegation
                                  (with-current-buffer parent-buf
                                    (insert "Turn 3: Post-delegation live buffer modification\n"))
                                  ;; Child searching parent uses stationary snapshot directly
                                  (spy-on 'macher-agent-zero-mem--buffer-to-traces :and-call-through)
                                  (let ((res (macher-agent-memory-search-zero-mem "DB_SECRET_KEY" parent-buf 2)))
                                    (expect res :to-match "DB_SECRET_KEY")
                                    ;; Does not re-index the parent live buffer
                                    (expect 'macher-agent-zero-mem--buffer-to-traces :not :to-have-been-called))))
                            (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
                            (when (buffer-live-p child-buf) (kill-buffer child-buf))))))

          (describe "10. Autonomous Plugin Registration and Agnostic Core"
                    (it "registers and unregisters pipeline steps dynamically"
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

          (describe "11. Plugin State Isolation in Context Plugins"
                    (it "verifies macher-agent-context has no dedicated zero-mem slot and accessor is unbound"
                        (expect (fboundp 'macher-agent-context-zero-mem) :to-be nil)
                        (let ((ctx (make-macher-agent-context :id "test-slotless-ctx")))
                          (expect (macher-agent-context-p ctx) :to-be t)
                          (expect (macher-agent-context-plugins ctx) :to-be nil)
                          (expect (macher-agent-zero-mem-get-state ctx) :to-be nil)))

                    (it "reads and writes zero-mem state strictly inside context plugins plist"
                        (let ((ctx (make-macher-agent-context :id "test-ctx" :plugins '(:existing-key "val"))))
                          ;; Initial state
                          (expect (macher-agent-zero-mem-get-state ctx) :to-be nil)
                          ;; Set state via helper
                          (macher-agent-zero-mem-set-state ctx '(:traces ((:id 1 :text "node1"))))
                          ;; Verify getter retrieves the state
                          (expect (macher-agent-zero-mem-get-state ctx) :to-equal '(:traces ((:id 1 :text "node1"))))
                          ;; Verify state is stored under :zero-mem in plugins plist
                          (expect (plist-get (macher-agent-context-plugins ctx) :zero-mem)
                                  :to-equal '(:traces ((:id 1 :text "node1"))))
                          ;; Verify other plugin keys remain intact
                          (expect (plist-get (macher-agent-context-plugins ctx) :existing-key)
                                  :to-equal "val")
                          ;; Update state again
                          (let ((graph (macher-agent-zero-mem-build-graph
                                        (list '(:id 1 :text "Alpha node")
                                              '(:id 2 :text "Beta node")))))
                            (macher-agent-zero-mem-set-state ctx graph)
                            (expect (macher-agent-zero-mem-get-state ctx) :to-equal graph)
                            (expect (plist-get (macher-agent-context-plugins ctx) :zero-mem) :to-equal graph))))

                    (it "handles raw plist contexts seamlessly with get-state and set-state"
                        (let ((raw-ctx (list :id "raw-ctx-1" :zero-mem '(:graph "raw-graph"))))
                          (expect (macher-agent-zero-mem-get-state raw-ctx) :to-equal '(:graph "raw-graph"))
                          (let ((updated (macher-agent-zero-mem-set-state raw-ctx '(:graph "new-graph"))))
                            (expect updated :to-equal '(:graph "new-graph")))))

                    (it "persists interaction graph directly into context plugins via persist-interaction"
                        (let* ((buf (generate-new-buffer " *test-persist-plugins*"))
                               (ctx (make-macher-agent-context :id "persist-ctx")))
                          (unwind-protect
                              (progn
                                (with-current-buffer buf
                                  (setq-local macher-agent--persistent-context ctx)
                                  (insert "Turn 1: Server running on port 9000\n"))
                                (let ((graph (macher-agent-memory--persist-interaction buf)))
                                  (expect graph :not :to-be nil)
                                  ;; State must be in context plugins :zero-mem
                                  (expect (macher-agent-zero-mem-get-state ctx) :to-equal graph)
                                  (expect (plist-get (macher-agent-context-plugins ctx) :zero-mem) :to-equal graph)))
                            (when (buffer-live-p buf) (kill-buffer buf)))))

                    (it "retrieves stationary graph from parent context plugins during subagent parent context injection"
                        (let* ((parent-buf (generate-new-buffer " *test-parent-plugins-buf*"))
                               (child-buf (generate-new-buffer " *test-child-plugins-buf*"))
                               (parent-ctx (make-macher-agent-context :id "parent-ctx"))
                               (child-ctx (make-macher-agent-context :id "child-ctx")))
                          (unwind-protect
                              (progn
                                (with-current-buffer parent-buf
                                  (setq-local macher-agent--persistent-context parent-ctx)
                                  (insert "Turn 1: Database credentials stored under PROD_DB_SECRET\n")
                                  (insert "Turn 2: API gateway configuration\n"))
                                ;; Persist parent interaction to initialize parent context state
                                (let ((parent-graph (macher-agent-memory--persist-interaction parent-buf)))
                                  (expect (macher-agent-zero-mem-get-state parent-ctx) :to-equal parent-graph)
                                  (with-current-buffer child-buf
                                    (setq-local macher-agent--persistent-context child-ctx)
                                    (macher-agent--push-routing "task-plugins-101" (buffer-name parent-buf))
                                    (insert "Retrieve PROD_DB_SECRET from parent."))
                                  (let* ((state (make-macher-agent-transmission-state
                                                 :target-buffer child-buf
                                                 :directives nil))
                                         (updated-state (macher-agent-pipe--inject-parent-context
                                                         state child-buf nil nil nil))
                                         (dirs (macher-agent-transmission-state-directives updated-state)))
                                    (expect (length dirs) :to-be-greater-than 0)
                                    (let ((text (string-join dirs "\n\n")))
                                      (expect text :to-match "<parent_conversation_context>")
                                      (expect text :to-match "PROD_DB_SECRET")))))
                            (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
                            (when (buffer-live-p child-buf) (kill-buffer child-buf))))))))

(provide 'macher-agent-zero-mem-test)
;;; macher-agent-zero-mem-test.el ends here
