;;; macher-agent-skills-test.el --- Tests for macher-agent-skills -*- lexical-binding: t; -*-

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
(require 'macher-agent-test-setup)
(require 'macher-agent)
(require 'macher-agent-macher nil t)
(require 'macher-agent-vfs)
(require 'macher-agent-zero-mem)
(require 'macher-agent-test-harness)

(describe
 "macher-agent-skills"
 (before-all
  (when (fboundp 'macher-agent-zero-mem-uninstall)
    (macher-agent-zero-mem-uninstall))
  (setq macher-agent-search-backend-function #'macher-agent-search-glob))
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

 (describe
  "Orchestration Tools (skills/scripts/*.el)"
  (before-all
   ;; Load all tool scripts to define their variables for testing
   (let* ((file-name (or load-file-name buffer-file-name))
          (test-dir (if file-name (file-name-directory file-name) default-directory))
          (root-dir (or (locate-dominating-file test-dir "skills") test-dir))
          (scripts-dir (expand-file-name "skills/scripts" root-dir)))
     (dolist (script (directory-files scripts-dir t "\\.el$"))
       (load script nil t))))
  (it "guarantees list_buffers_in_workspace output perfectly matches context-tree buffer categorisation"
      (let*
          ((ctx
            (macher-agent--make-vfs-context
             :contents (list (macher-agent-vfs-make-entry "pure-worker-buf" "" "")
                             (macher-agent-vfs-make-entry "/external/path.txt" "" "")
                             (macher-agent-vfs-make-entry "/root/internal.txt" "" ""))))
           (list-tool-fn (gptel-tool-function macher-agent-list-buffers-in-workspace-tool)))

        (spy-on 'macher-agent--classify-file-path :and-call-fake
                (lambda (path &rest _)
                  (pcase path
                    ("pure-worker-buf" 'buffer)
                    ("/external/path.txt" 'external)
                    ("/root/internal.txt" 'file))))

        (with-macher-agent-mock-fsm ctx
                                    (let ((result (funcall list-tool-fn nil)))
                                      (expect result :to-match "pure-worker-buf")
                                      (expect result :to-match "/external/path\\.txt")
                                      (expect result :to-match "internal\\.txt")))))
  (it "commit_buffer registers virtual edits cleanly without destructive live buffer mutation"
      (let* ((buf (generate-new-buffer "live-commit-buf"))
             (ctx (macher-agent--make-vfs-context :contents (list (macher-agent-vfs-make-entry "live-commit-buf" "original content" "original content"))))
             (tool-fn (gptel-tool-function macher-agent-commit-buffer-tool)))
        (with-current-buffer buf
          (insert "original content"))
        (spy-on 'macher-agent--ensure-access)
        (with-macher-agent-mock-fsm ctx
                                    (let ((response (funcall tool-fn nil "live-commit-buf" "committed virtual content")))
                                      (expect response :to-match "SUCCESS")
                                      (expect (with-current-buffer buf (buffer-string)) :to-equal "original content")
                                      (expect (macher-agent--get-context-dirty-p ctx) :to-be t)
                                      (let ((entry (cl-find "live-commit-buf" (macher-agent--get-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                                        (expect entry :not :to-be nil)
                                        (expect (macher-agent-vfs-entry-curr entry) :to-equal "committed virtual content")
                                        (expect (macher-agent-vfs-entry-orig entry) :to-equal "original content"))))
        (kill-buffer buf)))
  (it "properly parses a JSON string into a vector for task delegation"
      (let* ((ctx (macher-agent--make-context))
             (callback-called nil)
             (callback (lambda (res) (setq callback-called res)))
             (json-tasks (vector (list :buffer_name "test-sub" :instructions "do work")))
             (tool-fn (gptel-tool-function (or (bound-and-true-p macher-agent-delegate-tasks-to-subagents-tool)
                                               macher-agent-delegate-tasks-to-subagents-tool)))
             (buf (get-buffer-create "test-sub")))
        
        (spy-on 'macher-agent-a2a-dispatch)
        (spy-on 'macher-agent--ensure-access)
        
        (with-macher-agent-mock-fsm ctx
                                    (funcall tool-fn callback :tasks json-tasks))
        
        (expect 'macher-agent-a2a-dispatch :to-have-been-called)
        (kill-buffer buf)))

  (it "reports an error if gptel-send aborts or fails silently"
      (let* ((buf (generate-new-buffer "*macher-agent: worker*"))
             (callback-called nil)
             (callback (lambda (msg) (setq callback-called msg))))

        ;; Simulate gptel-send firing and instantly triggering the post-response hook
        (spy-on 'gptel-send :and-call-fake
                (lambda ()
                  (with-current-buffer buf
                    (erase-buffer)
                    (run-hook-with-args 'gptel-post-response-functions (point-min) (point-max)))))

        (macher-agent-a2a-dispatch
         (list (macher-agent-make-a2a-payload
                :type 'SEND_MESSAGE
                :task-id "task-err"
                :payload "test"
                :metadata (list :buffer_name (buffer-name buf))))
         callback)
        
        (let ((res (if (vectorp callback-called) (aref callback-called 0) callback-called)))
          (expect (plist-get res :status) :to-equal 'error)
          (expect (plist-get res :error) :to-match "stopped silently"))
        (kill-buffer buf)))

  (it "correctly aggregates results from multiple event-driven sub-agents"
      (let* ((buf1 (generate-new-buffer "worker1"))
             (buf2 (generate-new-buffer "worker2"))
             (callback-called nil)
             (callback (lambda (msg) (setq callback-called msg))))
        
        ;; Mock the dispatcher to instantly return a success payload rather than firing the network
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda ()
                     (let* ((task-id (bound-and-true-p macher-agent--current-task-id))
                            (cb (when task-id (gethash task-id macher-agent--pending-callbacks))))
                       (when cb
                         (funcall cb (list :status 'success :data (format "Output from %s" (buffer-name)))))))))
          (macher-agent-a2a-dispatch
           (list (macher-agent-make-a2a-payload
                  :type 'SEND_MESSAGE
                  :task-id "t1"
                  :payload "do w1"
                  :metadata (list :buffer_name "worker1"))
                 (macher-agent-make-a2a-payload
                  :type 'SEND_MESSAGE
                  :task-id "t2"
                  :payload "do w2"
                  :metadata (list :buffer_name "worker2")))
           callback))
        
        (expect (length callback-called) :to-equal 2)
        (expect (plist-get (aref callback-called 0) :status) :to-equal 'success)
        (expect (plist-get (aref callback-called 0) :data) :to-match "Output from worker1")
        (expect (plist-get (aref callback-called 1) :data) :to-match "Output from worker2")
        (kill-buffer buf1)
        (kill-buffer buf2)))

  (it "ensures target buffer exists when using write_buffer_in_workspace to support patch UI"
      (let* ((ctx (macher-agent--make-vfs-context :contents (list (macher-agent-vfs-make-entry "dummy" "dummy" "dummy"))))
             (tool-fn (gptel-tool-function macher-agent-write-buffer-in-workspace-tool)))
        (unwind-protect
            (progn
              (with-macher-agent-mock-fsm ctx
                                          (funcall tool-fn nil "*new-virtual-asset*" "Ghost content"))
              (expect (cl-find "*new-virtual-asset*" (macher-agent--get-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :not :to-be nil)
              (let ((contents (cl-find "*new-virtual-asset*" (macher-agent--get-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                (expect (macher-agent-vfs-entry-curr contents) :to-equal "Ghost content")))
          (when-let* ((b (get-buffer "*new-virtual-asset*")))
            (kill-buffer b)))))
  
  (it "rejects fuzzy security matching in read_buffer_in_workspace"
      (let* ((ctx (macher-agent--make-vfs-context :contents (list (macher-agent-vfs-make-entry "*scratch*" "" "content"))))
             (tool-fn (gptel-tool-function macher-agent-read-buffer-in-workspace-tool)))
        (with-macher-agent-mock-fsm ctx
                                    (let ((result (funcall tool-fn nil "scratch")))
                                      (expect result :to-match "SECURITY ERROR.*scratch.*")))))

  (it "submit_task_result triggers callback from registry and flags completion"
      (let* ((ctx (macher-agent--make-context))
             (buf (generate-new-buffer "worker-buf"))
             (tool-fn (gptel-tool-function macher-agent-submit-task-result-tool))
             (task-id "skill-submit-task-1")
             (callback-data nil))
        (expect (gptel-tool-description macher-agent-submit-task-result-tool)
                :to-match "CRITICAL DIRECTIVE: You MUST use the `submit_task_result` tool")
        (puthash task-id (lambda (res) (setq callback-data res)) macher-agent--pending-callbacks)
        (with-current-buffer buf
          (macher-agent--push-routing task-id "originator-buf")
          (with-macher-agent-mock-fsm ctx
                                      (expect (funcall tool-fn nil "My final answer")
                                              :to-equal "SUCCESS: Result submitted. STOP NOW."))
          (expect (macher-agent-transit-payload-type callback-data) :to-equal 'ARTIFACT_UPDATE)
          (expect (plist-get (macher-agent-transit-payload-payload callback-data) :message) :to-equal "My final answer")
          (expect macher-agent-task-finished :to-be t)
          (expect (funcall tool-fn nil "Second final answer") :to-equal "ERROR: Task has already been submitted."))
        (kill-buffer buf)))
  
  (it "write_buffer_in_workspace registers a virtual edit safely"
      (let* ((ctx (macher-agent--make-vfs-context :contents (list (macher-agent-vfs-make-entry "test-buf" "orig" "orig"))))
             (tool-fn (gptel-tool-function macher-agent-write-buffer-in-workspace-tool)))
        (with-macher-agent-mock-fsm ctx
                                    (let* ((response (funcall tool-fn nil "test-buf" "New virtual content")))
                                      (expect response :to-match "SUCCESS")
                                      (expect (macher-agent--get-context-dirty-p ctx) :to-be t)
                                      (expect (macher-agent-vfs-entry-curr (cl-find "test-buf" (macher-agent--get-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)) :to-equal "New virtual content")))))

  (it "multi_edit_buffer_in_workspace uses a decoupled deterministic scratchpad"
      (let* ((ctx (macher-agent--make-vfs-context :contents (list (macher-agent-vfs-make-entry "test-file.rs" "line1\nline2" "line1\nline2"))))
             (tool-fn (gptel-tool-function macher-agent-multi-edit-buffer-in-workspace-tool)))
        (with-macher-agent-mock-fsm ctx
                                    (let* ((edits (vector (list :old_text "line2" :new_text "line3")))
                                           (response (funcall tool-fn nil "test-file.rs" edits)))
                                      (expect response :to-match "SUCCESS")
                                      (let ((contents (cl-find "test-file.rs" (macher-agent--get-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                                        (expect (macher-agent-vfs-entry-curr contents) :to-equal "line1\nline3"))))))

  (it "allows multi_edit_buffer_in_workspace after reading a live buffer"
      (let* ((buf (generate-new-buffer "read-then-edit.txt"))
             (ctx (macher-agent--make-vfs-context :contents nil))
             (read-fn (gptel-tool-function macher-agent-read-buffer-in-workspace-tool))
             (edit-fn (gptel-tool-function macher-agent-multi-edit-buffer-in-workspace-tool)))
        (with-current-buffer buf
          (insert "hello world"))
        (spy-on 'macher-agent--ensure-access)
        
        (with-macher-agent-mock-fsm ctx
                                    (funcall read-fn nil "read-then-edit.txt")
                                    (let* ((edits (vector (list :old_text "hello" :new_text "goodbye")))
                                           (response (funcall edit-fn nil "read-then-edit.txt" edits)))
                                      (expect response :to-match "SUCCESS")
                                      (let ((contents (cl-find "read-then-edit.txt" (macher-agent--get-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                                        (expect contents :not :to-be nil)
                                        (when contents
                                          (expect (macher-agent-vfs-entry-curr contents) :to-equal "goodbye world")))))
        (kill-buffer buf)))

  (it "ensures untouched scoped buffer produces zero diff after read_buffer_in_workspace"
      (let* ((buf (generate-new-buffer "unmodified-scoped-buf"))
             (ctx (macher-agent--make-vfs-context :contents nil))
             (read-fn (gptel-tool-function macher-agent-read-buffer-in-workspace-tool)))
        (with-current-buffer buf
          (insert "initial unchanged content"))
        (spy-on 'macher-agent--ensure-access)
        (with-macher-agent-mock-fsm ctx
                                    (let ((res (funcall read-fn nil "unmodified-scoped-buf")))
                                      (expect res :to-equal "initial unchanged content")
                                      (let ((entry (cl-find "unmodified-scoped-buf" (macher-agent--get-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                                        (expect entry :not :to-be nil)
                                        (expect (macher-agent-vfs-entry-orig entry) :to-equal "initial unchanged content")
                                        (expect (macher-agent-vfs-entry-curr entry) :to-equal "initial unchanged content")
                                        (expect (macher-agent-vfs-entry-modified-p entry) :to-be nil))))
        (kill-buffer buf)))

  (it "picks up external live buffer modifications on subsequent read_buffer_in_workspace calls"
      (let* ((buf (generate-new-buffer "live-mod-buf"))
             (ctx (macher-agent--make-vfs-context :contents nil))
             (read-fn (gptel-tool-function macher-agent-read-buffer-in-workspace-tool)))
        (with-current-buffer buf
          (insert "initial buffer text"))
        (spy-on 'macher-agent--ensure-access)
        (with-macher-agent-mock-fsm ctx
                                    (let ((res1 (funcall read-fn nil "live-mod-buf")))
                                      (expect res1 :to-equal "initial buffer text")
                                      ;; Externally modify the live buffer
                                      (with-current-buffer buf
                                        (erase-buffer)
                                        (insert "externally updated buffer text"))
                                      ;; Subsequent read must synchronise and return the updated content
                                      (let ((res2 (funcall read-fn nil "live-mod-buf")))
                                        (expect res2 :to-equal "externally updated buffer text")
                                        (let ((entry (cl-find "live-mod-buf" (macher-agent--get-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                                          (expect (macher-agent-vfs-entry-curr entry) :to-equal "externally updated buffer text")
                                          (expect (macher-agent-vfs-entry-orig entry) :to-equal "externally updated buffer text")))))
        (kill-buffer buf)))

  (it "synchronises context seamlessly when interleaving macher-agent and macher tools"
      (let* ((proj-dir (file-name-as-directory (expand-file-name "tests/fixtures/interleave-proj")))
             (file-path (concat proj-dir "interleave.txt")))
        (make-directory proj-dir t)
        (with-temp-file file-path (insert "Initial file text"))
        (let* ((ws (cons 'directory proj-dir))
               (ctx (make-macher-agent-context
                     :project-root proj-dir
                     :plugins (list :vfs (list :contents nil))))
               (agent-write-fn (gptel-tool-function macher-agent-write-buffer-in-workspace-tool))
               (agent-read-fn (gptel-tool-function macher-agent-read-buffer-in-workspace-tool)))
          (cl-letf (((symbol-function 'macher--tool-write-file)
                     (lambda (_c p cont)
                       (macher-agent-context-update ctx p cont))))
            (with-macher-agent-mock-fsm ctx
                                        (funcall agent-write-fn nil "interleave.txt" "VFS Step 1: Agent Write")
                                        (macher--tool-write-file ctx "interleave.txt" "VFS Step 2: Macher Write")
                                        (let ((res (funcall agent-read-fn nil "interleave.txt")))
                                          (expect res :to-equal "VFS Step 2: Macher Write")
                                          (expect (length (macher-agent--get-context-contents ctx)) :to-equal 1))))
          (delete-file file-path)
          (delete-directory proj-dir))))

  (it "searches VFS content written by write_buffer_in_workspace using search_in_workspace"
      (let* ((ws (make-macher-agent-workspace :project-root default-directory))
             (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
             (write-fn (gptel-tool-function macher-agent-write-buffer-in-workspace-tool))
             (cmd-fn (or (get 'macher-agent-search-in-workspace-tool 'command-fn)
                         (get 'macher-agent-tool-search-in-workspace 'command-fn))))
        (spy-on 'macher-agent--vfs-verify-clean-merge)
        (spy-on 'macher-agent--vfs-sync-baseline)
        (spy-on 'shell-command-to-string :and-call-fake
                (lambda (cmd)
                  (if (string-match-p "unique-vfs-search-token" cmd)
                      "vfs-search-target.txt:1:unique-vfs-search-token line\n"
                    "No matches found.")))
        (with-macher-agent-mock-fsm ctx
                                    (funcall write-fn nil "vfs-search-target.txt" "unique-vfs-search-token line")
                                    (let ((res (funcall cmd-fn '(:pattern "unique-vfs-search-token") ctx default-directory)))
                                      (expect res :to-match "vfs-search-target\\.txt")
                                      (expect res :to-match "unique-vfs-search-token line")))))

  (it "ensures search_in_workspace uses -- before the pattern in the rg CLI invocation"
      (let* ((ws (make-macher-agent-workspace :project-root default-directory))
             (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
             (cmd-fn (or (get 'macher-agent-search-in-workspace-tool 'command-fn)
                         (get 'macher-agent-tool-search-in-workspace 'command-fn)))
             (captured-cmd nil))
        (spy-on 'macher-agent--vfs-verify-clean-merge)
        (spy-on 'macher-agent--vfs-sync-baseline)
        (spy-on 'shell-command-to-string :and-call-fake
                (lambda (cmd)
                  (setq captured-cmd cmd)
                  "No matches found."))
        (with-macher-agent-mock-fsm ctx
                                    (funcall cmd-fn '(:pattern "-v --some-flag") ctx default-directory)
                                    (expect captured-cmd :to-match "rg --line-number --color=never --max-columns=150 -- '-v --some-flag' \\."))))

  (it "list_available_tools filters tools to only perception, collaboration, event, execution categories and includes workspace tools"
      (let* ((global-reg (make-hash-table :test 'equal))
             (tool-perception (gptel-make-tool :name "tool-perc" :description "perception tool" :category "perception"))
             (tool-collab (gptel-make-tool :name "tool-collab" :description "collaboration tool" :category "collaboration"))
             (tool-event (gptel-make-tool :name "tool-event" :description "event tool" :category "event"))
             (tool-exec (gptel-make-tool :name "tool-exec" :description "execution tool" :category "execution"))
             (tool-other (gptel-make-tool :name "tool-other" :description "conversation tool" :category "conversation"))
             (tool-none (gptel-make-tool :name "tool-none" :description "uncategorized tool" :category "misc"))
             (tool-ws (gptel-make-tool :name "tool-workspace" :description "workspace tool" :category "perception"))
             (ws (make-macher-agent-workspace :project-root default-directory))
             (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
             (list-fn (gptel-tool-function macher-agent-list-available-tools-tool)))
        (puthash "tool-perc" tool-perception global-reg)
        (puthash "tool-collab" tool-collab global-reg)
        (puthash "tool-event" tool-event global-reg)
        (puthash "tool-exec" tool-exec global-reg)
        (puthash "tool-other" tool-other global-reg)
        (puthash "tool-none" tool-none global-reg)
        (puthash "tool-workspace" tool-ws (macher-agent-workspace-tools-registry ctx))
        (let ((macher-agent-tools-registry global-reg))
          (with-macher-agent-mock-fsm ctx
                                      (let ((res (funcall list-fn nil)))
                                        (expect res :to-match "tool-perc")
                                        (expect res :to-match "tool-collab")
                                        (expect res :to-match "tool-event")
                                        (expect res :to-match "tool-exec")
                                        (expect res :to-match "tool-workspace")
                                        (expect res :not :to-match "tool-other")
                                        (expect res :not :to-match "tool-none"))))))

  (it "searches conversation history via glob search"
      (let ((buf (generate-new-buffer "test-conv-glob")))
        (with-current-buffer buf
          (insert "Line 1: system start\nLine 2: important keyword found\nLine 3: system end\n"))
        (let ((res (macher-agent-search-glob "keyword" buf 1)))
          (expect res :to-match "Match near line 2")
          (expect res :to-match "important keyword found"))
        (let ((res-none (macher-agent-search-glob "nonexistent" buf 1)))
          (expect res-none :to-match "No matches found in history for: nonexistent"))
        (kill-buffer buf)))

  (it "converts buffer lines into zero-mem trace plists"
      (let ((buf (generate-new-buffer "test-conv-traces")))
        (with-current-buffer buf
          (insert "First trace line\n\nSecond trace line\n"))
        (let ((traces (macher-agent-zero-mem--buffer-to-traces buf)))
          (expect (length traces) :to-equal 2)
          (expect (plist-get (nth 0 traces) :text) :to-equal "First trace line")
          (expect (plist-get (nth 0 traces) :timestamp) :to-equal 1.0)
          (expect (plist-get (nth 1 traces) :text) :to-equal "Second trace line")
          (expect (plist-get (nth 1 traces) :timestamp) :to-equal 3.0))
        (kill-buffer buf)))

  (it "searches conversation history via zero-mem PageRank search"
      (let ((buf (generate-new-buffer "test-conv-zeromem")))
        (with-current-buffer buf
          (insert "Trace 1: macher-agent initialization\nTrace 2: gptel-bridge configuration\nTrace 3: random text\n"))
        (let ((res (macher-agent-memory-search-zero-mem "macher-agent" buf 2)))
          (expect res :to-match "Match near line 1")
          (expect res :to-match "macher-agent initialization"))
        (kill-buffer buf)))

  (it "defaults macher-agent-search-backend-function to macher-agent-search-glob"
      (expect (default-value 'macher-agent-search-backend-function) :to-equal #'macher-agent-search-glob))

  (it "dispatches search based on macher-agent-search-backend-function configuration"
      (let ((buf (generate-new-buffer "test-conv-dispatch"))
            (macher-agent-search-backend-function #'macher-agent-search-glob))
        (with-current-buffer buf
          (insert "Header text\nTarget Query inside history\nFooter text\n"))
        (spy-on 'macher-agent-search-glob :and-call-through)
        (spy-on 'macher-agent-memory-search-zero-mem :and-call-through)
        
        ;; Test glob backend
        (let ((res-glob (macher-agent-search-dispatch "query" buf 2)))
          (expect 'macher-agent-search-glob :to-have-been-called)
          (expect res-glob :to-match "Target Query inside history"))
        
        ;; Test zero-mem backend
        (setq macher-agent-search-backend-function #'macher-agent-memory-search-zero-mem)
        (let ((res-zm (macher-agent-search-dispatch "query" buf 2)))
          (expect 'macher-agent-memory-search-zero-mem :to-have-been-called)
          (expect res-zm :to-match "Target Query inside history"))
        
        ;; Test dead buffer error handling
        (kill-buffer buf)
        (expect (macher-agent-search-dispatch "query" buf 2) :to-match "Error: Cannot locate original conversation buffer.")))

  (it "executes search_conversation_history tool via dispatcher without inline loops or persistent global mutation"
      (let* ((buf (generate-new-buffer "test-conv-tool"))
             (ctx (macher-agent--make-context))
             (mock-fsm (if (fboundp 'gptel-make-fsm)
                           (gptel-make-fsm :info (list :buffer buf :macher-agent-context ctx))
                         (list :buffer buf :macher-agent-context ctx)))
             (macher-agent--active-fsm mock-fsm)
             (cmd-fn (or (get 'macher-agent-search-conversation-history-tool 'command-fn)
                         (get 'macher-agent-tool-search-conversation-history 'command-fn)))
             (macher-agent-search-backend-function #'macher-agent-search-glob))
        (with-current-buffer buf
          (insert "Line 1: hello\nLine 2: target-match\nLine 3: world\n"))
        (spy-on 'macher-agent-search-dispatch :and-call-through)
        (let ((res (funcall cmd-fn '(:query "target-match" :context_lines 1) ctx default-directory)))
          (expect 'macher-agent-search-dispatch :to-have-been-called)
          (expect res :to-match "target-match"))
        (kill-buffer buf)))

  (it "retrieves conversation buffer from context in search_conversation_history tool"
      (let* ((buf (generate-new-buffer "test-conv-ctx"))
             (mock-fsm (if (fboundp 'gptel-make-fsm)
                           (gptel-make-fsm :info (list :buffer buf))
                         (list :buffer buf)))
             (ctx (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root default-directory)
                                                  :contents nil))
             (cmd-fn (or (get 'macher-agent-search-conversation-history-tool 'command-fn)
                         (get 'macher-agent-tool-search-conversation-history 'command-fn)))
             (macher-agent-search-backend-function #'macher-agent-search-glob)
             (macher-agent--active-fsm nil))
        (with-current-buffer buf
          (insert "Line 1: foo\nLine 2: context-buffer-match\nLine 3: bar\n"))
        (setf (macher-agent-context-plugins ctx)
              (plist-put (copy-sequence (macher-agent-context-plugins ctx)) :fsm mock-fsm))
        (let ((res (funcall cmd-fn '(:query "context-buffer-match" :context_lines 1) ctx default-directory)))
          (expect res :to-match "context-buffer-match"))
        (kill-buffer buf)))

  (it "handles zero-length matches and invalid-regexp safely in search_glob"
      (let ((buf (generate-new-buffer "test-conv-edge")))
        (with-current-buffer buf
          (insert "Line 1\nLine 2\n"))
        ;; Test zero-length match (e.g., "^" or ".*") does not infinite loop
        (let ((res (macher-agent-search-glob "^" buf 1)))
          (expect res :to-match "Match near line 1"))
        ;; Test invalid regexp handling
        (let ((res-err (macher-agent-search-glob "[unclosed" buf 1)))
          (expect res-err :to-match "Error: Invalid regular expression"))
        (kill-buffer buf)))

  )

 (describe "Agent Skills (macher-agent-skills.el)"
           (before-each
            (let* ((ws (make-macher-agent-workspace :project-root "/mock/proj"))
                   (ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))
              (puthash (expand-file-name "/mock/proj") ctx macher-agent-active-workspaces)
              (setq-local macher-agent--persistent-context ctx)))
           
           (it "parses SKILL.md files correctly extracting frontmatter and markdown body"
               (let* ((parsed (macher-agent-parse-skill-file "tests/fixtures/skills/global/SKILL.md")))
                 (expect (plist-get parsed :name) :to-equal "mock-skill")
                 (expect (plist-get parsed :name-sym) :to-equal 'mock-skill)
                 (expect (plist-get parsed :description) :to-equal "A mock skill for testing")
                 (expect (plist-get parsed :allowed-tools) :to-equal '("mock-tool-1" "mock-tool-2"))
                 (expect (plist-get parsed :body) :to-equal "This is the system prompt for the mock skill.\nIt spans multiple lines.")))

           (it "extracts boot-directive from SKILL.md frontmatter"
               (let* ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                               (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces)))
                      (vfs-file "tests/fixtures/skills/global/SKILL.md"))
                 (macher-agent--set-context-contents
                  ctx
                  (list (macher-agent-vfs-make-entry
                         vfs-file
                         "original"
                         "---\nname: boot-skill\ndescription: Skill with boot directive\nboot-directive: Perform initial setup before executing.\n---\nSkill body content")))
                 (let ((parsed (macher-agent-parse-skill-file vfs-file ctx)))
                   (expect (plist-get parsed :name) :to-equal "boot-skill")
                   (expect (plist-get parsed :boot-directive) :to-equal "Perform initial setup before executing."))))

           (it "persists boot-directive into gptel--known-presets and sets buffer-local macher-agent--boot-directive when applying preset"
               (let* ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                               (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces)))
                      (workspace (macher-agent-context-workspace ctx)))
                 (setf (alist-get 'boot-preset (macher-agent-workspace-skills-alist workspace))
                       '(:system "System prompt" :description "Boot Preset" :boot-directive "Initial boot directive text"))
                 (macher-agent-initialize-skills ctx)
                 (let ((spec (alist-get 'boot-preset gptel--known-presets)))
                   (expect (plist-get spec :boot-directive) :to-equal "Initial boot directive text"))
                 (let ((test-buf (generate-new-buffer "test-boot-preset-buf")))
                   (with-current-buffer test-buf
                     (if (boundp 'gptel--known-presets)
                         (setq gptel--known-presets (copy-tree gptel--known-presets))
                       (setq-local gptel--known-presets (copy-tree gptel--known-presets)))
                     (macher-agent--apply-preset '(boot-preset))
                     (expect macher-agent--boot-directive :to-equal "Initial boot directive text"))
                   (kill-buffer test-buf))))

           (it "sets buffer-local macher-agent--boot-directive via macher-agent-use-skill, macher-agent-add-subagent, and macher-agent--apply-payload-locally"
               (let* ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                               (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces)))
                      (workspace (macher-agent-context-workspace ctx)))
                 (setf (alist-get 'boot-preset (macher-agent-workspace-skills-alist workspace))
                       '(:system "System prompt" :description "Boot Preset" :boot-directive "Initial boot directive text"))
                 (macher-agent-initialize-skills ctx)
                 ;; Test macher-agent-use-skill
                 (let ((use-buf (generate-new-buffer "test-use-skill-buf")))
                   (macher-agent-use-skill 'boot-preset use-buf)
                   (with-current-buffer use-buf
                     (expect macher-agent--boot-directive :to-equal "Initial boot directive text"))
                   (kill-buffer use-buf))
                 ;; Test macher-agent-add-subagent
                 (let ((sub-buf (macher-agent-add-subagent "test-subagent-boot-buf" '(boot-preset))))
                   (with-current-buffer sub-buf
                     (expect macher-agent--boot-directive :to-equal "Initial boot directive text"))
                   (kill-buffer sub-buf))
                 ;; Test macher-agent--apply-payload-locally
                 (let ((local-buf (generate-new-buffer "test-apply-payload-buf")))
                   (with-current-buffer local-buf
                     (macher-agent--apply-payload-locally '(:boot-directive "Direct boot directive"))
                     (expect macher-agent--boot-directive :to-equal "Direct boot directive"))
                   (kill-buffer local-buf))))

           (it "prioritises virtual edits inside the VFS when parsing SKILL.md"
               (let* ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                               (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces)))
                      (vfs-file "tests/fixtures/skills/global/SKILL.md"))
                 (macher-agent--set-context-contents 
                  ctx 
                  (list (macher-agent-vfs-make-entry 
                         vfs-file 
                         "original content on disk" 
                         "---\nname: virtual-skill\ndescription: A virtual test skill\n---\nThis body is completely virtual.")))
                 (let ((parsed (macher-agent-parse-skill-file vfs-file ctx)))
                   (expect (plist-get parsed :name) :to-equal "virtual-skill")
                   (expect (plist-get parsed :body) :to-equal "This body is completely virtual."))))

           (it "resolves global skill tools by loading their script if not registered"
               (let* ((loaded-tool-object (gptel-make-tool :name "mock-tool-load" :category "test")))
                 (setq mock-tool-load-global loaded-tool-object)
                 (spy-on 'file-exists-p :and-return-value t)
                 (spy-on 'insert-file-contents :and-call-fake (lambda (f) (insert "(setq mock-tool-load mock-tool-load-global)")))
                 (let ((resolved (macher-agent-resolve-tool "mock-tool-load" nil "tests/fixtures/skills/global/")))
                   (expect resolved :to-equal loaded-tool-object))))

           (it "refuses to load workspace skill tools (security context)"
               (let* ((mock-script-dir (expand-file-name "tests/fixtures/skills/workspace/scripts"))
                      (mock-script-path (expand-file-name "workspace-tool-1.el" mock-script-dir)))
                 ;; Setup mock script
                 (make-directory mock-script-dir t)
                 (with-temp-file mock-script-path
                   (insert "(setq workspace-tool-1 'workspace-loaded)"))
                 
                 ;; Test workspace parsing logic
                 (let ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                                (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces))))
                   (macher-agent--load-skill-from-path "tests/fixtures/skills/workspace/" ctx)
                   (let* ((workspace (macher-agent-context-workspace ctx))
                          (skill-meta (alist-get 'workspace-skill (macher-agent-workspace-skills-alist workspace))))
                     (expect (plist-get skill-meta :context-dir) :to-be nil)))
                 
                 ;; Resolution should fail to load because context-dir is nil,
                 ;; returning the raw string fallback instead of a loaded tool object.
                 (let ((resolved (macher-agent-resolve-tool "workspace-tool-1" nil nil)))
                   (expect resolved :to-equal "workspace-tool-1"))
                 
                 (delete-directory mock-script-dir t)))

           (it "verifies tool resolution hierarchy (workspace shadows package tools)"
               (let* ((pkg-dir (make-temp-file "macher-pkg" t))
                      (ws-dir (make-temp-file "macher-ws" t))
                      (pkg-scripts (expand-file-name "scripts" pkg-dir))
                      (ws-scripts (expand-file-name "scripts" ws-dir))
                      (ws-a (gptel-make-tool :name "tool-a" :category "test1"))
                      (pkg-b (gptel-make-tool :name "tool-b" :category "test2")))
                 (setq mock-ws-a-global ws-a)
                 (setq mock-pkg-b-global pkg-b)
                 (make-directory pkg-scripts t)
                 (make-directory ws-scripts t)
                 ;; Package provides tool-a and tool-b
                 (with-temp-file (expand-file-name "tool-a.el" pkg-scripts) (insert "(setq tool-a 'pkg-a)"))
                 (with-temp-file (expand-file-name "tool-b.el" pkg-scripts) (insert "(setq tool-b mock-pkg-b-global)"))
                 ;; Workspace overrides tool-a
                 (with-temp-file (expand-file-name "tool-a.el" ws-scripts) (insert "(setq tool-a mock-ws-a-global)"))
                 
                 ;; Clear registry
                 (let* ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                                 (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces)))
                        (ws (macher-agent-context-workspace ctx)))
                   (clrhash (macher-agent-workspace-tools-registry ws)))
                 
                 ;; Resolve pkg first, then workspace shadows
                 (let* ((res-pkg-b (macher-agent-resolve-tool "tool-b" nil pkg-dir))
                        (res-ws-a (macher-agent-resolve-tool "tool-a" nil ws-dir)))
                   (expect res-pkg-b :to-equal pkg-b)
                   (expect res-ws-a :to-equal ws-a))
                 
                 (delete-directory pkg-dir t)
                 (delete-directory ws-dir t)))
           
           (it "applies skill tools correctly into gptel-tools when selected"
               (let* ((gptel-tools nil)
                      (gptel--known-presets nil)
                      (gptel-directives nil)
                      (mock-tool-obj (if (fboundp 'gptel-make-tool)
                                         (gptel-make-tool :name "the_tool" :function (lambda () nil) :description "A tool")
                                       'the-tool)))
                 (spy-on 'gptel-tool-p :and-return-value t)
                 (let* ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                                 (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces)))
                        (workspace (macher-agent-context-workspace ctx)))
                   (puthash "selected-tool" mock-tool-obj (macher-agent-workspace-tools-registry workspace))
                   (setf (alist-get 'test-preset (macher-agent-workspace-skills-alist workspace))
                         (list :description "test" :system "test system" :tools (list mock-tool-obj) :context-dir nil))
                   
                   (with-temp-buffer
                     (let ((gptel--known-presets nil))
                       (macher-agent-initialize-skills ctx)
                       (let ((preset-def (buffer-local-value 'gptel--known-presets (current-buffer))))
                         (setq preset-def (alist-get 'test-preset preset-def))
                         (expect preset-def :not :to-be nil)
                         (expect (plist-get preset-def :tools) :to-equal '(:append ("the_tool")))))))))

           (it "merges and applies buffer-local macher-agent-presets during composed skill evaluation"
               (let* ((gptel-tools nil)
                      (gptel--known-presets nil)
                      (gptel-directives nil)
                      (mock-tool-obj (if (fboundp 'gptel-make-tool)
                                         (gptel-make-tool :name "the_tool" :function (lambda () nil) :description "A tool")
                                       'the-tool)))
                 (spy-on 'gptel-tool-p :and-return-value t)
                 (let* ((ctx (or (bound-and-true-p macher-agent--persistent-context)
                                 (gethash (expand-file-name "/mock/proj") macher-agent-active-workspaces)))
                        (workspace (macher-agent-context-workspace ctx)))
                   (puthash "selected-tool" mock-tool-obj (macher-agent-workspace-tools-registry workspace))
                   (setf (alist-get 'test-preset (macher-agent-workspace-skills-alist workspace))
                         (list :description "test" :system "test system" :tools (list mock-tool-obj) :context-dir nil))
                   
                   (with-temp-buffer
                     (let ((gptel--known-presets nil))
                       (macher-agent-initialize-skills ctx)
                       (setq-local macher-agent-presets '(test-preset))
                       (setq-local gptel-tools nil)
                       (let ((payload (macher-agent-compose-payload 
                                       (list :model nil :system nil :temperature nil :max-tokens nil :tools nil :known-presets gptel--known-presets)
                                       '(test-preset))))
                         (macher-agent--apply-payload-locally payload))
                       (expect gptel-tools :not :to-be nil)
                       (expect (car gptel-tools) :to-equal mock-tool-obj))))))

           (it "expands org-macros in SKILL.md body"
               (let* ((parsed (macher-agent-parse-skill-file "tests/fixtures/skills/macro-skill/SKILL.md")))
                 (expect (plist-get parsed :body) :to-match "Version: 0.1.0")))

           (it "creates a preset when allowed-tools is provided"
               (let* ((mock-dir (make-temp-file "macher-test-skills-preset" t))
                      (skill-dir (expand-file-name "test-skill" mock-dir)))
                 (make-directory skill-dir t)
                 (with-temp-file (expand-file-name "SKILL.md" skill-dir)
                   (insert "---\nname: my-preset\ndescription: test\nallowed-tools:\n  - some-tool\nmodel: gpt-4o\n---\nPreset body"))
                 (spy-on 'macher-agent-resolve-tool :and-return-value "some-tool")
                 (with-temp-buffer
                   (let ((gptel-directives nil)
                         (gptel--known-presets nil))
                     (macher-agent-initialize-skills nil mock-dir)
                     
                     (let ((preset-def (alist-get 'my-preset gptel--known-presets)))
                       (expect preset-def :not :to-be nil)
                       (expect (plist-get preset-def :system) :to-equal "Preset body")
                       (expect (plist-get preset-def :model) :to-equal 'gpt-4o)
                       (expect (plist-get preset-def :tools) :to-equal '(:append ("some-tool"))))
                     (delete-directory mock-dir t)))))

           (it "injects directly into gptel-directives when allowed-tools is omitted"
               (let* ((mock-dir (make-temp-file "macher-test-skills-directive" t))
                      (skill-dir (expand-file-name "test-skill" mock-dir)))
                 (make-directory skill-dir t)
                 (with-temp-file (expand-file-name "SKILL.md" skill-dir)
                   (insert "---\nname: my-directive\n---\nDirective body"))
                 (with-temp-buffer
                   (let ((gptel-directives nil)
                         (gptel--known-presets nil))
                     (macher-agent-initialize-skills nil mock-dir)
                     (expect (alist-get 'my-directive gptel-directives) :to-equal "Directive body")
                     (let ((preset-def (alist-get 'my-directive gptel--known-presets)))
                       (expect preset-def :not :to-be nil)
                       (expect (plist-get preset-def :system) :to-equal "Directive body"))
                     (delete-directory mock-dir t)))))

           (it "stores ptc-primitives in gptel--known-presets and composes them into payload"
               (let* ((mock-dir (make-temp-file "macher-test-skills-ptc" t))
                      (skill-dir (expand-file-name "test-skill" mock-dir)))
                 (make-directory skill-dir t)
                 (with-temp-file (expand-file-name "SKILL.md" skill-dir)
                   (insert "---\nname: my-ptc-preset\ndescription: test ptc\nptc-primitives:\n  - spawn_subagent\n  - delegate_tasks\n---\nPTC preset body"))
                 (with-temp-buffer
                   (let ((gptel-directives nil)
                         (gptel--known-presets nil))
                     (macher-agent-initialize-skills nil mock-dir)
                     (let ((preset-def (alist-get 'my-ptc-preset gptel--known-presets)))
                       (expect preset-def :not :to-be nil)
                       (expect (plist-get preset-def :ptc-primitives) :to-equal '(spawn_subagent delegate_tasks))
                       (let ((composed (macher-agent-compose-payload
                                        (list :known-presets gptel--known-presets)
                                        '(my-ptc-preset))))
                         (expect (plist-get composed :ptc-primitives) :to-equal '(spawn_subagent delegate_tasks))))
                     (delete-directory mock-dir t)))))

           (describe "Skills Media Support"
                     (it "detects media file extensions"
                         (expect (macher-agent-media-file-p "image.png") :to-be-truthy)
                         (expect (macher-agent-media-file-p "photo.jpg") :to-be-truthy)
                         (expect (macher-agent-media-file-p "code.el") :to-be nil))))

  (describe "Canonical Context and Direct Plugin Access in Skills Scripts"
    (it "verifies all skills/scripts and presets files contain no occurrences of obsolete context helpers"
      (let* ((file-name (or load-file-name buffer-file-name))
             (test-dir (if file-name (file-name-directory file-name) default-directory))
             (root-dir (or (locate-dominating-file test-dir "skills") test-dir))
             (scripts-dir (expand-file-name "skills/scripts" root-dir))
             (script-files (directory-files scripts-dir t "\\.el$"))
             (preset-file (expand-file-name "macher-agent-presets.el" root-dir))
             (all-files (cons preset-file script-files)))
        (dolist (file all-files)
          (when (file-exists-p file)
            (let ((content (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))))
              (expect (string-match-p "macher-agent--get-context-data" content) :to-be nil)
              (expect (string-match-p "macher-agent--set-context-data" content) :to-be nil)
              (expect (string-match-p "macher-agent--get-context-workspace" content) :to-be nil))))))

    (it "reads audit log from macher-agent-context-plugins via read_context_audit_log"
      (let* ((ctx (macher-agent--make-context))
             (audit-entries '(((preset . "AuditPreset") (type . "gptel-tool") (target . "test_tool") (args . (:param "val")))))
             (tool-fn (gptel-tool-function macher-agent-read-context-audit-log-tool)))
        (setf (macher-agent-context-plugins ctx) (list :audit-log audit-entries))
        (let ((result (funcall tool-fn ctx nil :limit 5)))
          (expect result :to-match "SUCCESS: Audit log retrieved.")
          (expect result :to-match "AuditPreset")
          (expect result :to-match "test_tool"))))

    (it "suppresses patch by mutating macher-agent-context-plugins via submit_task_result"
      (let* ((ctx (macher-agent--make-context))
             (tool-fn (gptel-tool-function macher-agent-submit-task-result-tool))
             (macher-agent--suppress-patch t)
             (macher-agent-task-finished nil))
        (spy-on 'macher-agent-a2a-dispatch)
        (funcall tool-fn ctx nil :final_answer "Done with work")
        (expect (plist-get (macher-agent-context-plugins ctx) :suppress-patch) :to-be t)))

    (it "executes submit_task_result safely when macher-agent--routing-stack is unbound"
      (let* ((ctx (macher-agent--make-context))
             (tool-fn (gptel-tool-function macher-agent-submit-task-result-tool))
             (macher-agent-task-finished nil)
             (stack-bound (boundp 'macher-agent--routing-stack))
             (saved-val (when (boundp 'macher-agent--routing-stack)
                          (default-value 'macher-agent--routing-stack))))
        (spy-on 'macher-agent-a2a-dispatch)
        (unwind-protect
            (progn
              (makunbound 'macher-agent--routing-stack)
              (let ((res (funcall tool-fn ctx nil :final_answer "Unbound stack work")))
                (expect res :to-equal "SUCCESS: Result submitted. STOP NOW.")))
          (when stack-bound
            (set-default 'macher-agent--routing-stack saved-val)))))

    (it "resolves fsm and buffer from macher-agent-context-plugins in search_conversation_history"
      (let* ((buf (generate-new-buffer "*macher-test: search-history-direct*"))
             (ctx (macher-agent--make-context :origin-buffer buf))
             (mock-fsm (if (fboundp 'gptel-make-fsm)
                           (gptel-make-fsm :info (list :buffer buf))
                         (list :buffer buf)))
             (cmd-fn (or (get 'macher-agent-search-conversation-history-tool 'command-fn)
                         (get 'macher-agent-tool-search-conversation-history 'command-fn)))
             (macher-agent-search-backend-function #'macher-agent-search-glob))
        (with-current-buffer buf
          (insert "Line 1: test-keyword\nLine 2: bar\n"))
        (setf (macher-agent-context-plugins ctx) (list :fsm mock-fsm :buffer buf))
        (let ((res (funcall cmd-fn '(:query "test-keyword" :context_lines 1) ctx default-directory)))
          (expect res :to-match "test-keyword"))
        (kill-buffer buf)))

    (it "resolves parent buffer from macher-agent-context-plugins and handles unbound routing stack in search_parent_conversation_history"
      (let* ((parent-buf (generate-new-buffer "*macher-test: parent-direct*"))
             (child-buf (generate-new-buffer "*macher-test: child-direct*"))
             (ctx (macher-agent--make-context :origin-buffer child-buf))
             (cmd-fn (or (get 'macher-agent-search-parent-conversation-history-tool 'command-fn)
                         (get 'macher-agent-tool-search-parent-conversation-history 'command-fn)))
             (macher-agent-search-backend-function #'macher-agent-search-glob)
             (stack-bound (boundp 'macher-agent--routing-stack))
             (saved-val (when (boundp 'macher-agent--routing-stack)
                          (default-value 'macher-agent--routing-stack))))
        (with-current-buffer parent-buf
          (insert "Line 1: parent-info-target\nLine 2: summary\n"))
        (setf (macher-agent-context-plugins ctx)
              (list :originator-name (buffer-name parent-buf) :buffer child-buf))
        (unwind-protect
            (progn
              (makunbound 'macher-agent--routing-stack)
              (with-current-buffer child-buf
                (let ((res (funcall cmd-fn '(:query "parent-info-target" :context_lines 1) ctx default-directory)))
                  (expect res :to-match "parent-info-target"))))
          (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
          (when (buffer-live-p child-buf) (kill-buffer child-buf))
          (when stack-bound
            (set-default 'macher-agent--routing-stack saved-val))))))

 )

(provide 'macher-agent-skills-test)
;;; macher-agent-skills-test.el ends here
