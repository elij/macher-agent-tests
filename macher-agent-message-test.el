;;; macher-agent-message-test.el --- Point-to-point message passing tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Point-to-point message passing tests for macher-agent.

;;; Code:
(let* ((file (or load-file-name buffer-file-name))
       (this-dir (if file (file-name-directory (expand-file-name file)) (expand-file-name default-directory)))
       (root-dir (or (locate-dominating-file this-dir "macher-agent.el")
                     (locate-dominating-file default-directory "macher-agent.el")
                     (locate-dominating-file default-directory "tests")
                     default-directory))
       (test-dir (cond
                  ((file-exists-p (expand-file-name "macher-agent-test-setup.el" this-dir))
                   this-dir)
                  ((file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" root-dir))
                   (expand-file-name "tests" root-dir))
                  ((file-exists-p (expand-file-name "macher-agent-test-setup.el" default-directory))
                   (expand-file-name default-directory))
                  ((file-exists-p (expand-file-name "tests/macher-agent-test-setup.el" default-directory))
                   (expand-file-name "tests" default-directory))
                  (t (or (locate-dominating-file default-directory "tests") (expand-file-name "tests" root-dir))))))
  (when root-dir
    (add-to-list 'load-path (file-name-as-directory (expand-file-name root-dir))))
  (add-to-list 'load-path (expand-file-name "tests" default-directory))
  (add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name) default-directory)))
  (when test-dir
    (add-to-list 'load-path (file-name-as-directory (expand-file-name test-dir)))
    (add-to-list 'load-path (file-name-as-directory (expand-file-name "helpers" test-dir)))))

(require 'buttercup)
(require 'macher-agent-test-setup)
(require 'macher-agent)
(require 'macher-agent-macher nil t)

(describe "Point-to-point message passing tools"

          (before-each
           (clrhash macher-agent--pending-callbacks)
           (load (expand-file-name "skills/scripts/wait_for_message.el") nil t)
           (load (expand-file-name "skills/scripts/send_message.el") nil t))

          (describe "wait_for_message"
                    (it "registers the current buffer in pending callbacks"
                        (let* ((buf (get-buffer-create "*test-wait-buf*"))
                               (wait-fn (get 'macher-agent-wait-for-message-tool 'ptc-function))
                               (resolved nil)
                               (resolve-cb (lambda (msg) (setq resolved msg))))
                          (with-current-buffer buf
                            (let ((thunk (funcall wait-fn nil nil nil)))
                              (funcall thunk resolve-cb))
                            (expect (gethash (buffer-name buf) macher-agent--pending-callbacks) :not :to-be nil))
                          (kill-buffer buf))))

          (describe "routing stack behavior"
                    (it "restores the previous routing frame and transmits ARTIFACT_UPDATE after popping frame"
                        (load (expand-file-name "skills/scripts/submit_task_result.el") nil t)
                        (let* ((task-root "root-task-1")
                               (task-peer "peer-task-2")
                               (root-buf (get-buffer-create "*test-root-parent*"))
                               (child-buf (get-buffer-create "*test-child-agent*"))
                               (peer-buf (get-buffer-create "*test-peer-agent*"))
                               (root-received nil)
                               (peer-received nil))
                          (unwind-protect
                              (progn
                                (clrhash macher-agent--pending-callbacks)
                                ;; 1. Root registers task and callback
                                (puthash task-root (lambda (res) (setq root-received res)) macher-agent--pending-callbacks)
                                (with-current-buffer child-buf
                                  (setq-local macher-agent--routing-stack nil)
                                  (macher-agent--push-routing task-root (buffer-name root-buf) t))

                                ;; 2. Peer communicates with child via A2A callback
                                (puthash task-peer (lambda (res) (setq peer-received res)) macher-agent--pending-callbacks)
                                (with-current-buffer child-buf
                                  (macher-agent--push-routing task-peer (buffer-name peer-buf)))

                                ;; 3. Child completes peer request -> submit_task_result pops peer frame
                                (with-current-buffer child-buf
                                  (let ((submit-fn (get 'macher-agent-submit-task-result-tool 'ptc-function)))
                                    (funcall submit-fn "Peer answer" nil nil)))

                                (expect peer-received :not :to-be nil)
                                (expect (macher-agent-transit-payload-type peer-received) :to-equal 'ARTIFACT_UPDATE)
                                (expect (plist-get (macher-agent-transit-payload-payload peer-received) :payload) :to-equal "Peer answer")
                                (expect root-received :to-be nil)

                                ;; 4. Child completes root task -> submit_task_result pops root frame
                                (with-current-buffer child-buf
                                  (let ((submit-fn (get 'macher-agent-submit-task-result-tool 'ptc-function)))
                                    (funcall submit-fn "Root final answer" nil nil)))

                                (expect root-received :not :to-be nil)
                                (expect (macher-agent-transit-payload-type root-received) :to-equal 'ARTIFACT_UPDATE)
                                (expect (plist-get (macher-agent-transit-payload-payload root-received) :payload) :to-equal "Root final answer"))
                            (kill-buffer root-buf)
                            (kill-buffer child-buf)
                            (kill-buffer peer-buf))))))

(provide 'macher-agent-message-test)
;;; macher-agent-message-test.el ends here
