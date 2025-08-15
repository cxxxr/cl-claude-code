;; https://docs.anthropic.com/ja/docs/claude-code/sdk

(uiop:define-package :cl-claude-code
  (:nicknames :claude-code)
  (:use :cl
        :alexandria)
  (:export :make-options
           :spawn
           :query))
(in-package :claude-code)

(defmethod print-object ((object hash-table) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (alexandria:hash-table-alist object) stream)))

(defun hash (&rest args)
  (alexandria:plist-hash-table args :test 'equal))

(defstruct (claude-code-options (:constructor make-options))
  allowed-tools
  max-thinking-tokens
  system-prompt
  append-system-prompt
  mcp-servers
  permission-mode
  continue-conversation
  resume
  max-turns
  disallowed-tools
  model
  permission-prompt-tool-name
  cwd
  settings
  add-dirs)

(defun json-encode (object &key (encode #'yason:encode))
  (with-output-to-string (out) (funcall encode object out)))

(defun construct-command (prompt options)
  `("claude"
    "--output-format" "stream-json"
    "--verbose" 
    "--print" ,prompt
    ,@(when (claude-code-options-system-prompt options)
        (list "--system-prompt" (claude-code-options-system-prompt options)))
    ,@(when (claude-code-options-append-system-prompt options)
        (list "--append-system-prompt" (claude-code-options-append-system-prompt options)))
    ,@(when (claude-code-options-allowed-tools options)
        (list "--allowedTools" (format nil "~{~A~^,~}" (claude-code-options-allowed-tools options))))
    ,@(when (claude-code-options-max-turns options)
        (list "--max-turns" (write-to-string (claude-code-options-max-turns options))))
    ,@(when (claude-code-options-disallowed-tools options)
        (list "--disallowedTools"
              (format nil "~{~A~^,~}" (claude-code-options-disallowed-tools options))))
    ,@(when (claude-code-options-model options)
        (list "--model" (claude-code-options-model options)))
    ,@(when (claude-code-options-permission-prompt-tool-name options)
        (list "--permission-prompt-tool" (claude-code-options-permission-prompt-tool-name options)))
    ,@(when (claude-code-options-permission-mode options)
        (list "--permission-mode" (claude-code-options-permission-mode options)))
    ,@(when (claude-code-options-continue-conversation options)
        (list "--continue"))
    ,@(when (claude-code-options-resume options)
        (list "--resume" (claude-code-options-resume options)))
    ,@(when (claude-code-options-settings options)
        (list "--settings" (claude-code-options-settings options)))
    ,@(when (claude-code-options-add-dirs options)
        (mapcan (lambda (dir) (list "--add-dir" (princ-to-string dir)))
                (claude-code-options-add-dirs options)))
    ,@(when (claude-code-options-mcp-servers options)
        (cond
          ((consp (claude-code-options-mcp-servers options))
           (list "--mcp-config"
                 (let ((yason:*parse-object-as-alist* t))
                   (json-encode
                    (acons "mcpServers"
                           (claude-code-options-mcp-servers options)
                           nil)
                    :encode #'yason:encode-alist))))
          (t
           (list "--mcp-config" (princ-to-string (claude-code-options-mcp-servers options))))))))

(defstruct client
  thread
  mailbox
  mailbox-for-interrupt
  (request-counter 0))

(defun yield (client &key (wait t) timeout)
  (check-type client client)
  (cond (wait
         (lem-mailbox:receive-message (client-mailbox client) :timeout timeout))
        (t
         (lem-mailbox:receive-message-no-hang (client-mailbox client)))))

(defun receive-message (process)
  (check-type process async-process::process)
  (loop :with buffer := ""
        :do (let ((data (async-process:process-receive-output process)))
              (when data (format t "output: ~A~%" data))
              (setf buffer (concatenate 'string buffer data))
              (handler-case
                  (yason:parse buffer)
                (error ()
                  ;; If there is a parse error, the input is incomplete.
                  )
                (:no-error (value &rest *)
                  (return value))))
            (sleep 0.1)
        :while (async-process:process-alive-p process)))

(defun spawn (prompt options)
  (let* ((mailbox (lem-mailbox:make-mailbox :name "Claude Code mailbox"))
         (mailbox-for-interrupt (lem-mailbox:make-mailbox 
                                 :name "Claude Code mailbox-for-interrupt")))
    (make-client
     :mailbox mailbox
     :mailbox-for-interrupt mailbox-for-interrupt
     :thread (bt2:make-thread
              (lambda ()
                (let* ((process
                         (async-process:create-process
                          (construct-command prompt options)
                          :nonblock t)))
                  (loop
                    :do
                       (when-let (message (lem-mailbox:receive-message-no-hang
                                           mailbox-for-interrupt))
                         (format t "send-message: ~A~%" message)
                         (async-process:process-send-input process message))
                       (let ((value (receive-message process)))
                         (when value
                           (format t "receive-message: ~A~%" value)
                           (lem-mailbox:send-message mailbox value)))
                    :while (async-process:process-alive-p process))))
              :name "Claude Code thread"))))

(defun query (client prompt &key (session-id "default"))
  (check-type client client)
  (lem-mailbox:send-message (client-mailbox-for-interrupt client) 
                            (json-encode
                             (hash "type" "user"
                                   "message" (hash "role" "user"
                                                   "content" prompt)
                                   "session_id" session-id))))


;;; This code may not work properly.

(defun new-request-id (client)
  (check-type client client)
  (format nil
          "req_~D_~X"
          (incf (client-request-counter client))
          (random most-positive-fixnum)))

(defun generate-interrupt-message (request-id)
  (with-output-to-string (out)
    (yason:encode (hash "type" "control_request"
                        "request_id" request-id
                        "request" (hash "subtype" "interrupt"))
                  out)))

(defun interrupt (client)
  (check-type client client)
  (let* ((request-id (new-request-id client))
         (message (generate-interrupt-message request-id)))
    (lem-mailbox:send-message (client-mailbox-for-interrupt client) message)))
