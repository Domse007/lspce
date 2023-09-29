;; NOTE: maybe useful functions: `lspce--path-to-uri'

(eval-when-compile
  (require 'subr-x))

(defcustom lspce-poll-timer-time 0.5
  "Either the idle time or repeat time."
  :type 'float)

(defcustom lspce-poll-timer-constructor #'run-with-idle-timer
  "Timer constructor used by lspce-poll."
  :type 'function)

(defcustom lspce-poll-max-poll-time 0.1
  "Maximum amount of time spent handling server requests. If the poll queue is
too long to be handled in one go, it will suspend after N secs."
  :type 'float)

(defvar lspce-poll--handlers
  (let ((table (make-hash-table)))
    (puthash "workspace/inlayHint/refresh"
	     #'lspce-poll--handle-HintRefreshRequest
	     table)
    table)
  "Hash table with all handlers for server requests. The key is the function
which should be called by emacs. The value is a function name to the handler.
The handler should take one argument: the parameter from the server.")

(defvar lspce-poll--idle-timer nil
  "Global variable storing the timer.")

(defun lspce-poll--handle-HintRefreshRequest (_params)
  "Handler for hint refreshes requested by the server."
  (lspce-hints--recalculate-inlays))

(defun lspce-poll--get-last (&optional root-uri lsp-type)
  "Get the last poll request from the rust part."
  (let* ((root-uri (or root-uri lspce--root-uri))
	 (lsp-type (or lsp-type lspce--lsp-type)))
    (lspce-module-poll-server-request root-uri lsp-type)))

(defun lspce-poll--timer-handler-last ()
  ""
  ;; TODO: write logic
  (when-let* ((request-string (lspce-poll--get-last))
	      (request (lspce--json-deserialize request-string)))
    (let ((method (gethash "method" request))
	  (params (gethash "params?" request)))
      (if-let ((handler (gethash method lspce-poll--handlers)))
	  (progn (funcall handler params)
		 ;; return value
		 t)))))

(defun lspce-poll--timer-handler ()
  (let ((start-time (current-time))
	(run t))
    (while (and (lspce-poll--timer-handler-last)
		run)
      ;; Stop when running for too long
      (when (< lspce-poll-max-poll-time (float-time (time-since start-time)))
	(setq run nil)))))

(defun lspce-poll--init (&optional timer-constructor)
  (let* ((t-constructor (or timer-constructor lspce-poll-timer-constructor))
	 (timer (funcall t-constructor lspce-poll-timer-time t
			 #'lspce-poll--timer-handler)))
    (setq lspce-poll--idle-timer timer)))

(defun lspce-poll--terminate ()
  (cancel-timer lspce-poll--idle-timer)
  (setq lspce-poll--idle-timer nil))

(provide 'lspce-poll)
