;;; lspce-hints.el --- Inlay type information for lspce-mode

;; TODO: Check how it lspce checks in which buffers it must act on, because
;;       this mode is currently buffer local.

(defgroup lspce-hints-mode nil
  "Language Server Protocol client."
  :group 'tools
  :prefix "lspce-hints-")

;;; Variables:

(defface lspce-hints-type-foreground '((t (:inherit shadow)))
  "Face used to color type inlay hints."
  :group 'lspce-hints-mode)

(defface lspce-hints-parameter-foreground '((t :inherit shadow))
  "Face used to color parameter inlay hints.")

(defcustom lspce-hints-idle-timer-secs 1
  "Update the inlay hints after emacs is idle for N secs."
  :type 'float
  :group 'lspce-hints-mode)

(defvar lspce-hints--idle-timer nil
  "The idle timer used to update the inlay hints.")

;;; Functions:

(defun lspce-hints--clear-overlays ()
  "Clear all overlays with property `lspce-inlay-hint'."
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) 'lspce-inlay-hint t)))

(defun lspce-hints--add-overlay (abspos label kind padding-left padding-right)
  "Create an overlay with LABEL at ABSPOS and maybe add PADDING-LEFT and
PADDING-RIGHT."
  (let ((overlay (make-overlay abspos abspos nil 'front-advance 'end-advance))
	(face (cond ((eq kind 1) 'lspce-hints-type-foreground)
		    ((eq kind 2) 'lspce-hints-parameter-foreground)
		    (t (error "Server returned %s, which is not supported."
			      kind)))))
    (overlay-put overlay 'lspce-inlay-hint t)
    (overlay-put overlay 'before-string
		 (format "%s%s%s"
			 (if padding-left " " "")
 			 (propertize label 'font-lock-face face)
			 (if padding-right " " "")))))

(defun lspce-hints--get-label-string (label)
  "Convert the LABEL data from the server into a string. The LABEL is either a
string or a list."
  (cond ((stringp label) label)
	((consp label)
	 (string-join (mapcar (lambda (part) (gethash "value" part))
			      label)))))

(defun lspce-hints--abs-pos (pos)
  "Translate POS to a char position. POS is translated by
`lspce-hints--get-position' to a character position, because emacs barely knows
the concept of lines."
  (save-excursion
    (goto-char (point-min))
    (forward-line (car pos))
    (move-to-column (cdr pos))
    (point)))

(defun lspce-hints--get-position (pos)
  "Get line and char from POS. POS is returned form server. line and pos are
returned as a cons."
  (cons (gethash "line" pos) (gethash "character" pos)))

(defun lspce-hints--make-inlayHintParams ()
  "Generate the params for the server."
  (let* ((params (make-hash-table)))
    (puthash :textDocument (lspce--textDocumentIdenfitier
			    (lspce--path-to-uri buffer-file-name)) params)
    (puthash :range (lspce-hints--make-range)
	     params)
    params))

;; NOTE: maybe use lspce--make-postion. Otherwise use lspce--postition

(defun lspce-hints--make-range-start ()
  "Get the start of the visisble part of the buffer. It returns it in a format
the server can understand."
  (let ((params (make-hash-table)))
    (puthash :line (line-number-at-pos (window-start)) params)
    (puthash :character 0 params)
    params))

(defun lspce-hints--make-range-end ()
  "Get the end of the visisble part of the buffer. It returns it in a format
the server can understand."
  (let ((params (make-hash-table)))
    (puthash :line (min (+ (line-number-at-pos (window-end)) 1)
			(count-lines (point-min) (point-max)))
	     params)
    (puthash :character 0 params)
    params))

(defun lspce-hints--make-range ()
  "Calculate the visisble range for the server."
  (let ((params (make-hash-table)))
    (puthash :start (lspce-hints--make-range-start) params)
    (puthash :end (lspce-hints--make-range-end) params)
    params))

(defun lspce-hints--recalculate-inlays ()
  "Recalculate the inlays. This is called by the `lspce-hints--idle-timer'."
  (lspce-hints--clear-overlays)
  (let* ((response (lspce--request "textDocument/inlayHint"
				   (lspce-hints--make-inlayHintParams))))
    (dolist (elem response)
      (let* ((position (gethash "position" elem))
	     (label (gethash "label" elem))
	     (kind (gethash "kind" elem))
	     ;; (textEdits (gethash "textEdits" elem)) returned but not used.
	     (paddingLeft (gethash "paddingLeft" elem))
	     (paddingRight (gethash "paddingRight" elem))
	     ;; (data (gethash "data" elem)) returned but not used.
	     (pos (lspce-hints--get-position position))
	     (abspos (lspce-hints--abs-pos pos)))
	(lspce-hints--add-overlay abspos (lspce-hints--get-label-string label)
				  kind paddingLeft paddingRight)))))

(defun lspce-hints--deactivate-mode ()
  "Function called when buffer is killed."
  (when lspce-hints-mode
    (lspce-hints-mode nil)))

(define-minor-mode lspce-hints-mode
  "When lspce-hints-mode is enabled, emacs displays type information
in the current buffer. This mode requires lspce-mode to be enabled."
  :lighter nil
  :global nil
  ;; TODO: Check if lspce-mode is enabled.
  (if lspce-hints-mode
      (progn
	(lspce-hints--recalculate-inlays)
	(setq lspce-hints--idle-timer
	      (run-with-idle-timer
	       lspce-hints-idle-timer-secs t
	       ;; TODO: optimize this:
	       (lambda () (when lspce-hints-mode
			    (lspce-hints--recalculate-inlays)))))
	;; Overlays and timer will get handled when mode is deactivated.
	(add-hook 'kill-buffer-hook 'lspce-hints--deactivate-mode nil t))
    (progn
      (when lspce-hints--idle-timer
	(cancel-timer lspce-hints--idle-timer))
      (lspce-hints--clear-overlays)
      (remove-hook 'kill-buffer-hook 'lspce-hints--kill-buffer-function t))))

(provide 'lspce-hints)
