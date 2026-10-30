;;; octave-breakpoint.el --- Manage Octave breakpoints -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'octave)
(require 'subr-x)
(require 'mb-octave-eval)

(declare-function octave-debug-p "octave-debug-mode")

(defvar inferior-octave-output-list)

(defun octave-breakpoint-map ()
  "Return the keymap for Octave breakpoint commands."
  (let ((map (make-sparse-keymap)))
    (define-key map "i" #'octave-toggle-breakpoint)
    (define-key map "t" #'octave-toggle-breakpoint)
    (define-key map "r" #'octave-refresh-breakpoints)
    (define-key map "p" #'octave-list-all-breakpoints)
    (define-key map "j" #'octave-forward-breakpoint)
    (define-key map "k" #'octave-backward-breakpoint)
    (define-key map "d" (octave-delete-breakpoint-map))
    map))

(defun octave-delete-breakpoint-map ()
  "Return the keymap for deleting Octave breakpoints."
  (let ((map (make-sparse-keymap)))
    (define-key map "r" #'octave-delete-region-breakpoints)
    (define-key map "l" #'octave-delete-breakpoint-line)
    (define-key map "d" #'octave-delete-defun-breakpoints)
    (define-key map "b" #'octave-delete-buffer-breakpoints)
    (define-key map "a" #'octave-delete-all-breakpoints)
    map))

(defface octave-breakpoint-indicator
  '((((min-colors 88) (class color))
     :foreground "yellow1" :background "red1" :width condensed)
    (((class color))
     :foreground "yellow" :background "red" :width condensed)
    (t :inverse-video t))
  "Face used by Octave mode for marking breakpoint lines in the fringe."
  :group 'octave)

(defvaralias '+octave-breakpoint-string+ 'octave-breakpoint-string)
(defvaralias '+octave-breakpoint-regexp+ 'octave-breakpoint-regexp)
(defvaralias '+octave-breakpoint-display+ 'octave-breakpoint-display)

(defconst octave-breakpoint-string " #BREAKPOINT"
  "Text appended to an Octave breakpoint line.")

(defconst octave-breakpoint-regexp
  (concat (regexp-quote octave-breakpoint-string) "$")
  "Regexp matching an Octave breakpoint marker.")

(defconst octave-breakpoint-display
  '(left-fringe hollow-rectangle octave-breakpoint-indicator)
  "Display property used for Octave breakpoint markers.")

(defun octave--goto-line (line)
  "Move to the beginning of one-based LINE in the current buffer."
  (unless (and (integerp line) (> line 0))
    (user-error "Line must be a positive integer"))
  (goto-char (point-min))
  (unless (zerop (forward-line (1- line)))
    (user-error "Line %d is outside the buffer" line)))

(defun octave--line-region (&optional line)
  "Return the region covering LINE, or the current line."
  (save-excursion
    (when line
      (octave--goto-line line))
    (cons (line-beginning-position) (line-end-position))))

(defun octave--scope-region (scope)
  "Return the buffer region designated by SCOPE."
  (cond
   ((memq scope '(nil :line))
    (octave--line-region))
   ((integerp scope)
    (octave--line-region scope))
   ((eq scope :buffer)
    (cons (point-min) (point-max)))
   (t
    (user-error "Unsupported breakpoint scope: %S" scope))))

(defun octave--function-location (&optional pos)
  "Return the Octave function name and line number at POS."
  (save-excursion
    (when pos
      (goto-char pos))
    (let ((line (line-number-at-pos)))
      (octave-beginning-of-defun)
      (unless (looking-at octave-function-header-regexp)
        (user-error "Point is not inside an Octave function"))
      (list (match-string-no-properties 3) line))))

(defun octave--mode-buffers ()
  "Return all live Octave mode buffers."
  (cl-loop for buffer in (buffer-list)
           when (with-current-buffer buffer
                  (derived-mode-p 'octave-mode))
           collect buffer))

(defun octave-buffer-breakpoint-p (&optional pos)
  "Return non-nil when the line at POS has a buffer breakpoint."
  (save-excursion
    (when pos
      (goto-char pos))
    (goto-char (line-beginning-position))
    (re-search-forward octave-breakpoint-regexp (line-end-position) t)))

(defun octave-set-buffer-breakpoint (&optional line)
  "Add a buffer breakpoint marker at one-based LINE.
LINE defaults to the current line."
  (save-excursion
    (when line
      (octave--goto-line line))
    (goto-char (line-end-position))
    (unless (octave-buffer-breakpoint-p)
      (let ((start (point)))
        (insert octave-breakpoint-string)
        (put-text-property start (point) 'display
                           octave-breakpoint-display))
      t)))

(defun octave-unset-buffer-breakpoint (&optional scope)
  "Delete buffer breakpoint markers in SCOPE.
SCOPE may be nil or `:line' for the current line, a line number,
or `:buffer' for the entire current buffer.  Return non-nil when
at least one marker was deleted."
  (pcase-let ((`(,beg . ,end) (octave--scope-region scope)))
    (let ((end-marker (copy-marker end))
          deleted)
      (unwind-protect
          (save-excursion
            (goto-char beg)
            (while (re-search-forward octave-breakpoint-regexp end-marker t)
              (replace-match "")
              (setq deleted t)))
        (set-marker end-marker nil))
      deleted)))

(defun octave-region-breakpoints (beg end &optional buffer)
  "Return buffer breakpoints between BEG and END in BUFFER.
Each result has the form (FUNCTION LINE)."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char beg)
      (cl-loop while (re-search-forward octave-breakpoint-regexp end t)
               collect (octave--function-location
                        (line-beginning-position))))))

(defun octave-buffer-breakpoints (&optional buffer)
  "Return all buffer breakpoints in BUFFER as (FUNCTION LINE) pairs."
  (with-current-buffer (or buffer (current-buffer))
    (octave-region-breakpoints (point-min) (point-max))))

(defun octave-all-buffer-breakpoints ()
  "Return marked breakpoints in all active Octave mode buffers.
Each result has the form (BUFFER BREAKPOINTS)."
  (cl-loop for buffer in (octave--mode-buffers)
           for breakpoints = (octave-buffer-breakpoints buffer)
           when breakpoints
           collect (list buffer breakpoints)))

(defun octave-dbstop-p (&optional pos)
  "Return non-nil when Octave has a dbstop at POS."
  (pcase-let ((`(,function ,line) (octave--function-location pos)))
    (when-let ((entry (assoc-string function (octave-dbstop-list))))
      (memq line (cadr entry)))))

(defun octave-dbstop-list-1 (line)
  "Parse one Octave dbstatus response LINE.
Return a (FUNCTION LINES) list, or signal an error for malformed
nonempty input."
  (if (string-match
       "\\`[[:space:]]*breakpoint in \\([^[:space:]]+\\) at lines? \\(.+?\\)\\.[[:space:]]*\\'"
       line)
      (list (match-string 1 line)
            (mapcar #'string-to-number
                    (split-string (match-string 2 line) ",[[:space:]]*" t)))
    (error "Unexpected Octave dbstatus output: %s" line)))

(defun octave-dbstop-list (&optional function)
  "Return Octave dbstops, optionally limited to FUNCTION."
  (let ((output
         (octave-send-string
          (if function (format "dbstatus %s" function) "dbstatus"))))
    (mapcar #'octave-dbstop-list-1
            (split-string output "\n" t "[[:space:]]+"))))

(defun octave-set-dbstops-1 (function lines)
  "Compose a dbstop command for FUNCTION and LINES."
  (let ((line-list (if (listp lines) lines (list lines))))
    (when line-list
      (string-join
       (append (list "dbstop")
               (when function (list function))
               (mapcar #'number-to-string line-list))
       " "))))

(defun octave-set-dbstops (function lines &optional show-output-p)
  "Set Octave dbstops at LINES in FUNCTION.
When SHOW-OUTPUT-P is non-nil, also show the response in the
inferior Octave buffer."
  (when-let ((command (octave-set-dbstops-1 function lines)))
    (octave-send-string command show-output-p)))

(defun octave-update-dbstops (buffer-breakpoints)
  "Convert BUFFER-BREAKPOINTS to Octave dbstops.
BUFFER-BREAKPOINTS contains (BUFFER FUNCTION-BREAKPOINTS) items."
  (dolist (entry buffer-breakpoints)
    (pcase-let ((`(,buffer ,function-breakpoints) entry))
      (octave-source-buffer t buffer)
      (dolist (breakpoint function-breakpoints)
        (pcase-let ((`(,function ,lines) breakpoint))
          (octave-set-dbstops function lines t))))))

(defun octave-update-dbstops-buffer (&optional buffer)
  "Convert marked breakpoints in BUFFER to Octave dbstops."
  (let ((target (or buffer (current-buffer))))
    (when-let ((breakpoints (octave-buffer-breakpoints target)))
      (octave-update-dbstops (list (list target breakpoints))))))

(defun octave-update-all-dbstops ()
  "Convert all marked buffer breakpoints to Octave dbstops."
  (octave-update-dbstops (octave-all-buffer-breakpoints)))

(defalias 'octave-update-all-dbstop #'octave-update-all-dbstops)

(defun octave-breakpoint-p (&optional pos)
  "Return non-nil when POS has a buffer breakpoint or Octave dbstop."
  (let ((dbstop-p (octave-dbstop-p pos))
        (buffer-breakpoint-p (octave-buffer-breakpoint-p pos)))
    (unless (eq (not dbstop-p) (not buffer-breakpoint-p))
      (message "Buffer breakpoint and dbstop settings differ"))
    (or dbstop-p buffer-breakpoint-p)))

(defun octave-set-breakpoint (&optional pos)
  "Set an Octave breakpoint on the line containing POS."
  (if (octave-breakpoint-p pos)
      (message "Breakpoint is already present")
    (pcase-let ((`(,function ,line) (octave--function-location pos)))
      (octave-set-dbstops
       (unless (and (fboundp 'octave-debug-p) (octave-debug-p))
         function)
       line t)
      (octave-set-buffer-breakpoint line))))

(defun octave-unset-breakpoint (&optional pos)
  "Unset any Octave breakpoint on the line containing POS."
  (if (not (octave-breakpoint-p pos))
      (message "No breakpoint to unset")
    (pcase-let ((`(,function ,line) (octave--function-location pos)))
      (octave-send-string
       (if (and (fboundp 'octave-debug-p) (octave-debug-p))
           (format "dbclear %d" line)
         (format "dbclear %s %d" function line))
       t)
      (octave-unset-buffer-breakpoint line))))

(defun octave-unset-breakpoints-region (beg end &optional buffer)
  "Unset every breakpoint between BEG and END in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (dolist (breakpoint
             (reverse (octave-region-breakpoints beg end)))
      (octave--goto-line (cadr breakpoint))
      (octave-unset-breakpoint (point)))))

(defmacro octave-with-temp-dbstop (pos &rest body)
  "Execute BODY with a temporary breakpoint at POS."
  (declare (indent 1) (debug t))
  (let ((position (make-symbol "position"))
        (existing (make-symbol "existing")))
    `(let* ((,position ,pos)
            (,existing (octave-breakpoint-p ,position)))
       (unless ,existing
         (octave-set-breakpoint ,position))
       (unwind-protect
           (progn ,@body)
         (unless ,existing
           (octave-unset-breakpoint ,position))))))

(defun octave-toggle-breakpoint ()
  "Toggle the breakpoint on the current line."
  (interactive)
  (if (octave-breakpoint-p)
      (octave-unset-breakpoint)
    (octave-set-breakpoint)))

(defun octave-list-all-breakpoints ()
  "Display all Octave dbstops."
  (interactive)
  (octave-send-string "dbstatus" t))

(defun octave-refresh-breakpoints ()
  "Refresh Octave dbstops from all buffer breakpoint markers."
  (interactive)
  (octave-update-all-dbstops))

(defun octave-delete-breakpoint-line ()
  "Delete the breakpoint on the current line."
  (interactive)
  (octave-unset-breakpoint))

(defun octave-delete-region-breakpoints (beg end)
  "Delete breakpoints between BEG and END."
  (interactive "r")
  (octave-unset-breakpoints-region beg end))

(defun octave-delete-defun-breakpoints ()
  "Delete all breakpoints in the current Octave function."
  (interactive)
  (save-excursion
    (octave-beginning-of-defun)
    (let ((beg (point)))
      (end-of-defun)
      (octave-delete-region-breakpoints beg (point)))))

(defun octave-delete-buffer-breakpoints ()
  "Delete all breakpoints in the current buffer."
  (interactive)
  (octave-delete-region-breakpoints (point-min) (point-max)))

(defun octave-delete-all-breakpoints ()
  "Delete all buffer breakpoints and Octave dbstops."
  (interactive)
  (dolist (entry (octave-all-buffer-breakpoints))
    (with-current-buffer (car entry)
      (octave-delete-buffer-breakpoints)))
  (octave-send-string "dbclear all"))

(defun octave-forward-breakpoint (&optional n)
  "Move forward N buffer breakpoints, wrapping around the buffer."
  (interactive "p")
  (setq n (or n 1))
  (unless (zerop n)
    (let ((positions
           (mapcar #'cadr (octave-buffer-breakpoints))))
      (unless positions
        (user-error "Buffer has no breakpoints"))
      (let* ((line (line-number-at-pos))
             (start
              (if (> n 0)
                  (or (cl-position-if (lambda (candidate)
                                        (> candidate line))
                                      positions)
                      0)
                (or (cl-position-if (lambda (candidate)
                                      (< candidate line))
                                    positions :from-end t)
                    (1- (length positions)))))
             (offset (if (> n 0) (1- n) (1+ n)))
             (index (mod (+ start offset) (length positions))))
        (octave--goto-line (nth index positions))))))

(defun octave-backward-breakpoint (&optional n)
  "Move backward N buffer breakpoints, wrapping around the buffer."
  (interactive "p")
  (octave-forward-breakpoint (- (or n 1))))

(provide 'octave-breakpoint)

;;; octave-breakpoint.el ends here
