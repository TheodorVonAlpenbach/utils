(require 'octave)

(cl-defun octave-last-sexp (&optional (point (point)))
  (save-excursion
    (goto-char point)
    (forward-sexp -1)
    (when (looking-at "[[(]")
      (forward-sexp -1))
    (concat (buffer-substring-no-properties (point) point) "\n")))

(defun octave-eval-last-sexp (prefix)
  "Evaluate Octave sexp before point; print value in the echo area.
Interactively, with prefix argument, print output into current buffer."
  (interactive "P")
  (case prefix
    ((4) (octave-eval-print-last-sexp))
    (0 (octave-eval-print-last-sexp 'size))
    (otherwise
     (inferior-octave t)
     (let ((standard-output t)
	   (sexp (octave-last-sexp)))
       (inferior-octave-send-list-and-digest (list sexp))
       (prin1 (mapconcat 'identity inferior-octave-output-list "\n"))))))


(defun octave-update-proc-buffer (line)
  "Perhaps concat of inferior-octave-output-list and
inferior-octave-output-string should be done by caller and sent
to this function as a string."
  (insert-before-markers
   (mapconcat 'identity
	      (append
	       (if octave-send-echo-input (list line) (list ""))
	       inferior-octave-output-list
	       (list inferior-octave-output-string))
	      "\n")))

(defun octave-send-string (string)
  "Send current region to the inferior Octave process."
  (interactive "r")
  (inferior-octave t)
  (let ((proc inferior-octave-process)
        line)
    (with-current-buffer inferior-octave-buffer
      ;; http://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00095.html
      (compilation-forget-errors)
      (setq inferior-octave-output-list nil)
      ;; I think the while loops over lines
      (with-lines (line string)
	(setq inferior-octave-receive-in-progress t)
        (inferior-octave-send-list-and-digest (list (concat line "\n")))
        (while inferior-octave-receive-in-progress
          (accept-process-output proc))
        (octave-update-proc-buffer line))))
  (if octave-send-show-buffer
    (display-buffer inferior-octave-buffer)))

(defun octave-eval-current-line ()
  "Sends one-line expressions to the SQL output buffer. 
It handles also lines commented with #.
Examples:
ones (2)
#ones (2)
##ones (2)

\(Note the important prefixed NEWLINE, so correct aligning not displaced by
the output buffer prompt.)"
  (interactive)
  (let* ((regexp "^[[:space:]#]*\\(.*\\)")
	 (s (string-trim (string-match* regexp (current-line-as-string) :num 1))))
    (if s
      (octave-send-string (format "\n%s" s))
      (message "No expression at point"))))

(defvar *octave-eval-history* nil)
;;(nilf *octave-eval-history*)

(defun octave-eval-expression ()
  "Interactive evaluation of Octave expression."
  (interactive)
  (let* ((default (first *octave-eval-history*))
	 (exp (read-string (format "Eval Octave expression: (%s): " default)
		nil '*octave-eval-history* default)))
    (octave-send-string exp)))
;;(eval-scilab-expression)
;;(read-string "test: ")

(add-hook 'octave-mode-hook 'linum-mode)

;;;; extensions for debugging
(defun mb-octave-debug-filter ()
  "Sentinel for getting debug info. Should only be active around
  debugging commands in mb-octave mode. Here is a typical usage:
1. Activate (this) sentinel
2. Send debug command (dbstop, dbclear, dbquit etc)
3. Sentinel reads output and takes the appropriate action
   (move active line marker, toggle breakpoint line marker etc)
4. Deactivate sentinel")

(defun mb-octave-location ()
  "Returns (FUNCTION-NAME LINUM), where FUNCTION-NAME is the
function at point and LINUM is the current line number."
  (let ((fn (substring-no-properties (add-log-current-defun)))
	(linum (line-number-at-pos)))
    (list fn linum)))

(defun mb-octave-breakpoint-list (&optional fn)
  (octave-send-string (if fn (format "dbstatus %s" fn) "dbstatus"))
  (loop for line in inferior-octave-output-list
	for (fn linum) = (string-match*
			  "breakpoint in \\([^ ]+\\) at line \\([[:digit:]]+\\)\\."
			  line :num '(1 2))))

(defun mb-octave-set-breakpoint ()
  (octave-send-string (apply #'format "dbstop %s %d" (mb-octave-location)))
  (first inferior-octave-output-list))
;;(mb-octave-set-breakpoint)

(defun mb-octave-unset-breakpoint ()
  (octave-send-string (apply #'format "dbclear %s %d" (mb-octave-location)))
  inferior-octave-output-list)
;;(mb-octave-unset-breakpoint)

(defun mb-octave-parse-ans (line)
  "Handles only single line answers"
  (and (stringp line)
       (string-match* "ans = \\(.\\)" line :num 1)))
;;(mapcar #'mb-octave-parse-ans (list nil "ans = 0"))

(defun mb-octave-debug-mode-p ()
  (octave-send-string "isdebugmode")
  (awhen (first inferior-octave-output-list)
    (not (equal (mb-octave-parse-ans it) "0"))))

(defun octave-exit-debug-mode ()
  "Not implemented Should make current buffer writeable, remove step line
  markings, etc")

(defun octave-enter-debug-mode ()
  "Not implemented Should make current buffer read-only, amplify breakpoints, etc.
  markings, etc")

(defun octave-resume ()
  (octave-send-string "dbcont")
  (unless (mb-octave-debug-mode-p)
    (octave-exit-debug-mode)))

;;; UI
(defun octave-clear-all-breakpoints ()
  (interactive)
  (octave-send-string "dbclear all"))

(defun octave-toggle-breakpoint ()
  (interactive)
  (let ((bl (mb-octave-breakpoint-list)))
    (if (find (mb-octave-location) bl :test #'equal)
      (mb-octave-unset-breakpoint)
      (mb-octave-set-breakpoint))))

(defun octave-step ()
  (interactive)
  (octave-send-string "dbstep")
  (unless (mb-octave-debug-mode-p)
    (octave-exit-debug-mode)))

(defun octave-step-in ()
  (interactive)
  (octave-send-string "dbstep in")
  (unless (mb-octave-debug-mode-p)
    (octave-exit-debug-mode)))

(defun octave-step-out ()
  (interactive)
  (octave-send-string "dbstep out")
  (unless (mb-octave-debug-mode-p)
    (octave-exit-debug-mode)))

(defun octave-quit-debug ()
  (interactive)
  (octave-send-string "dbquit")
  (if (mb-octave-debug-mode-p)
    (error "Octave still in debug mode!")
    (octave-exit-debug-mode)))

(defun octave-run ()
  (interactive)
  (if (mb-octave-debug-mode-p)
    (octave-resume)
    (save-excursion
      (end-of-defun)
      (octave-eval-current-line))))

(defun octave-eval-defun ()
  (interactive)
  (let ((standard-output t))
    (octave-send-defun)))

(defun octave-info (prefix)
  "Open the Info page for Octave.
If the current buffer mode is Octave mode, the Info buffer is
shown in the other window. The other window is created if
necessary.

If PREFIX argument is provided it opens the Info page for Octave
mode in Emacs."
  (interactive "P")
  (cl-flet ((main (nodename bufname)
	      (aif (get-buffer bufname)
		(switch-to-buffer-other-window it)
		(save-excursion
		  (other-window 1)
		  (info nodename bufname)))))
    (if prefix
      (main "(octave-mode.info)Top" "*Octave-mode-info*")
      (main "(octave.info)Top" "*Octave-info*"))))

(defun inferior-octave-info ()
  (interactive)
  (aif (get-buffer "*Octave-info*")
    (switch-to-buffer it)
    (info "(octave.info)Top" "*Octave-info*")))

(defun octave-fill-documentation-paragraph ()
  "Fills an Octave documentation paragraph.
This functionality is not well covered by octave-fill-paragraph"
  (let ((fill-prefix "## ")
	(fill-paragraph-function nil))
    (fill-paragraph)))

(defun octave-in-documentation-p ()
  "Returns non-nil iff point is in the Octave documentation.
Currently only the documentation of the main function is
supported. To implement the general version we need to know if we
are between functions."
  (string-match
     "\\(^##.*\\)*"
     (buffer-substring-no-properties (point-min) (point))))

(defun mb-octave-fill-paragraph ()
  (interactive)
  (if (octave-in-documentation-p)
    (octave-fill-documentation-paragraph)
    (octave-fill-paragraph)))

;;; shortcuts
(define-key octave-mode-map (kbd "C-x C-e") 'octave-eval-last-sexp)
(define-key octave-mode-map (kbd "C-M-x") 'octave-eval-defun)
(define-key octave-mode-map (kbd "M-q") 'mb-octave-fill-paragraph)
(define-key octave-mode-map (kbd "C-c C-;") 'comment-region)

(define-key octave-mode-map [(f5)] #'octave-run)
(define-key octave-mode-map [(f9)] #'octave-toggle-breakpoint)
(define-key octave-mode-map [(ctrl f9)] #'octave-clear-all-breakpoints)
(define-key octave-mode-map [(f10)] #'octave-step)
(define-key octave-mode-map [(ctrl f5)] #'octave-quit-debug)
(define-key octave-mode-map [(ctrl f10)] #'octave-step-in)
(define-key octave-mode-map [(shift f10)] #'octave-step-out)

(define-key octave-mode-map (kbd "C-h i") #'octave-info)

(define-key inferior-octave-mode-map (kbd "C-h i") #'inferior-octave-info)

(provide 'mb-octave)
