;;;; eval defuns
;;;; prefix is key-chord jk (as usual)
;;;; b: buffer. TODO confirm message in echo
;;;; d: defun at point. TODO: confirm message in echo
;;;; t: eval defun/test at point (as usual); output to echo
;;;; T: eval test, but not defun, at point (as usual); output to echo
;;;; c: eval sexp at point (as usual); output to echo
;;;; l: eval sexp before point (as usual); output to echo
;;;;
;;;; with keyboard prefix, more verbose:
;;;; b, d: ouput eval string to REPL
;;;; t: output eval string (defun) AND result to REPL 
;;;; T, c: output result to REPL 
;;;; l: insert result at point  

(defun octave-source-buffer (&optional no-help-p buffer)
  "Source current Octave buffer, and show documentation when
point is in the texinfo region, except when NO-HELP-P is not
nil."
  (interactive)
  (inferior-octave-send-list-and-digest
   (list (format "source %s;\n" (buffer-file-name buffer))))
  (when (and (null no-help-p)
	     (null buffer)
	     (or current-prefix-arg (octave-in-documentation-p)))
    (octave-help (octave-main-defun-name))))

(cl-defun octave-last-sexp (&optional (point (point)))
  "Return the sexp before POINT as a string."
  (if (octave-comment-line-p)
    (save-excursion
      (save-restriction
       (bol)
       (narrow-to-region (re-search-forward "[# \t]*" point) point)
       (octave-last-sexp point)))
    (save-excursion
      (goto-char point)
      (forward-sexp -1)
      (when (looking-at "[[(]")
	(forward-sexp -1))
      (buffer-substring-no-properties (point) point))))

(defun octave-eval-last-sexp ()
  "Evaluate Octave sexp before point; print value in the echo area.
Interactively, with prefix argument, print output into current
buffer. Obsolete for now. In the future we should have a common
standard for where to put the eval output
* repl
* echo
* insert at point."
  (case current-prefix-arg
    ((4) (octave-eval-print-last-sexp))
    (0 (octave-eval-print-last-sexp 'size))
    (otherwise
     (inferior-octave t)
     (let ((standard-output t)
	   (sexp (octave-last-sexp)))
       (octave-send-string sexp)
       (prin1 (mapconcat 'identity inferior-octave-output-list "\n"))))))

(defun octave-update-proc-buffer (line print-repl-p)
  "Perhaps concat of inferior-octave-output-list and
inferior-octave-output-string should be done by caller and sent
to this function as a string."
  (when print-repl-p
    (insert-before-markers
     (mapconcat 'identity
		(append
		 (if octave-send-echo-input (list line) (list ""))
		 inferior-octave-output-list
		 (list inferior-octave-output-string))
		"\n"))))

(defun octave-send-string (string &optional print-repl-p)
  "Send STRING to the inferior Octave process."
  (interactive "r")
  (inferior-octave t)
  (let ((proc inferior-octave-process))
    (with-current-buffer inferior-octave-buffer
      ;; http://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00095.html
      (compilation-forget-errors)
      (setq inferior-octave-output-list nil)
      ;; I think the while loops over lines
      (with-lines (line string)
	(setq inferior-octave-receive-in-progress t)
        (inferior-octave-send-list-and-digest (list (concat line "\n")))
        (while inferior-octave-receive-in-progress
          (accept-process-output proc 0.1))
        (octave-update-proc-buffer line print-repl-p))))
  (if octave-send-show-buffer
    (display-buffer inferior-octave-buffer)))

(defun octave-send-string-silently (string)
  (octave-send-string string t))

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
	 (s (string-trim (string-match* regexp (line-string) :num 1))))
    (if s
      (let ((expr (format "\n%s" s)))
	(setf *octave-last-debug-expression* expr)
	(octave-send-string expr t))
      (message "No expression at point"))))

(defvar *octave-last-debug-expression* nil)
;;(nilf *octave-last-debug-expression*)

(defun octave-eval-defun-test ()
  "Eval comment expression after defun at point.
Also stores this expression in `*octave-last-debug-expression*'."
  (end-of-defun)
  (octave-eval-current-line))

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

(provide 'mb-octave-eval)
