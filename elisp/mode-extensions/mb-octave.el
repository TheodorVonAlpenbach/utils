(require 'octave)

(defun octave-help (fn)
  "Display the documentation of FN."
  (interactive (list (octave-completing-read)))
  (inferior-octave-send-list-and-digest
   (list (format "help ('%s');\n" fn)))
  (let ((lines inferior-octave-output-list)
        (inhibit-read-only t))
    (when (string-match "error: \\(.*\\)$" (car lines))
      (error "%s" (match-string 1 (car lines))))
    (with-help-window octave-help-buffer
      (princ (mapconcat 'identity lines "\n"))
      (with-current-buffer octave-help-buffer
        ;; Bound to t so that `help-buffer' returns current buffer for
        ;; `help-setup-xref'.
        (let ((help-xref-following t))
          (help-setup-xref (list 'octave-help fn)
                           (called-interactively-p 'interactive)))
        ;; Note: can be turned off by suppress_verbose_help_message.
        ;;
        ;; Remove boring trailing text: Additional help for built-in functions
        ;; and operators ...
        (goto-char (point-max))
        (when (search-backward "\n\n\n" nil t)
          (goto-char (match-beginning 0))
          (delete-region (point) (point-max)))
        ;; File name highlight
        (goto-char (point-min))
        (when (re-search-forward "from the file \\(.*\\)$"
                                 (line-end-position)
                                 t)
          (let* ((file (match-string 1))
                 (dir (file-name-directory
                       (directory-file-name (file-name-directory file)))))
            (replace-match "" nil nil nil 1)
            (insert "`")
            ;; Include the parent directory which may be regarded as
            ;; the category for the FN.
            (help-insert-xref-button (file-relative-name file dir)
                                     'octave-help-file fn)
            (insert "'")))
        ;; Make 'See also' clickable.
        (with-syntax-table octave-mode-syntax-table
          (when (re-search-forward "^\\s-*See also:" nil t)
            (let ((end (save-excursion (re-search-forward "^\\s-*$" nil t))))
              (while (re-search-forward
                      "\\s-*\\([^,\n]+?\\)\\s-*\\(?:[,]\\|[.]?$\\)" end t)
                (make-text-button (match-beginning 1) (match-end 1)
                                  :type 'octave-help-function)))))
        (octave-help-mode)))))
;;(inferior-octave-send-list-and-digest (list (format "help ('%s');\n" "egina_export")))

(defun octave-function-name ()
  (apply #'buffer-substring-no-properties (last (octave-function-file-p) 2)))

(defun octave-eval-buffer ()
  "Source current Octave buffer, and show documentation when
point is in the texinfo region."
  (interactive)
  (inferior-octave-send-list-and-digest
   (list (format "source %s;\n" (buffer-file-name))))
  (when (octave-in-documentation-p)
    (octave-help (octave-function-name))))

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
	 (s (string-trim (string-match* regexp (line-string) :num 1))))
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

(add-hook 'octave-mode-hook 'turn-on-eldoc-mode)
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

(defun octave-list-all-breakpoints ()
  (interactive)
  (octave-send-string "dbstatus"))

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

(defun regexp-or (&rest regexps)
  "Join together REGEXPS with \\| (OR)."
  (concat* regexps :in "\\|" :key #'(lambda (x) (format "\\(%s\\)" x))))
;;(regexp-or "abc" "def")

(defun octave-fill-documentation-paragraph ()
  "Fills an Octave documentation paragraph.
This functionality is not well covered by octave-fill-paragraph"
  (let ((fill-prefix "## ")
	(paragraph-separate (regexp-or paragraph-separate
				       "## -*- texinfo -*-"
				       "## @seealso"
				       "## @deftypefn"))
	(fill-paragraph-function nil))
    (if (use-region-p)
      (fill-region (region-beginning) (region-end))
      (fill-paragraph))))

(defun octave-in-main-documentation-p ()
  "Returns non-nil iff POINT is in the function file comment region."
  (i-contains (octave-function-file-comment) (point)))

(defun octave-in-documentation-p ()
  "Returns non-nil iff point is a larger Octave comment section.
Currently only the documentation of the main function is
supported. To implement the general version we need to know if we
are between functions.

Consider also `octave-function-file-comment'."
  (string-match
     "\\(^##.*\\)"
     (line-string)))

(defun mb-octave-fill-paragraph ()
  (interactive)
  (if (octave-in-documentation-p)
    (octave-fill-documentation-paragraph)
    (octave-fill-paragraph)))

(defun insert-texinfo-var ()
  (interactive)
  (if (symbol-at-point)
    (destructuring-bind (beg . end) (bounds-of-thing-at-point 'symbol)
      (insert (format "@var{%s}" (delete-and-extract-region beg end))))
    (insert "@var{}")
    (backward-char 1)))

(defun mb-octave-test-buffer-file ()
  (interactive)
  (octave-send-string (format "test %s" (buffer-file-name))))
;;(mb-octave-test-buffer-file)

;;; shortcuts
(define-key octave-mode-map (kbd "C-x C-e") 'octave-eval-last-sexp)
(define-key octave-mode-map (kbd "C-M-x") 'octave-eval-defun)
(define-key octave-mode-map (kbd "M-q") 'mb-octave-fill-paragraph)
(define-key octave-mode-map (kbd "C-c C-;") 'comment-region)

(define-key octave-mode-map [(f5)] #'octave-run)
(define-key octave-mode-map [(f9)] #'octave-toggle-breakpoint)
(define-key octave-mode-map [(ctrl f9)] #'octave-clear-all-breakpoints)
(define-key octave-mode-map [(meta f9)] #'octave-list-all-breakpoints)
(define-key octave-mode-map [(f10)] #'octave-step)
(define-key octave-mode-map [(ctrl f5)] #'octave-quit-debug)
(define-key octave-mode-map [(ctrl f10)] #'octave-step-in)
(define-key octave-mode-map [(shift f10)] #'octave-step-out)

(define-key octave-mode-map (kbd "C-h i") #'octave-info)

(define-key inferior-octave-mode-map (kbd "C-h i") #'inferior-octave-info)

(define-key octave-mode-map (kbd "C-c i @") #'insert-texinfo-var)

(provide 'mb-octave)
