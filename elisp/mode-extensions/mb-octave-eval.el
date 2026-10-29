;;; mb-octave-eval.el --- Evaluate Octave code -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'octave)
(require 'subr-x)

(defcustom mb-octave-response-timeout 10
  "Seconds to wait for an inferior Octave response."
  :type 'number
  :group 'octave)

(defvaralias '*octave-last-debug-expression*
  'octave-last-debug-expression)

(defvar octave-last-debug-expression nil
  "Most recent expression evaluated by `octave-eval-current-line'.")

(defvaralias '*octave-eval-history* 'octave-eval-history)

(defvar octave-eval-history nil
  "History for `octave-eval-expression'.")

(defun mb-octave--quote-string (string)
  "Return STRING as an Octave double-quoted string literal."
  (concat "\""
          (replace-regexp-in-string "[\\\\\"]" "\\\\\\&" string)
          "\""))

(defun octave-source-buffer (&optional no-help-p buffer)
  "Source the Octave file visited by BUFFER.
BUFFER defaults to the current buffer.  Unless NO-HELP-P is
non-nil, optionally display documentation when requested from
the current buffer."
  (interactive)
  (let* ((target (or buffer (current-buffer)))
         (filename (buffer-file-name target)))
    (unless filename
      (user-error "Octave buffer is not visiting a file"))
    (inferior-octave-send-list-and-digest
     (list (format "source (%s);\n"
                   (mb-octave--quote-string filename))))
    (when (and (not no-help-p)
               (not buffer)
               (or current-prefix-arg
                   (and (fboundp 'octave-in-documentation-p)
                        (octave-in-documentation-p)))
               (fboundp 'octave-help)
               (fboundp 'octave-main-defun-name))
      (octave-help (octave-main-defun-name)))))

(defun mb-octave--comment-expression (point)
  "Return comment text before POINT as an expression, or nil."
  (save-excursion
    (goto-char point)
    (let ((text
           (buffer-substring-no-properties
            (line-beginning-position) point)))
      (when (string-match "\\`[ \t]*#+[ \t]*\\(.*\\)\\'" text)
        (match-string 1 text)))))

(cl-defun octave-last-sexp (&optional (point (point)))
  "Return the Octave expression before POINT as a string."
  (let ((comment-expression (mb-octave--comment-expression point)))
    (if comment-expression
        (with-temp-buffer
          (insert comment-expression)
          (octave-last-sexp (point-max)))
      (save-excursion
        (goto-char point)
        (forward-sexp -1)
        (when (looking-at-p "[[(]")
          (forward-sexp -1))
        (buffer-substring-no-properties (point) point)))))

(defun mb-octave--response-string ()
  "Return the current inferior Octave response as a string."
  (string-join
   (append inferior-octave-output-list
           (unless (string-empty-p
                    (or inferior-octave-output-string ""))
             (list inferior-octave-output-string)))
   "\n"))

(defun octave-update-proc-buffer (line print-repl-p)
  "Insert the response for LINE when PRINT-REPL-P is non-nil."
  (when print-repl-p
    (insert-before-markers
     (string-join
      (append
       (when octave-send-echo-input (list line))
       inferior-octave-output-list
       (unless (string-empty-p
                (or inferior-octave-output-string ""))
         (list inferior-octave-output-string)))
      "\n"))))

(defun mb-octave--wait-for-response (process)
  "Wait for PROCESS to finish the current response."
  (let ((deadline (+ (float-time) mb-octave-response-timeout)))
    (while inferior-octave-receive-in-progress
      (when (>= (float-time) deadline)
        (error "Timed out waiting for Octave response"))
      (accept-process-output process 0.1))))

(defun octave-send-string (string &optional print-repl-p)
  "Send STRING to inferior Octave and return its output.
When PRINT-REPL-P is non-nil, also insert responses in the
inferior Octave buffer."
  (interactive "sOctave expression: ")
  (inferior-octave t)
  (let ((process inferior-octave-process)
        responses)
    (with-current-buffer inferior-octave-buffer
      (compilation-forget-errors)
      (dolist (line (string-lines string))
        (setq inferior-octave-output-list nil
              inferior-octave-output-string ""
              inferior-octave-receive-in-progress t)
        (inferior-octave-send-list-and-digest (list (concat line "\n")))
        (mb-octave--wait-for-response process)
        (let ((response (mb-octave--response-string)))
          (unless (string-empty-p response)
            (push response responses)))
        (octave-update-proc-buffer line print-repl-p)))
    (when octave-send-show-buffer
      (display-buffer inferior-octave-buffer))
    (string-join (nreverse responses) "\n")))

(defun octave-send-string-silently (string)
  "Send STRING to Octave and insert its response in the process buffer."
  (octave-send-string string t))

(defun octave-eval-current-line ()
  "Evaluate the expression on the current line.
Leading whitespace and Octave comment markers are ignored."
  (interactive)
  (let* ((line
          (buffer-substring-no-properties
           (line-beginning-position) (line-end-position)))
         (expression
          (string-trim
           (replace-regexp-in-string "\\`[[:space:]#]*" "" line))))
    (when (string-empty-p expression)
      (user-error "No Octave expression on current line"))
    (setq octave-last-debug-expression (concat "\n" expression))
    (octave-send-string octave-last-debug-expression t)))

(defun octave-eval-defun-test ()
  "Evaluate the comment expression following the defun at point."
  (interactive)
  (end-of-defun)
  (octave-eval-current-line))

(defun octave-eval-expression ()
  "Prompt for and evaluate an Octave expression."
  (interactive)
  (let* ((default (car octave-eval-history))
         (expression
          (read-string
           (if default
               (format "Eval Octave expression (%s): " default)
             "Eval Octave expression: ")
           nil 'octave-eval-history default)))
    (octave-send-string expression)))

(defun octave-eval-last-sexp ()
  "Evaluate the Octave expression before point and display its output."
  (interactive)
  (when current-prefix-arg
    (user-error "Prefix evaluation is no longer supported"))
  (let ((output (octave-send-string (octave-last-sexp))))
    (unless (string-empty-p output)
      (message "%s" output))
    output))

(provide 'mb-octave-eval)

;;; mb-octave-eval.el ends here
