;;; mb-paragraph.el --- Paragraph and sentence helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers for schedule-style paragraphs, marking complete words and sexps,
;; and the personal single-spaced sentence motion formerly implemented by a
;; global override of `forward-sentence'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mb-utils-buffer)
(require 'mb-utils-io)
(require 'mb-utils-iso-time)
(require 'mb-utils-strings)

(defvaralias 'time-regexp 'mb-paragraph-time-regexp)

(defconst mb-paragraph-time-regexp "[0-2][0-9]:[0-5][0-9]"
  "Regexp matching the HH:MM timestamps used in schedule paragraphs.")

(defconst schedule-paragraph-start mb-paragraph-time-regexp
  "Regexp matching the beginning of a schedule paragraph.")

(defconst schedule-paragraph-separate
  (format "%s\\|[\n\t ]*$" mb-paragraph-time-regexp)
  "Regexp separating schedule paragraphs.")

(defconst schedule-paragraph-indent 6
  "Legacy indentation width for schedule paragraphs.")

(defvar schedule-hanging-column 7
  "Column at which schedule paragraph text begins.")

(defconst quiz-hanging-space 1
  "Legacy extra indentation used by quiz paragraphs.")

(defconst mb-paragraph-sentence-end
  "[.?!:][]\"')}]*\\([ \t\n]+[A-W]\\)"
  "Regexp used by `mb-paragraph-forward-sentence'.")

(defun kill-time-exp ()
  "Kill the two words making up the time expression at point."
  (interactive)
  (kill-word 2))

(defun kill-region* (beg end &optional with-properties)
  "Kill text from BEG to END and return it as a string.
When WITH-PROPERTIES is non-nil, retain text properties in the result."
  (prog1 (if with-properties
             (buffer-substring beg end)
           (buffer-substring-no-properties beg end))
    (kill-region beg end)))

(defun kill-current-paragraph ()
  "Kill the current paragraph and return its text."
  (interactive)
  (kill-region* (bop) (eop)))

(defun kill-current-schedule-paragraph ()
  "Kill the current paragraph and return (TIMESTAMP TEXT).
TIMESTAMP is nil when the paragraph has no timestamp."
  (let ((parts
         (split-string* (kill-current-paragraph)
                        mb-paragraph-time-regexp :right)))
    (if (= (length parts) 1)
        (cons nil parts)
      parts)))

(defun schedule-fill-paragraph (&optional justify)
  "Fill the current schedule paragraph.
Do nothing on an ISO-date heading.  JUSTIFY has the same meaning as in
`fill-paragraph'."
  (interactive "P")
  (unless (string-match-p (iso-date-regexp) (line-string))
    (cl-destructuring-bind (timestamp text)
        (kill-current-schedule-paragraph)
      (let* ((column fill-column)
             (new-text
             (with-temp-buffer
               (setq fill-column column)
               (insert (string-trim-left
                        (replace-regexp-in-string "\n" "" text)))
               (goto-char (point-min))
               (insert (make-string schedule-hanging-column ?\s))
               (goto-char (point-max))
               (fill-paragraph justify)
               (buffer-string))))
        (when timestamp
          (cl-replace new-text timestamp))
        (insert new-text)
        (newline)))))

(defun set-locals-arbeidslog (&optional buffer)
  "Install schedule paragraph settings in BUFFER.
BUFFER defaults to the existing buffer named \"test.txt\" for
compatibility with the original command."
  (interactive)
  (with-buffer (or buffer "test.txt")
    (setq-local paragraph-start schedule-paragraph-start)
    (setq-local fill-prefix
                (make-string schedule-hanging-column ?\s))
    (setq-local fill-paragraph-function #'schedule-fill-paragraph)))

(defun mark-whole-word ()
  "Activate the region around the whole word at point."
  (interactive)
  (forward-word 1)
  (push-mark nil t t)
  (backward-word 1))

(defun mark-whole-sexp ()
  "Activate the region around the whole sexp at point."
  (interactive)
  (forward-sexp 1)
  (push-mark nil t t)
  (backward-sexp 1))

(defun mb-paragraph-forward-sentence (&optional count)
  "Move forward COUNT sentences using `mb-paragraph-sentence-end'.
With negative COUNT, move backward.  Unlike the obsolete original
implementation, this command does not replace Emacs's
`forward-sentence' globally."
  (interactive "p")
  (let ((case-fold-search nil)
        (sentence-end mb-paragraph-sentence-end)
        (sentence-end-double-space nil))
    (forward-sentence (or count 1))))

(provide 'mb-paragraph)

;;; mb-paragraph.el ends here
