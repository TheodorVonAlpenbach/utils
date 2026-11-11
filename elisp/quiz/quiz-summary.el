;;; quiz-summary.el --- Build summaries of dated quiz files -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'quiz-cleanup)
(require 'quiz-config)
(require 'quiz-format)
(require 'quiz-mode)
(require 'quiz-navigation)

(defun quiz-today ()
  "Return the current local date in ISO format."
  (format-time-string "%F"))

(defun quiz-date-add-days (date &optional days)
  "Return ISO DATE shifted by DAYS.
DAYS defaults to one."
  (unless (and (stringp date)
               (string-match-p
                "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
                date))
    (user-error "Invalid ISO date: %S" date))
  (format-time-string
   "%F"
   (time-add
    (date-to-time
     (concat date " 12:00"))
    (days-to-time (or days 1)))))

(define-obsolete-function-alias
  'add-iso-day #'quiz-date-add-days "2026-11-11")

(defun quiz-summary--validate-items (items)
  "Return ITEMS after validating its quiz prefixes."
  (unless (and (stringp items)
               (not (string-empty-p items)))
    (user-error "Quiz item prefixes cannot be empty"))
  (dolist (prefix (string-to-list items))
    (unless (string-match-p
             (regexp-quote
              (char-to-string prefix))
             quiz-item-prefixes)
      (user-error "Unknown quiz item prefix: %c"
                  prefix)))
  items)

(defun quiz-summary--file-exists-p (path)
  "Return non-nil when PATH names an existing file."
  (file-exists-p path))

(defun quiz-summary--insert-file-contents (path)
  "Insert the contents of PATH at point."
  (insert-file-contents path))

(defun quiz-summary--insert-file (date items)
  "Insert the dated quiz file for DATE, retaining ITEMS."
  (let* ((file-name
          (format "mq-%s.qz" date))
         (path
          (quiz-file-path file-name)))
    (when (quiz-summary--file-exists-p path)
      (insert (file-name-base file-name) "\n")
      (insert
       (make-string
        (length (file-name-base file-name))
        ?=)
       "\n\n")
      (let ((start (point)))
        (quiz-summary--insert-file-contents path)
        (let ((end
               (copy-marker (point) t)))
          (save-restriction
            (narrow-to-region start end)
            (quiz-summary--filter-items items))
          (set-marker end nil)))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert "\n")
      t)))

(defun quiz-summary--filter-items (items)
  "Remove item types not listed in ITEMS."
  (dolist (prefix (string-to-list quiz-item-prefixes))
    (unless (string-match-p
             (regexp-quote
              (char-to-string prefix))
             items)
      (quiz-remove-items
       (char-to-string prefix)))))

(cl-defun quiz-compile-summary
    (&key
     (iso-from (quiz-today))
     (iso-to (quiz-date-add-days iso-from))
     (items quiz-item-prefixes)
     (display t))
  "Build a quiz summary for the interval [ISO-FROM, ISO-TO).
ITEMS lists the item prefixes retained in the summary. When DISPLAY
is non-nil, switch to the generated buffer. Return that buffer."
  (quiz-summary--validate-items items)
  (unless (string< iso-from iso-to)
    (user-error
     "Quiz summary end date must follow start date"))
  (let* ((buffer-name
          (if (string=
               iso-to
               (quiz-date-add-days iso-from))
              (format "mq-%s-%s" iso-from items)
            (format
             "mq-%s-to-%s-%s"
             iso-from iso-to items)))
         (buffer
          (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (quiz-mode-input-method nil))
        (erase-buffer)
        (let ((date iso-from))
          (while (string< date iso-to)
            (quiz-summary--insert-file date items)
            (setq date
                  (quiz-date-add-days date))))
        (goto-char (point-max))
        (skip-chars-backward "\n")
        (delete-region (point) (point-max))
        (unless (bobp)
          (insert "\n"))
        (quiz-mode)))
    (when display
      (switch-to-buffer buffer))
    buffer))

(define-obsolete-function-alias
  'quiz-compile #'quiz-compile-summary "2026-11-11")

(defun quiz-summary (days items)
  "Build a summary containing DAYS days and retained ITEMS."
  (unless (and (integerp days)
               (> days 0))
    (user-error
     "Summary days must be positive: %S"
     days))
  (let ((today (quiz-today)))
    (quiz-compile-summary
     :iso-from
     (quiz-date-add-days today (- 1 days))
     :iso-to
     (quiz-date-add-days today)
     :items items)))

(defun quiz-summary-print (days)
  "Build a printable question-and-answer summary for DAYS."
  (interactive "P")
  (quiz-summary
   (if days
       (prefix-numeric-value days)
     14)
   "QA"))

(defun quiz-summary-mail (days)
  "Build a question-and-source summary for DAYS."
  (interactive "P")
  (quiz-summary
   (if days
       (prefix-numeric-value days)
     7)
   "QS"))

(defun quiz-summary-edit-question-1
    (number &optional date directory)
  "Visit question NUMBER in the quiz file for DATE and DIRECTORY."
  (interactive "nQuestion number: ")
  (find-file
   (quiz-get-file
    (or date (quiz-today))
    directory))
  (unless (quiz-goto-question number)
    (user-error
     "Question %d does not exist"
     number)))

(defun quiz-summary-edit-question
    (&optional directory)
  "Visit the source question represented at point."
  (interactive)
  (let ((number
         (quiz-current-item-number))
        date)
    (unless number
      (user-error
       "Point is not in a summary question"))
    (save-excursion
      (unless (re-search-backward
               "^mq-\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)$"
               nil t)
        (user-error
         "No dated quiz heading before point"))
      (setq date (match-string 1)))
    (quiz-summary-edit-question-1
     number date directory)))

(provide 'quiz-summary)

;;; quiz-summary.el ends here
