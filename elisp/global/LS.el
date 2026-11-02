;;; LS.el --- Work-log parsing and insertion helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'mb-utils-buffer)
(require 'mb-utils-iso-time)
(require 'mb-utils-strings)
(require 'mb-utils-time)

(autoload 'insert-date "mb-insert" nil t)
(autoload 'insert-time "mb-insert" nil t)

(defgroup ls nil
  "Helpers for the personal work log."
  :group 'org)

(defcustom ls-notes-buffer-name "LS-notes.org"
  "Name of the work-log buffer."
  :type 'string)

(defcustom ls-project-directory "~/systems"
  "Directory containing project directories."
  :type 'directory)

(defun ls-fontify ()
  "Fontify ISO time prefixes in the current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   `((,(concat "^" (iso-time-regexp)) (0 'org-level-6)))))

(defalias 'LS-fontify #'ls-fontify)

(defun ls-buffer ()
  "Return the work-log buffer or report that it is unavailable."
  (or (get-buffer ls-notes-buffer-name)
      (user-error "Work-log buffer is not loaded: %s"
                  ls-notes-buffer-name)))

(defalias 'LS-buffer #'ls-buffer)

(defun ls-log-day-separator ()
  "Return the regexp introducing a work-log day."
  (concat "^\\*\\* " (iso-date-regexp)))

(defalias 'LS-log-day-separator #'ls-log-day-separator)

(defun ls-log-item-separator ()
  "Return the regexp introducing a timed work-log item."
  (concat "^" (iso-time-regexp)))

(defalias 'LS-log-item-separator #'ls-log-item-separator)

(defun ls-time-within-p (time interval)
  "Return non-nil when TIME is within half-open INTERVAL."
  (and (time<= (car interval) time)
       (time< time (cadr interval))))

(defalias 'time-within #'ls-time-within-p)

(cl-defun ls-estimate
    (&optional
     (from (monthstart))
     (to (add-time from :month 1))
     (rate 10)
     (employer-factor 0.84)
     (tax-factor 0.6))
  "Estimate net payment for work between FROM and TO.
RATE is multiplied by logged hours, EMPLOYER-FACTOR, and
TAX-FACTOR."
  (* rate employer-factor tax-factor
     (apply #'+ (mapcar #'cadr (ls-hours from to)))))

(defalias 'LS-estimate #'ls-estimate)

(cl-defun ls-log-items
    (day-log
     &optional
     (from (midnight (car day-log)))
     (to (now)))
  "Return timed items from DAY-LOG within FROM and TO.
DAY-LOG is a pair of an ISO date and its log text."
  (cl-remove-if-not
   (lambda (item)
     (ls-time-within-p
      (format "%sT%s" (car day-log) (car item))
      (list from to)))
   (split-string-regexp-pairs
    (cadr day-log) (ls-log-item-separator) :trim ":* *")))

(defalias 'LS-log-items #'ls-log-items)

(cl-defun ls-log-days
    (log-string &optional (from (monthstart)) (to (now)))
  "Return day logs in LOG-STRING within FROM and TO."
  (cl-remove-if-not
   (lambda (day-log)
     (ls-time-within-p (car day-log) (list from to)))
   (split-string-regexp-pairs
    log-string (ls-log-day-separator)
    :trim "\\*+ *" :side :left)))

(defalias 'LS-log-days #'ls-log-days)

(defun ls-log-string (&optional buffer)
  "Return the log section from BUFFER.
BUFFER defaults to `ls-buffer'."
  (with-current-buffer (or buffer (ls-buffer))
    (save-excursion
      (goto-char (point-max))
      (unless (re-search-backward "^\\* Logg$" nil t)
        (user-error "Work-log heading not found"))
      (buffer-substring-no-properties (point) (point-max)))))

(defalias 'LS-log-string #'ls-log-string)

(cl-defun ls-hours
    (&optional (from (monthstart)) (to (now)) (lunchtime 0.5))
  "Return logged hours by day between FROM and TO.
Subtract LUNCHTIME from each nonempty day."
  (cl-loop for day-log in (ls-log-days (ls-log-string) from to)
           for items = (ls-log-items day-log from to)
           when items
           collect
           (let* ((date (car day-log))
                  (hours
                   (time- (format "%sT%s" date (caar (last items)))
                          (format "%sT%s" date (caar items))
                          :hour)))
             (list (car day-log)
                   (- (/ (round hours 0.25) 4.0)
                      lunchtime)))))

(defalias 'LS-hours #'ls-hours)

(defun ls--ensure-notes-buffer ()
  "Report an error unless the current buffer is the work log."
  (unless (equal (buffer-name) ls-notes-buffer-name)
    (user-error "Command is only valid in buffer %s"
                ls-notes-buffer-name)))

(defun ls-insert-arrival-time ()
  "Append today's arrival entry to the work log."
  (interactive)
  (ls--ensure-notes-buffer)
  (goto-char (point-max))
  (just-one-blank-line)
  (insert "** ")
  (insert-date nil)
  (insert "()\n")
  (insert-time nil)
  (insert " Ankomst\n"))

(defun ls-insert-departure-time ()
  "Append the current departure time to the work log."
  (interactive)
  (ls--ensure-notes-buffer)
  (goto-char (point-max))
  (just-one-blank-line)
  (insert-time nil)
  (insert " Hjem\n"))

(defalias 'ls-insert-depature-time #'ls-insert-departure-time)

(defun ls-project-number->tag (project-number)
  "Convert integer PROJECT-NUMBER to its underscore project tag."
  (let ((number (format "%05d" project-number)))
    (concat (substring number 0 2)
            "_"
            (substring number 2))))

(defun ls-project-name (project-number)
  "Return the project directory containing PROJECT-NUMBER."
  (let ((tag (ls-project-number->tag project-number)))
    (cl-find-if
     (lambda (name) (string-match-p (regexp-quote tag) name))
     (directory-files ls-project-directory nil
                      directory-files-no-dot-files-regexp))))

(provide 'LS)

;;; LS.el ends here
