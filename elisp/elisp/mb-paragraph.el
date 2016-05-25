(defconst time-regexp "[0-2][0-9]:[0-5][0-9]")
(defconst time-regexp "[0-2][0-9]:[0-5][0-9]")
(defun kill-time-exp () (kill-word 2))
(defconst schedule-paragraph-start time-regexp)
(defconst schedule-paragraph-separate (format "%s\\|[\n\t ]*$" time-regexp))
(defconst schedule-paragraph-indent 6)
(defvar schedule-hanging-column 7)
(defconst quiz-hanging-space 1)

(defun kill-region* (beg end &optional with-properties)
  "Same as `kill-region' but returns the killed text as string.
Also, it is not interactive. I similar function that does not
store the deleted region in the kill ring, is
`delete-and-extract-region'"
  (prog1 (if with-properties
	   (buffer-substring beg end)
	   (buffer-substring-no-properties beg end))
    (kill-region beg end)))

(defun kill-current-paragraph ()
  (kill-region* (bop) (eop)))

(defun kill-current-schedule-paragraph ()
  "Kills current paragraph and returns the pair (TIMESTAMP TEXT)"
  (let ((res (split-string* (kill-current-paragraph) time-regexp :right)))
    (if (= (length res) 1)
      (cons nil res) res)))

(defun schedule-fill-paragraph (&optional justify)
  "Fills schedule paragraphs. Too macroish implemented."
  (interactive "P")
  (unless (string-match* (iso-date-regexp) (current-line-as-string))
      (destructuring-bind (timestamp text) (kill-current-schedule-paragraph)
	(let ((new-text (with-temp-buffer
			  (insert (string-trim-left (remove 10 text)))
			  (bob)
			  (insert "       ")
			  (eob)
			  (fill-paragraph)
			  (buffer-string))))
	  (when timestamp (cl-replace new-text timestamp))
	  (insert new-text)
	  (newline)))))

(defun set-locals-arbeidslog ()
  (with-buffer "test.txt"
    (setq-local paragraph-start schedule-paragraph-start)
    (setq-local fill-prefix "       ")
    (setq-local fill-paragraph-function #'schedule-fill-paragraph)))

(provide 'mb-paragraph)
