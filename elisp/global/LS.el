(require 'mb-utils-strings)
(require 'time)

;;; LS formatter
(cl-defun LS-fontify ()
  (interactive)
  (font-lock-add-keywords
   nil ;;i.e. only for the current buffer
   (list (format "^%s" (iso-time-regexp))
	 '(0 org-level-6))))

;;; LS parser
(cl-defun LS-buffer ()
  (or (get-buffer "LS-notes.org")
      (error "LS buffer is not loaded")))

(cl-defun LS-log-day-separator ()
  (format "^\\*\\* %s"(iso-date-regexp)))
(cl-defun LS-log-item-separator ()
  (format "^%s" (iso-time-regexp)))

(cl-defun time-within (x interval)
  (and (time<= (first interval) x) (time< x (second interval))))
;;(mapcar #'first (LS-log-days (LS-log-string)))

(cl-defun LS-estimate (&optional (from (monthstart)) (to (add-time from :month 1)) (rate 10) (sans-arbeidsgiveravgift .84) (sans-tax .6))
  (* rate sans-arbeidsgiveravgift sans-tax (sum (mapcar #'second (LS-hours from to)))))

(cl-defun LS-log-items (day-log &optional (from (midnight (first day-log))) (to (now)))
  (copy-if (bind #'time-within (list from to))
	   (split-string-regexp-pairs (second day-log) (LS-log-item-separator)
				      :trim ":* *")
	   :key #'(lambda (x) (format "%sT%s" (first day-log) (first x)))))
;;(LS-log-items (last-elt (LS-log-days (LS-log-string))) (midnight) "11:01")

(cl-defun LS-log-days (log-string &optional (from (monthstart)) (to (now)))
  (copy-if (bind #'time-within (list from to))
	   (split-string-regexp-pairs log-string (LS-log-day-separator)
				      :trim "\\*+ *" :side :left)
	   :key #'first))
;;(mapcar #'first (LS-log-days (LS-log-string)))

(cl-defun LS-extract-hours-buffer ()
  (LS-extract-hours (LS-log-string)))
;;(LS-extract-hours-buffer)

(cl-defun LS-log-string (&optional (buffer (LS-buffer)))
  (with-buffer buffer
    (save-excursion
      (let ((end (goto-char (point-max)))
	    (beg (re-search-backward "^\\* Logg$")))
	(buffer-substring-no-properties beg end)))))
;;(LS-log-string)

;;; LS utilities
(require 'mb-utils-time)
(cl-defun LS-hours (&optional (from (monthstart)) (to (now)) (lunchtime 0.5))
  (cl-loop for day-log in (LS-log-days (LS-log-string) from to)
	for items = (LS-log-items day-log from to)
	if items
	collect (let ((hours (time- (first (last-elt items))
				    (first (first items))
				    :unit :hour)))
		  (list (first day-log) (- (/ (round hours 0.25) 4.0) lunchtime)))))
;;(pp (LS-hours (monthstart -1) (monthstart)))
;;(pp (LS-hours (monthstart)))
;;(let ((x (LS-hours (monthstart -1) (monthstart)))) (list x (sum (mapcar #'second x))))


;;; hack script for renaming
;;"Hs5.4-Tz11/2-Cum15-Dir000/165-6-50x20.csv" (two directories)
;; to
;;"75/Hs5.4-Tz5.5-Cum15-Dir000/165-6-50x20.csv" (one directory)

;;(cl-defun pathcorr (&optional (root "~/projects/imms/data/amplitudes/75/"))
(cl-defun newdirname (x y)
  (cl-destructuring-bind (a b) (split-string x "z" )
    (cl-destructuring-bind (c d) (split-string y "-C")
      (format "%sz%SC%s" a (/ (float (string-to-number b)) (string-to-number c)) d))))
;;(newdirname "Hs0.25-Tz11" "2-Cum33-Dir")

(cl-defun pathcorr (&key (root "~/tmp/") (log t) (execute nil))
  "This could be a powerful operation, so set EXECUTE to NIL and check log output first"
  (cl-loop for x in (directory-files root t "Hs")
	do (cl-loop for old in (directory-files x t "Cum")
		      for parts = (split-string old "/")
		      for new = (concat* (rcons (butlast parts 2) (apply #'newdirname (last parts 2))) :in "/")
		      if log do (princ (format "%s -> %s\n" old new))
		      if execute do (rename-file old new))
	if log do (princ (format "\tDeleting %s\n" x))
	if execute do (delete-directory x)))
;;(pathcorr :log t :execute t)
;;(pathcorr :root "~/projects/imms/data/amplitudes/75/" :log t :execute nil)

(cl-defun move-directories-lt (parent-dir dest-dir string &key (log t) (execute nil))
  "Moves every directory DIR under PARENT-DIR to DEST-DIR iff DIR < string by string comparison.
PARENT-DIR and DEST-DIR are full paths, while STRING should be
compared to the relative directory names in PARENT-DIR"
  (cl-loop for x in (directory-files parent-dir nil "Hs")
	if (string< x string)
	do (let ((target (expand-file-name x parent-dir))
		 (dest (expand-file-name x dest-dir)))
	     (when log (princ (format "%s -> %s\n" target dest)))
	     (when execute (rename-file target dest)))))
;;(move-directories-lt "/home/MBe/projects/imms/data/amplitudes/75" "~/tmp/75" "Hs1.75-Tz12.5Cum4-Dir090" :execute t)
;;"Hs1.75-Tz12.5Cum4-Dir075" TODO: process this

(awhen (get-buffer "LS-notes.org")
  (with-buffer it
    (setf fill-paragraph-function #'fill-time-paragraph)))

(cl-defun ls-insert-arrival-time ()
  (interactive)
  (if (string/= (buffer-name) "LS-notes.org")
    (warn "Function ls-insert-arrival-time is only valid in buffer LS-notes.org.")
    ;;else
    (goto-char (point-max))
    (just-one-blank-line)
    (org-insert-heading)
    (insert-date nil)
    (insert-parentheses* nil)
    (eol)
    (insert "\n")
    (insert-time nil)
    (insert " Ankomst\n")))

(cl-defun ls-insert-depature-time ()
  (interactive)
  (if (string/= (buffer-name) "LS-notes.org")
    (warn "Function ls-insert-arrival-time is only valid in buffer LS-notes.org.")
    ;;else
    (goto-char (point-max))
    (just-one-blank-line)
    (insert-time nil)
    (insert " Hjem\n")))

(cl-defun ls-project-number->tag (project-number)
  "Convert integer PROJECT-NUMBER to corresponding project tag."
  (insert-sequence (format "%05d" project-number) "_" :start1 2 :end1 2))
;;(ls-project-number->tag 15010)

(cl-defun ls-project-name (project-number)
  "Return the directory name under ~/systems containing PROJECT-NUMBER."
  (cl-find (ls-project-number->tag project-number)
	(directory-files "~/systems")
	:test #'string-match*))
;;(ls-project-name 14029)

(provide 'LS)
