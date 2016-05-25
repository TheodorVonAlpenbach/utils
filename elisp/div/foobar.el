(defvar foobar-substitutions)
(setq foobar-substitutions
  (list
    (list "\\([cdefgab]\\)[- ]+\\(sharp\\|flat\\)? *\\(major\\|minor\\)?"
	  #'foobar-translate-key)))

(defun foobar-translate-key (string)
  "Translates typical English keys to German"
  (let* ((ms1 (match-string 1 string))
	 (ms2 (match-string 2 string))
	 (ms3 (match-string 3 string))
	 (res (if ms2
		;; mode modifier (flat or sharp)
		(if (string-match "sharp" ms2)
		  ;; sharp
		  (if (string-match "b" ms1)
		    "his"
		    (concat ms1 "is"))
		  ;; flat
		  (if (string-match "[ae]" ms1)
		    (concat ms1 "s")
		    (if (string-match "b" ms1)
		      "b"
		      (concat ms1 "es"))))
		;; no mode modifier
		(if (string-match "b" ms1)
		  "h"		  
		  ms1))))
    (if ms3
      (if (string-match "major" ms3)
	(capitalize res)
	(downcase res))
      res)))

(defun foobar-clean-text (beg end)
  (interactive "r")
  (let ((newstring (buffer-substring beg end)))
    (loop for foobar-substitution in foobar-substitutions
	  do (setq newstring 
		   (apply #'string-replace-f
			  newstring
			  foobar-substitution)))
    (unless (if (stringp newstring)
	      (equal newstring "")
	      (equal (nth 0 newstring) ""))
      (save-excursion
	(kill-region beg end)
	(goto-char beg)
	(insert newstring)))))


