(defun find-available-id-in-containers-json-1 ()
  (1+ (min-value
       (cl-loop for x in
		;; ignore the two first matches, as they are system containers
		(nthcdr 2 (string-matches-exact
			   "\"userContextId\": \\([0-9]+\\)"
			   (buffer-string-no-properties)))
		collect (string-to-integer
			 (string-trim (last-elt (split-string x ":")))))
       :test #'>)))

(defun find-available-id-in-containers-json ()
  (interactive)
  (message "Least available value for userContextId is %d"
	   (find-available-id-in-containers-json-1)))

(cl-defun ada-tag-number (number template-number &optional (cut-off 200))
  "Convert abbrevated NUMBER to TAG-NUMBER.
TEMPLATE-NUMBER  Conversions
17000            0 -> 17000, 199 -> 17199, 200 -> 16200
17100            0 -> 17100, 299 -> 17299, 300 -> 16300
17200            0 -> 17200, 399 -> 17399, 400 -> 16400
17800            0 -> 17800, 999 -> 17999, 100 -> 17100
"
  (let ((cut-off-number (+ template-number cut-off))
	(mod (expt 10 (num-digits number))))
    (cl-destructuring-bind (split remainder) (split-mod template-number mod)
      (let ((tag-number (+ number split)))
	(if (< tag-number (+ template-number cut-off))
	  tag-number
	  (- tag-number 1000))))))
;;(ada-tag-number 356 17100)

(cl-defun ada-jira-tag (number &optional
				 (ada-prefix "ADA-")
				 (template-number 17000)
				 (cut-off 200))
  (format "%s%d" ada-prefix (ada-tag-number number template-number cut-off)))
;;(ada-jira-tag 984)

(defun ada-open-readme-in-task-folder (jira-tag-suffix)
  "Opens ~/ada-tasks/JIRA-TAG/README.md.
If the JIRA-TAG subfolder does not exist, create it."
  (interactive "nJIRA tag suffix: ")
  (let* ((dir (expand-file-name (ada-jira-tag jira-tag-suffix) "~/ada-tasks"))
	 (filename (expand-file-name "README.org" dir)))
    (if (file-directory-p dir)
      (find-file filename)
      (when (yes-or-no-p
	     (format
		 "Task directory %s does not exist. Do you want to create one? "
	       dir))
	(make-directory dir t)
	(with-current-buffer (find-file-noselect filename)
	  (goto-char (point-max)))))))
