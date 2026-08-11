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

(defun ada-expand-jira-tag (jira-tag-suffix)
  (let* ((jira-tag-default "ADA-16400")
	 (n (length jira-tag-default))
	 (l (length jira-tag-suffix)))
    (concat (substring jira-tag-default 0 (- n l)) jira-tag-suffix)))
;;(ada-expand-jira-tag "30")

(defun ada-open-readme-in-task-folder (jira-tag-suffix)
  "Opens ~/ada-tasks/JIRA-TAG/README.md.
If the JIRA-TAG subfolder does not exist, create it."
  (interactive "sJIRA tag suffix: ")
  (let* ((dir (expand-file-name (ada-expand-jira-tag jira-tag-suffix)
				"~/ada-tasks"))
	 (filename (expand-file-name "README.org" dir)))
    (if (file-directory-p dir)
      (find-file filename)
      (when (yes-or-no-p
	     "Task directory does not exist. Do you want to create one? ")
	(make-directory dir t)
	(find-file filename)))))
