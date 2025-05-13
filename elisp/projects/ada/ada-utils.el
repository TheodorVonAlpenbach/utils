(defun find-available-id-in-containers-json ()
  (interactive)
  (cl-loop for x in
	    ;; ignore the two first matches, as they are system containers
	    (nthcdr 2 (string-matches-exact
		       "\"userContextId\": \\([0-9]+\\)"
		       (buffer-string-no-properties)))
	    collect (string-to-integer
		     (string-trim (last-elt (split-string x ":"))))))
