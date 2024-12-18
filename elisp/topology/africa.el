(cl-defun africa-read-csv (&optional (filename-hav "hav.csv")
				   (filename-boundaries "afrika.csv")
				   (path "c:/cygwin/usr/libs/emacs-21.3/site-lisp/mb-lisp/topology/"))
  (let ((hav (mapcar #'first (parse-csv-file (concat path filename-hav))))
	(boundaries (parse-csv-file (concat path filename-boundaries))))
    (cl-loop for boundary in boundaries
		for country1 = (first boundary)
		for country2 = (second boundary) 
		if (and (not (member country2 hav))
			(not (member country1 '("Ceuta" "Melilla")))
			(not (member country2 '("Ceuta" "Melilla"))))
		collect (list (intern country1)
			      (intern country2) 
			      1))))
;;(africa-read-csv)
;;(prin1 (extract-nodes (africa-read-csv)))

(cl-defun print-africa-matrix (&optional (filename-hav "hav.csv")
				      (filename-boundaries "afrika.csv")
				      (filename-csv "africa-matrix.csv")
				      (path "c:/cygwin/usr/libs/emacs-21.3/site-lisp/mb-lisp/topology/"))
  (let* ((segments (africa-read-csv))
	 (network (init-network segments)))
    (fill-network network segments)
    (string-to-file (csv-string (dijkstra-matrix network))
		    (concat path filename-csv))))
;;(print-africa-matrix)
;;(print* 'd "c:/cygwin/usr/libs/emacs-21.3/site-lisp/mb-lisp/topology/qwe")
