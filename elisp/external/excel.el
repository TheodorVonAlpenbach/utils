(cl-defun xls-to-csv1 (filename args &optional copy)
  (let ((res (apply #'call-process* "xlsx2csv" filename args)))
    (if (string-match "^Invalid" res)
      (error "Conversion failed. Is the document open in LibreOffice?")
      res)))

(cl-defun xls-to-csv (filename &key (id 1) name
				all-p include-sheet-regex exclude-sheet-regex
				output-encoding
				column-delimiter line-delimiter sheet-delimiter
				date-format
				skip-empty-columns-p skip-empty-lines-p
				escape-p with-hyperlinks-p)
  "Default :OUTPUT-ENCODING is \"utf-8\". If :NAME is not nil, it overrides :ID."
  (let ((not-supported-key-words
	 '(all-p include-sheet-regex exclude-sheet-regex output-encoding
	   column-delimiter sheet-delimiter date-format skip-empty-columns-p
	   skip-empty-lines-p escape-p with-hyperlinks-p)))
    (awhen (copy-if #'eval not-supported-key-words)
      (message (concat* it
		 :pre "Warning! Keyword "
		 :in ", "
		 :suf " not implemented"
		 :key #'(lambda (k) (concat ":" (upcase (sstring k)))))))
    (let ((args '()))
      (when id
	(push-list (list "-s" (number-to-string id)) args))
      (when name
	(push-list (list "-n" (sstring name)) args))
      (when escape-p
	(push "-e" args))
      (when column-delimiter
	(push-list (list "-d" (sstring column-delimiter)) args))
      (when line-delimiter
	(push-list (list "-l" (sstring line-delimiter)) args))
      (message (concat* (cons "xlsx2csv" (cons filename args)) :pre "Calling " :in " "))
      (xls-to-csv1 filename args))))
;;(xls-to-csv (car +aa-sensor-list-source+) :column-delimiter "|" :line-delimiter nil :id 3)
;;(xls-to-csv (car +aa-sensor-list-source+) :column-delimiter "|" :id 3)

(cl-defun read-xls (filename &rest args)
  (parse-csv-string
   (apply #'xls-to-csv filename args)
   ;; note line-separator is the windows sep ^M\n
   ","))
;;(read-xls "/home/eier/projects/aves/aves.xlsx" :id 2)

;;(defconst +aves+ (read-xls "/home/eier/projects/aves/aves.xlsx"))

(provide 'excel)
