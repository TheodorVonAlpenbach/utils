;;;; Import questions
(defun cram-import-csv (path)
  "Import cram tasks from CSV file at PATH.
See header of imports.cvs for correct format"
  (let ((rows (maptree #'(lambda (s) (if (empty-string-p s) nil s))
		(parse-csv-file path))))
    (loop for row in (rest rows)
	  ;; ignore ratings / levels for now
	  for args = (rcons (butlast row) +cram-default-rating+)
	  do (cram-db-insert-problem args))))
;;(cram-import-csv "~/projects/utils/elisp/games/cram/imports.csv")
;;(cram-db-problems)
;;(CAUTION! ld-delete-if #'always :problem)
(provide 'cram-import)
