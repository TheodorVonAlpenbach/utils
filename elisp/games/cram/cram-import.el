;;;; Import questions
(defun cram-import-csv (path)
  "Import cram tasks from CSV file at PATH.
See header of imports.cvs for correct format"
  (let ((rows (maptree #'(lambda (s) (if (empty-string-p s) nil s))
		(parse-csv-file path))))
    (loop for row in (rest rows)
	  ;; ignore ratings / levels for now
	  for args = (rcons (butlast row) +cram-default-rating+)
	  do (unless (cram-db-insert-problem args)
	       ;; A q with same :source-id is probably present in DB.
	       ;; Try update instead
	       (ld-update :problem
		 (string= :source-id (first args))
		 (butlast (rest args))
		 (:question :answer :picture :alternatives :hints))))))
;;(cram-import-csv "~/projects/utils/elisp/games/cram/serbokroatisk.csv")
;;(length (ld-select :problem))
;;(ld-save-database *current-database*)
;;(cram-import-csv "~/projects/utils/elisp/games/cram/imports.csv")

(defun ld-columns (table-designator)
  (mapcar (compose #'third #'ld-column-identifier)
    (ld-schema-column-definitions (ld-schema table-designator))))
;;(ld-columns :problem)

;;(cram-db-insert-user "Ludvik" '(1500 350))
;;(ld-select :problem :order-by #'cram-problem-rating-e)
;;(cl-sort (ld-select :problem :columns (:question :rating)) #'> :key #'caadr)
;;(ld-select :problem :where (= :id 13))
;;(ld-select :problem :where (= :id 17))
;;(ld-select :problem :where (string-equal "Šerbedžija" :answer))
;;(ld-select :problem :where (string-match "erbed" :answer))
;;(ld-select :problem :where (string-match "havn" :answer))
;;(ld-save-database *current-database*)
;;(ld-select :problem :where (string= :answer "Bern"))
;;(ld-update :problem (= :id 17) '("Serbedzija") (:alternatives))
;;(ld-update :user (= :id 1) '((1400 350)) (:rating))
;;(setf qwe '(1400 360))
;;(ld-update :user (= :id 1) `(,qwe) (:rating))
;;(ld-update :user (= :id 1) `(,qwe) (:rating))
;;(cram-db-problems)
;;(CAUTION! ld-delete-if #'always :problem)

(provide 'cram-import)
