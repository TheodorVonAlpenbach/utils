;;;; Import questions
(cl-defun cram-import-csv (path)
  "Import cram tasks from CSV file at PATH.
See header of imports.cvs for correct format"
  (let ((rows (maptree #'(lambda (s) (if (empty-string-p s) nil s))
		(parse-csv-file path))))
    (cl-loop for row in (rest rows)
	  ;; ignore ratings / levels for now
	  for args = (rcons (butlast row) +cram-default-rating+)
	  do (unless (cram-db-insert-problem args)
	       ;; A q with same :source-id is probably present in DB.
	       ;; Try update instead
	       (ld-update :problem
		 (string= :source-id (first args))
		 (butlast (rest args))
		 (:question :answer :picture :alternatives :hints))))))
;;(cram-import-csv "~/git/utils/elisp/games/cram/norske-fugler.csv")
;;(cram-import-csv "~/git/utils/elisp/games/cram/planter.csv")
;;((cram-import-csv "~/projects/utils/elisp/games/cram/serbokroatisk.csv")
;;(ld-select :problem :where (string-match "sonne" :answer))
;;(ld-select :problem :where (string-match "toppmeis" :answer))
;;(ld-select :problem :where (string-matopptch "brush" :answer))
;;(ld-select :problem :where (string-match "vaktel" :answer))
;;(ld-save-database *current-database*)
;;(cram-import-csv "~/projects/utils/elisp/games/cram/imports.csv")

(cl-defun ld-columns (table-designator)
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

(cl-defun cram-format-question (s)
  (andcat (split-string s ", ") ", " " og " " og "))
;;(cram-format-question "Parnassia palustris")

(cl-defun cram-format-picture (s)
  (destructuring-bind (f . r) (split-string s " ")
    (concat* (cons (capitalize f) r) :in "_" :suf ".jpg")))
;;(cram-format-picture "Parnassia palustris")

(cl-defun cram-parse-planter (&optional (startindex 1)
				(buffer "plantescratch")
				(refprefix "csv-planter-"))
  (concat* (cl-loop for (k l p) in (parse-csv-string
				 (buffer-string-no-properties buffer) "\t")
		 for i from startindex
		 collect (concat*
			     (list (format "%s%d" refprefix i)
				   (format "Hvilken plante er kommuneblomst i %s"
				     (cram-format-question k))
				   p
				   (cram-format-picture l)
				   "" "" "3")
			   :in ";"))
    :in "\n"))
;;(cram-parse-planter)
;;(cl-substitute ?_ ?  "a b")

;;; Norske fugler
(cl-defun format-norsk-fugl-ancestors (ancestors)
  (concat* (cl-remove-if #'empty-string-p ancestors) :in " - "))
;;(format-norsk-fugl-ancestors (subseq qwe 0 3))

(cl-defun format-norsk-fugl (n order family subfamily genus species norsk)
  (format "csv-norske-fugler-%d;%s - %s %s;%s;norske-fugler/%s_%s.jpg;;;3"
    n
    (format-norsk-fugl-ancestors (list order family subfamily))
    genus species norsk genus species))
;;(apply #'format-norsk-fugl (cons 1 qwe))

(cl-defun format-norske-fugler (&optional (start-index 1))
  "Copy columns Order to Norsk from aves.xlsx and paste into
*scratch*. Then run this."
  (lines-to-string
   (cl-loop for l in (remove ""
		    (string-lines (buffer-string-no-properties "*scratch*")))
	 for (order family subfamily genus species norsk)
	 = (project (split-string l "\t") '(0 4 5 6 7 9))
	 for n from start-index
	 collect (format-norsk-fugl n order family subfamily genus species norsk))))
;;(insert (format-norske-fugler 164))

(provide 'cram-import)
