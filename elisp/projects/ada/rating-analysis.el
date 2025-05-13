(require 'ada-mysql)

(defun format-result-column-header (x)
  (concat* (mapcar #'sstring (last (listify x) 2)) :in "-"))
;;(format-result-column-header '(1 "qwe"))

(defun rating-simple-stats ()
  (let* ((columns [u:subject-level
		   (funcall avg ur:rating)
		   (funcall avg ur:ratings-deviation)
		   (funcall count ur:rating)])
	 (header (mapcar #'format-result-column-header
		   (cl-coerce columns 'list)))
	 (result
	  (emacsql db
	    (vector
	     :select columns
	     :from 'user-rating 'ur
	     :inner :join 'user 'u
	     :on '(= ur:user-id u:id)
	     :where '(in u:subject-level [aarstrinn5 aarstrinn6 aarstrinn7])
	     :group-by 'u:subject-level))))
    (cons header result)))
;;(tab-format (maptree #'sstring (rating-simple-stats)))
;;(tab-format (maptree #'sstring qwe))

(provide 'rating-analysis)
