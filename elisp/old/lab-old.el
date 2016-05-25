(defun lab-api-query-to-solr-old (query-string)
  "Converts a lab-api query to an equivalent Solr expression."
  (concat* (mapply #'lab-api-filter-to-solr (lab-api-filters query-string))
	   :in " "))

(defun lab-api-filter-to-solr (lab-field lab-value)
  "Converts a lab-api FIELD and VALUE to a solr filter"
  (format "%s:%s" 
    (lab-field-api-to-solr lab-field)
    (lab-api-value-to-solr lab-value (lab-field-date-p lab-field))))

(defun lab-api-value-to-solr (lab-value date-p)
  "Converts a lab-api FIELD and VALUE to a solr filter"
  (if date-p
    (lab-api-period-to-solr lab-value)
    lab-value))
;;(mapcar* #'lab-api-value-to-solr '("qwe" "[qwe ewq]") '(nil t))

(defun* lab-api-filters (query-string &optional (regexp solr-field-regexp))
  "Retrives a list with elements on the form (FIELD VALUE) from
QUERY-STRING. Optional argument REGEXP is a regular expression
that matches a field substring"
  (let* ((pos-ivs (string-match-position-intervals query-string regexp))
	 (fields (loop for iv in pos-ivs
			    collect (apply #'substring query-string iv)))
	 (values (loop for iv in (cut (rest (flatten pos-ivs)) 2 t)
			     for s = (apply #'substring query-string iv)
			     collect (string-trim s "[ \f\t\n\r\v:]+"))))
    (mapcar* #'list fields values)))
;;(lab-api-filters lab-api-query-test) 

