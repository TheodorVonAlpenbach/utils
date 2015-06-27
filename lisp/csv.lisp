(defpackage :csv
  (:use :cl :mb-utils)
  (:export :read-csv :parse-csv-string :read-csv-file
	   :parse-csv :parse-csv-file
	   :write-csv :csv-string :write-csv-file))

(in-package :csv)

(defparameter *column-separator* #\,)
(defparameter *row-separator* #\Newline)

;;; read
(defun read-csv (stream &key (column-separator *column-separator*) remove-empty-lines-p remove-empty-columns-p
			   (from-line 0) to-line (columns t))
  "TODO: move to utils. Or load a common package. 
Converts CSV content from STREAM to a list of list of strings.
For info abount CSV files, see http://en.wikipedia.org/wiki/Comma-separated_values"
  (loop for line in (read-lines stream :remove-empty-p remove-empty-lines-p
				:start from-line :end to-line)
	for all-columns = (split-by-char line column-separator remove-empty-columns-p)
	collect (if (eql columns t)
		  all-columns
		  (apply #'mnth all-columns columns))))

(defun parse-csv-string (string &rest args)
  "Converts CSV STRING to a list of list of strings. See PARSE-CSV for ARGS."
  (with-input-from-string (s string) (apply #'read-csv s args)))
;;(parse-csv-string (concat (list "a,b" "c,d") :in (string #\Newline)) :from-line 1 :columns '(0))

(defun read-csv-file (path &rest args)
  "Converts CSV content from path to a list of list of strings. See PARSE-CSV for ARGS."
  (with-open-file (s path) (apply #'read-csv s args)))

;;; deprecated functions
(defun parse-csv (&rest args)
  (warn "PARSE-CSV is deprecated. Use READ-CSV instead.")
  (apply #'read-csv args))

(defun parse-csv-file (path &rest args)
  (warn "PARSE-CSV is deprecated. Use READ-CSV-FILE instead.")
  (apply #'read-csv-file args))
;;(subseq (parse-csv-file (merge-pathnames "data/EGINA_Seastates (on-site).csv" +egina-path+)) 0 10)

;;; write
(defun write-csv (tree stream &key (column-separator *column-separator*) (row-separator *row-separator*))
  (flet ((csv-line (x) (concat x :in (string column-separator))))
    (write-list (mapcar #'csv-line tree) stream :in (string row-separator))))
;;(write-csv '(("a" "b") ("a" "b")) t)

(defun csv-string (tree &rest args)
  (with-output-to-string (s) (apply #'write-csv tree s args)))
;;(csv-string '(("a" "b") ("a" "b")))

(defun write-csv-file (tree filename &rest args)
  (with-open-file (s filename :direction :output)
    (apply #'write-csv tree s args)))
;;(write-csv-file '(("a" "b") ("a" "b")) "~/tmp/qwe/qwe.txt")

(provide 'csv)
