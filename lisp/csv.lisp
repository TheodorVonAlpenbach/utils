(defpackage :csv
  (:use :cl :mb-utils)
  (:export :parse-csv :parse-csv-string :parse-csv-file
	   :write-csv :csv-string :write-csv-file))

(in-package :csv)

(defparameter *column-separator* #\,)
(defparameter *row-separator* #\Newline)

;;; read
(defun parse-csv (stream &key (column-separator *column-separator*) remove-empty-lines-p remove-empty-columns-p)
  "TODO: move to utils. Or load a common package. 
Converts CSV content from STREAM to a list of list of strings.
For info abount CSV files, see http://en.wikipedia.org/wiki/Comma-separated_values"
  (loop for line in (read-lines stream remove-empty-lines-p)
	collect (split-by-char line column-separator remove-empty-columns-p)))

(defun parse-csv-string (string &rest args)
  "Converts CSV STRING to a list of list of strings. See PARSE-CSV for ARGS."
  (with-input-from-string (s string) (apply #'parse-csv s args)))
;;(parse-csv-string (concat (list "a;b" "a;b") :in (string #\Newline)))

(defun parse-csv-file (path &rest args)
  "Converts CSV content from path to a list of list of strings. See PARSE-CSV for ARGS."
  (with-open-file (s path) (apply #'parse-csv s args)))
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
;;(write-csv-file '(("a" "b") ("a" "b")) "~/tmp/qwe.qwe.txt")

(provide 'csv)
