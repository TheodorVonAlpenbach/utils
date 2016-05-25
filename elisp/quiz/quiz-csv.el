;;; Simple conversion to list format and then to csv
;;; TODO: finish auto_open macro in C:\projects\qp\spm\EmacsImportTemplate.xls

(defconst quiz-csv-directory "c:/Documents and Settings/matsb/My Documents/projects/kviss/qp/vg/spm/")
(defconst quiz-excel-template "c:/projects/qp/spm/EmacsImportTemplate.xls")

(defun quiz-insert-current-buffer-into-excel ()
  (interactive)
  (quiz-insert-into-excel (buffer-name (current-buffer)) t))
;;(quiz-insert-into-excel)

(defun quiz-insert-into-excel (buffer &optional remove-source)
  ""
  (quiz-save-as-csv buffer remove-source)
  ;p(w32-shell-execute "open" quiz-excel-template)
)
;;(quiz-insert-into-excel "mq-2006-08-16.qz" t)

(defun quiz-csv-path (quiz-buffer-string)
  ""
  (concat quiz-csv-directory (file-name-sans-extension quiz-buffer-string) ".csv"))
;;(quiz-csv-path "mq-2006-08-16.qz")

(defun quiz-save-as-csv (buffer &optional remove-source)
  ""

  (string-to-file (quiz-convert-to-csv buffer remove-source)
		  (quiz-csv-path buffer)))
;;(quiz-save-as-csv "mq-2006-08-16.qz" t)

(defun quiz-convert-to-csv (buffer &optional remove-source)
  ""
  (concat* (loop for x in (quiz-parse-list buffer)
		 when remove-source do (setq x (butlast x))
		 collect (concat* x :in ";"))
	   :in "\n"))
;;(quiz-convert-to-csv "mq-2006-08-16.qz" t)

(defun quiz-parse-list (buffer)
  "Converts q-list in BUFFER to a list of list of q-item-strings."
  (mapcar* #'quiz-parse-q (quiz-collect-qs buffer)))
;;(length (quiz-parse-list "mq-2006-08-16.qz"))
;;(print (quiz-parse-list "mq-2006-08-16.qz"))

(defun quiz-collect-qs (buffer)
  "Returns a list of q-strings"
  (split-string 
   (with-buffer buffer
     (buffer-substring-no-properties (point-min) (point-max)))
   "\n\n"))
;;(quiz-collect-qs "mq-2006-08-16.qz")

(defun quiz-trim-q-item-string (q-item-string)
  "Trims whitespace at both ends of Q-ITEM-STRING. Also removes
abundant internal whitespace."
  (string-replace (string-trim q-item-string) "\\s-+" " "))

(defun quiz-parse-q (q-string)
  (mapcar* #'quiz-trim-q-item-string (split-string q-string "[QACS][0-9]+: ")))
;;(quiz-parse-q (fourth (quiz-collect-qs "mq-2006-08-15.qz")))
;;(fourth (quiz-collect-qs "mq-2006-08-16.qz"))

(provide 'quiz-csv)
