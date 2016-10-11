(cl-defun xlsx->cvs (filename &key (id 1))
  (call-process* "xlsx2csv" filename
		 "-s" (number-to-string id)))
;;(xlsx->cvs "/home/eier/projects/aves/aves.xlsx" :id 2)

(cl-defun xlsx->tree (filename &rest args)
  (parse-csv-string
   (apply #'xlsx->cvs filename args)
   ;; note line-separator is the windows sep ^M\n
   "," (string 13 10)))
;;(xlsx->tree "/home/eier/projects/aves/aves.xlsx" :id 2)

(defconst +aves+ (xlsx->tree "/home/eier/projects/aves/aves.xlsx"))

(defun aves-init ()
  (setf ))

(defun draw-bird ()
  )
