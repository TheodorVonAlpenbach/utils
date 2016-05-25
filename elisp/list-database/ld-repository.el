(require 'ld-general)
(require 'ld-utils)

(defconst +ld-file-extension+ ".eld")
(defconst +ld-database-metadata-filename "metadata.eld")

(defun ld-database-directory-name (db)
  (aif (ld-keyword db) 
    (keyword->filename it)
    "free-tables"))
;;(ld-database-directory-name :maths)

(defun ld-database-repository (db)
  "TODO. free-tables should be in a constant"
  (concat-directories +ld-repository+
		      (ld-database-directory-name db)))
;;(mapcar #'ld-database-repository (list *current-database* nil '(:dyne)))

(defun ld-database-file (db)
  "TODO. free-tables should be in a constant"
  (concat-directories (ld-database-repository db)
		      +ld-database-metadata-filename))
;;(mapcar #'ld-database-file (list *current-DB* nil '(:dyne)))

(defun ld-table-filename (table)
  (concat (keyword->filename (ld-keyword table)) +ld-file-extension+))
;;(ld-table-filename emps)

(defun ld-table-file (table)
  (concat-directories (ld-database-repository (ld-parent-id table))
		      (ld-table-filename table)))
;;(mapcar #'ld-table-file (list '(:milodyne :employee) emps))

;;; Write
(cl-defun print-all (tree &optional (stream nil) (print-function #'pp))
  "Prints TREE without any abbrevations (like ...).
Optional PRINT-FUNCTION should be a core print function, default is PP."
  (let ((print-level nil)
	(print-length nil))
    ;; now all of TREE will be printed
    (funcall print-function tree stream)))

(defun ld-save-table (table)
  (let* ((path (ld-table-file table))
	 (dirpath (file-name-directory path)))
    (unless (file-exists-p dirpath)
      (make-directory dirpath t))
    (with-temp-file path
      (print-all table (current-buffer)))))
;;(ld-save-table emps)

(defun ld-save-database (db)
  ;; first, save metadata
  (loop for table in (ld-database-tables db)
	do (ld-save-table table))
  (with-temp-file (ld-database-file db)
    (print-all (ld-clone-database db nil) (current-buffer)))
  (message "%d tables saved" (length (ld-database-tables db))))
;;(ld-save-database *current-database*)

;;; Read
(defun ld-load-table (table-id)
  (let ((path (ld-table-file table-id)))
    (if (file-exists-p path)
      (with-file path
	(read (current-buffer)))
      (error "Could not load table %S" table-id))))
;;(ld-load-table '(nil :employee))

(defun ld-load-database (keyword)
  (let ((path (ld-database-file (list keyword))))
    (when (file-exists-p path)
      (let* ((db (with-file path
		   (read (current-buffer))))
	     (tables (loop for table in (ld-database-tables db)
			   collect (ld-load-table (ld-table-identifier table)))))
	(setf (ld-database-tables db) tables)
	db))))
;;(ld-load-database :deebee)
;;(ld-load-database :maths)

(provide 'ld-repository)
