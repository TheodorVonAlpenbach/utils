(require 'ld-general)
(require 'ld-utils)

(defconst +ld-file-extension+ ".eld")
(defconst +ld-database-metadata-filename+ "metadata.eld")

(cl-defun ld-database-directory-name-1 (db)
  (aif (ld-keyword db) 
    (keyword->filename it)
    "free-tables"))

(cl-defun ld-database-directory-name (db &optional backup)
  (format "%s%s%s"
    (if backup "." "")
    (ld-database-directory-name-1 db)
    (cl-case backup
      ((nil) "")
      (:iso-date (format "-%s" (iso-date-and-time :simple-p t)))
      (otherwise
       (if (stringp backup)
	 backup
	 (error "BACKUP must be either nil, keyword, or a string."))))))
;;(ld-database-directory-name :maths :iso-date)

(cl-defun ld-database-repository (db &optional backup)
  "TODO. free-tables should be in a constant"
  (concat-directories +ld-repository+
		      (ld-database-directory-name db backup)))
;;(cl-loop for x in (list *current-database* nil '(:dyne)) collect (ld-database-repository x :iso-date))

(cl-defun ld-database-file (db &optional backup)
  "TODO. free-tables should be in a constant"
  (concat-directories (ld-database-repository db backup)
		      +ld-database-metadata-filename+))
;;(cl-loop for x in (list *current-database* nil '(:dyne)) collect (ld-database-file x :iso-date))

(cl-defun ld-table-filename (table)
  (concat (keyword->filename (ld-keyword table)) +ld-file-extension+))
;;(ld-table-filename emps)

(cl-defun ld-table-file (table &optional backup)
  (concat-directories
   (ld-database-repository (ld-parent-id table) backup)
   (ld-table-filename table)))
;;(ld-table-file '(:milodyne :employee) :iso-date)

;;; Write
(cl-defun print-all (tree &optional (stream nil) (print-function #'pp))
  "Prints TREE without any abbrevations (like ...).
Optional PRINT-FUNCTION should be a core print function, default is PP."
  (let ((print-level nil)
	(print-length nil))
    ;; now all of TREE will be printed
    (funcall print-function tree stream)))

(cl-defun ld-save-table (table &optional backup)
  (let* ((path (ld-table-file table backup))
	 (dirpath (file-name-directory path)))
    (unless (file-exists-p dirpath)
      (make-directory dirpath t)
      (string-to-file "" path))
    (with-temp-file path
      (print-all table (current-buffer)))))
;;(ld-save-table :problem :iso-date)

(cl-defun ld-save-database (db &optional backup)
  ;; First, save metadata
  (cl-loop for table in (ld-database-tables db)
	do (ld-save-table table backup))
  (with-temp-file (ld-database-file db backup)
    (print-all (ld-clone-database db nil) (current-buffer)))
  (message "%d tables saved" (length (ld-database-tables db))))
;;(ld-save-database *current-database*)

;;; Read
(cl-defun ld-load-table (table-id)
  (let ((path (ld-table-file table-id)))
    (if (file-exists-p path)
      (with-file path
	(read (current-buffer)))
      (error "Could not load table %S" table-id))))
;;(ld-load-table '(nil :employee))

(cl-defun ld-load-database (keyword)
  (let ((path (ld-database-file (list keyword))))
    (when (file-exists-p path)
      (let* ((db (with-file path
		   (read (current-buffer))))
	     (tables (cl-loop for table in (ld-database-tables db)
			   collect (ld-load-table (ld-table-identifier table)))))
	(setf (ld-database-tables db) tables)
	db))))
;;(ld-load-database :deebee)
;;(ld-load-database :maths)

(provide 'ld-repository)
