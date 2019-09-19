(require 'ld-general)
(require 'ld-utils)

(defconst +ld-file-extension+ ".eld")
(defconst +ld-database-metadata-filename+ "metadata.eld")

(defun ld-database-directory-name-1 (db)
  (aif (ld-keyword db) 
    (keyword->filename it)
    "free-tables"))

(defun ld-database-directory-name (db &optional backup)
  (format "%s%s%s"
    (if backup "." "")
    (ld-database-directory-name-1 db)
    (case backup
      ((nil) "")
      (:iso-date (format "-%s" (iso-date-and-time :simple-p t)))
      (otherwise
       (if (stringp backup)
	 backup
	 (error "BACKUP must be either nil, keyword, or a string."))))))
;;(ld-database-directory-name :maths :iso-date)

(defun ld-database-repository (db &optional backup)
  "TODO. free-tables should be in a constant"
  (concat-directories +ld-repository+
		      (ld-database-directory-name db backup)))
;;(mapcar #'ld-database-repository (list *current-database* nil '(:dyne)))

(defun ld-database-file (db &optional backup)
  "TODO. free-tables should be in a constant"
  (concat-directories (ld-database-repository db backup)
		      +ld-database-metadata-filename+))
;;(mapcar #'ld-database-file (list *current-DB* nil '(:dyne)))

(defun ld-table-filename (table)
  (concat (keyword->filename (ld-keyword table)) +ld-file-extension+))
;;(ld-table-filename emps)

(defun ld-table-file (table &optional backup)
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

(defun ld-save-table (table &optional backup)
  (let* ((path (ld-table-file table backup))
	 (dirpath (file-name-directory path)))
    (unless (file-exists-p dirpath)
      (make-directory dirpath t)
      (string-to-file "" path))
    (with-temp-file path
      (print-all table (current-buffer)))))
;;(ld-save-table emps)

(defun ld-save-database (db &optional backup)
  ;; First, save metadata
  (loop for table in (ld-database-tables db)
	do (ld-save-table table backup))
  (with-temp-file (ld-database-file db backup)
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
