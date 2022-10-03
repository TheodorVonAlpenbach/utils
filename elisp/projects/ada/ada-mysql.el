(require 'emacsql-mysql)
(require 'rot47)

;;; To debug an expression, use emacsql-compile, e.g:
;;; (emacsql-compile db [:select * :from component :where (= source-id $r1)] "617bac2c4075f937656d9d36")

(cl-indent '(emacsql-mysql emacsql) 'prog1)

(defun column-selection (columns)
  (if columns
    (coerce (mapcar #'decolonize-symbol columns) 'vector)
    '*))
;;(mapcar #'column-selection '(nil (a :b)))

(cl-defun ada-mysql-connect (&optional (db :ada))
  (destructuring-bind (db user password host port)
      (cl-find db (read* ".pwd") :key #'first)
    (emacsql-mysql (downcase (sstring db))
	     :user user :password (rot47-string password) :host host :port port)))
;;(emacsql-close db)

(defun ada-sstring (x)
  (unless (eql x 'NULL)
    (sstring x)))
;;(ada-sstring 'NULL)

(defvar db nil)

;;(setf db (ada-mysql-connect :ada))
;;(setf db (setf db-test (ada-mysql-connect :ada_test)))
;;(setf db-test (ada-mysql-connect :ada_test))
;;(setf db db-test)
;;(setf ada-test-db db)
;;(setf db-preprod (ada-mysql-connect :ada_preprod))
;;(setf db db-preprod)
;;(delete-process "emacsql-mysql<1>")
;;(emacsql db [:select id :from user])

(defun id (id-descriptor)
  (when id-descriptor
    (if (and (integerp id-descriptor)
	     (plusp id-descriptor))
      id-descriptor
      (if (listp id-descriptor)
	(id (car id-descriptor))
	(error "Argument is not an ID descriptor!")))))
;;(id (list 1))

(defun json-from-id (id)
  (car (emacsql db [:select json :from json :where (= id $s1)] id)))
;;(json-from-id 194581)

(cl-defmethod emacsql-parse ((connection emacsql-mysql-connection))
  (with-current-buffer (emacsql-buffer connection)
    (let ((standard-input (current-buffer)))
      (bob)
      (when (looking-at "ERROR")
        (search-forward ": ")
        (signal 'emacsql-error
                (list (buffer-substring (point) (line-end-position)))))
      (butlast
       (loop for l in (string-lines (buffer-string))
	     collect (split-string l "\t"))
       5))))
;;(user-from-name "%elev_no456326499_1a_1 %" :id)

(defun ada-columns (table)
  (emacsql db [:show columns :from $i1] table))
;;(ada-columns 'component)

(provide 'ada-mysql)
