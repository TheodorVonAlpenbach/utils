(require 'emacsql-mysql)

;;; To debug an expression, use emacsql-compile, e.g:
;;; (emacsql-compile db [:select * :from component :where (= source-id $r1)] "617bac2c4075f937656d9d36")

(cl-indent '(emacsql-mysql emacsql) 'prog1)

(cl-defun ada-mysql-connect (&optional (db :ada))
  (destructuring-bind (db user password host port)
      (cl-find db (read* ".pwd") :key #'first)
    (emacsql-mysql (downcase (sstring db))
	     :user user :password password :host host :port port)))

(defvar db nil)

;;(unless db (setf db (ada-mysql-connect :ada_test)))
;;(setf db (ada-mysql-connect :ada))
;;(setf ada-test-db db)
;;(delete-process "emacsql-mysql<1>")

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

(provide 'ada-mysql)
