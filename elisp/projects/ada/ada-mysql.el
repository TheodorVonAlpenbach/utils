(require 'emacsql-mysql)
(require 'rot47)

;;; To debug an expression, use emacsql-compile, e.g:
;;; (emacsql-compile db [:select * :from component :where (= source-id $r1)] "617bac2c4075f937656d9d36")

(cl-defun column-selection (columns)
  (if columns
    (coerce (mapcar #'decolonize-symbol columns) 'vector)
    '*))
;;(mapcar #'column-selection '(nil (a :b)))

(cl-defun ada-read-pwd (&optional (db :ada))
  (cl-find db (read* "~/git/utils/elisp/projects/ada/.pwd") :key #'first))

(cl-defun ada-mysql-connect (&optional (db-tag :ada))
  (cl-destructuring-bind (db-tag user password host port)
      (ada-read-pwd db-tag)
    (awhen (emacsql-mysql (downcase (sstring db-tag))
	     :user user
	     :password (rot47-string password)
	     :host host
	     :port port
	     :debug t)
      (message "Open new connection '%s'" (ada-name it))
      (setf db it))))
;;(ada-mysql-connect)

(cl-defun ada-sstring (x)
  (unless (eql x 'NULL)
    (sstring x)))
;;(ada-sstring 'NULL)

(cl-defun ada-prefix-to-environment (prefix)
  (cl-case current-prefix-arg
    (2 :ada_test)
    (3 :ada_preprod)
    (4 :ada_prod)
    (5 :ada_dummy)
    (6 :ada_bummy)
    (7 :ada_key)
    (t :ada)))
;;(ada-prefix-to-environment t)

(cl-defun ada-copy-to-clipboard (&optional prefix)
  (interactive)
  (let ((prefix (ada-prefix-to-environment current-prefix-arg)))
    (string-to-clipboard (rot47 (third (ada-read-pwd prefix))) t)
    (message "Password for %s was copied to clipboard" (sstring prefix))))

;; prefix gh q
(cl-defun mb-mysql-map ()
  "Return a sub map for functions related to MYSQL."
  (let ((mysql-map (make-sparse-keymap)))
    (define-key mysql-map "p" #'ada-copy-to-clipboard)
    (define-key mysql-map "e" #'ada-set-environment)
    (define-key mysql-map "n" #'ada-name)
    mysql-map))

(defvar db nil)
;;(setf db nil)

(cl-defun ada-set-environment (&optional prefix)
  (interactive)
  (let ((env (ada-prefix-to-environment current-prefix-arg)))
    ;; if no exception
    (setf db (ada-mysql-connect env))))
;;(ada-set-environment)

(cl-defun ada-name (&optional (db db))
  (interactive)
  (princ
   (if db (emacsql-psql-dbname db)
       "You are currently not connected to a database")))
;;(ada-name)

;;(setf db (ada-mysql-connect :adam))
;;(setf db (setf db-test (ada-mysql-connect :ada_test)))
;;(setf db-test (ada-mysql-connect :ada_test))
;;(setf db db-test)
;;(setf ada-test-db db)
;;(setf db-preprod (ada-mysql-connect :ada_preprod))
;;(setf db db-preprod)
;;(setf db-prod (ada-mysql-connect :ada_prod))
;;(setf db db-prod)
;;(delete-process "emacsql-mysql<1>")
;;(emacsql db [:select id :from user])
;;(string-to-integer "qwe")

(cl-defun id (id-descriptor)
  (when id-descriptor
    (if (and (integerp id-descriptor)
	     (plusp id-descriptor))
      id-descriptor
      (if (listp id-descriptor)
	(id (car id-descriptor))
	(error "Argument is not an ID descriptor!")))))

(cl-defun id (id-descriptor)
  (when id-descriptor
    (cond
      ((and (integerp id-descriptor)
	    (plusp id-descriptor))
       id-descriptor)
      ((stringp id-descriptor)
       (string-to-integer id-descriptor))
      ((listp id-descriptor)
       (id (car id-descriptor)))
      (t
       (error "Argument is not an ID descriptor!")))))
;;(id (user 322181))
;;(id (list "1"))

(cl-defun json-from-id (id)
  (car (emacsql db [:select json :from json :where (= id $s1)] id)))
;;(json-from-id 194581)

(cl-defun emacsql-mysql (database &key user password host port debug)
  "Connect to a MySQL server using the mysql command line program."
  (let* ((mysql (or (executable-find emacsql-mysql-executable)
                    (error "No mysql binary available, aborting")))
         (command (list database "--skip-pager" "-rfBNL" mysql)))
    (when user     (push (format "--user=%s" user) command))
    (when password (push (format "--password='%s'" password) command))
    (when host     (push (format "--host=%s" host) command))
    (when port     (push (format "--port=%s" port) command))
    (message "%s" (reverse command))
    (let* ((process-connection-type t)
           (buffer (generate-new-buffer " *emacsql-mysql*"))
           (command (concat* (nreverse command) :in " "))
           ;; (command "/usr/bin/mysql -rfBNL --skip-pager ada --user=root --password=docker77 --host=127.0.0.1 --port=30100")
           (process (start-process-shell-command
                     "emacsql-mysql" buffer (concat "stty raw &&" command)))
           (connection (make-instance 'emacsql-mysql-connection
                                      :process process
                                      :dbname database)))
      ;; (print command)
      (setf (process-sentinel process)
            (lambda (proc _) (kill-buffer (process-buffer proc))))
      (when debug (emacsql-enable-debugging connection))
      (emacsql connection
               [:set-session (= sql-mode 'NO_BACKSLASH_ESCAPES\,ANSI_QUOTES)])
      (emacsql connection
               [:set-transaction-isolation-level :serializable])
      (emacsql-register connection))))

(cl-defmethod emacsql-parse ((connection emacsql-mysql-connection))
  (with-current-buffer (emacsql-buffer connection)
    (let ((standard-input (current-buffer)))
      (bob)
      (when (looking-at "ERROR")
        (search-forward ": ")
        (signal 'emacsql-error
                (list (buffer-substring (point) (line-end-position)))))
      (butlast
       (cl-loop for l in (string-lines (buffer-string))
	     collect (split-string l "\t"))
       5))))
;;(user-from-name "%elev_no456326499_1a_1 %" :id)

(cl-defun ada-tables (&optional regexp)
  (let ((table-names (mapcar #'first (emacsql db [:show :tables]))))
    (if regexp
      (copy-if #'(lambda (x) (string-match regexp x)) table-names)
      table-names)))
;;(ada-tables "element")

(cl-defun ada-columns (table &rest columns)
  (if columns
    (emacsql db [:show columns :from $i1 :where field :in $v2]
	     table
	     (coerce
	      (cl-loop for c in columns collect
		    (decolonize-symbol c #'(lambda (string)
					     (cl-substitute ?_ ?- string))))
	      'vector))
    (emacsql db [:show columns :from $i1] table)))
;;(ada-columns 'task)
;;(ada-columns 'user-ntp-module-codes )
;;(coerce (mapcar #'decolonize-symbol '(user-id)) 'vector)
;;(cl-substitute ?_ ?- "a-ha")

(provide 'ada-mysql)
