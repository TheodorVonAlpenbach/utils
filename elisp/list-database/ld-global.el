;;; ld-global.el --- Shared list-database configuration and state -*- lexical-binding: t; -*-

(require 'ld-database)

(defgroup list-database nil
  "Databases represented by Emacs Lisp lists."
  :group 'applications)

(defun ld-default-repository-directory ()
  "Return the default directory for persisted list databases.

Use the historical `*local-data-dir*' base when it is available.
Otherwise place the repository below `user-emacs-directory'."
  (expand-file-name
   "ld"
   (if (boundp '*local-data-dir*)
       (symbol-value '*local-data-dir*)
     user-emacs-directory)))

(defvaralias '+ld-repository+ 'ld-repository-directory)

(defcustom ld-repository-directory
  (ld-default-repository-directory)
  "Directory containing persisted list databases."
  :type 'directory
  :group 'list-database)

(defvar *current-database*
  (ld-create-database :empty-db :name "An empty placeholder database")
  "Database used by operations whose database argument is omitted.")

(defcustom *ld-check-data-p* t
  "Whether insertion operations validate data against their schema."
  :type 'boolean
  :group 'list-database)

(defcustom *ld-autogenerate-base* 1
  "First automatically generated numeric value for an empty table."
  :type 'integer
  :group 'list-database)

(defcustom *ld-autogenerate-increment* 1
  "Increment used for automatically generated numeric values."
  :type 'integer
  :group 'list-database)

(provide 'ld-global)

;;; ld-global.el ends here
