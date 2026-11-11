;;; ld-repository.el --- Persistence for list databases -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'pp)
(require 'ld-general)
(require 'ld-utils)

(defconst +ld-file-extension+ ".eld"
  "Filename extension for persisted list-database tables.")

(defconst +ld-database-metadata-filename+ "metadata.eld"
  "Filename containing a persisted database's metadata.")

(defun ld-database-directory-name-1 (database)
  "Return DATABASE's base repository directory name."
  (if-let ((keyword (ld-keyword database)))
      (keyword->filename keyword)
    "free-tables"))

(defun ld-database-directory-name (database &optional backup)
  "Return DATABASE's repository directory name.
When BACKUP is `:iso-date', use a hidden directory suffixed with
the current compact ISO date and time.  A string BACKUP is used
as the suffix directly."
  (concat
   (when backup ".")
   (ld-database-directory-name-1 database)
   (pcase backup
     ('nil "")
     (:iso-date
      (format "-%s" (format-time-string "%Y%m%dT%H%M")))
     ((pred stringp) backup)
     (_ (user-error "Invalid backup designator: %S" backup)))))

(defun ld-database-repository (database &optional backup)
  "Return the repository directory for DATABASE and BACKUP."
  (expand-file-name
   (ld-database-directory-name database backup)
   ld-repository-directory))

(defun ld-database-file (database &optional backup)
  "Return DATABASE's metadata filename for BACKUP."
  (expand-file-name
   +ld-database-metadata-filename+
   (ld-database-repository database backup)))

(defun ld-table-filename (table)
  "Return TABLE's persisted filename."
  (concat (keyword->filename (ld-keyword table))
          +ld-file-extension+))

(defun ld-table-file (table &optional backup)
  "Return TABLE's persisted filename for BACKUP."
  (let ((target (or (ld-table table) table)))
    (expand-file-name
     (ld-table-filename target)
     (ld-database-repository (ld-parent-id target) backup))))

(defun print-all (tree &optional stream print-function)
  "Print TREE without abbreviation to STREAM.
PRINT-FUNCTION defaults to `pp'."
  (let ((print-level nil)
        (print-length nil))
    (funcall (or print-function #'pp) tree stream)))

(defun ld-save-table (table &optional backup)
  "Persist TABLE, optionally in a BACKUP repository.
Return the written filename."
  (let ((target (ld-table table)))
    (unless target
      (user-error "Table does not exist: %S" table))
    (let ((path (ld-table-file target backup)))
      (make-directory (file-name-directory path) t)
      (with-temp-file path
        (print-all target (current-buffer)))
      path)))

(defun ld-save-database (database &optional backup)
  "Persist DATABASE and all its tables.
BACKUP selects an alternate repository directory.  Return the
metadata filename."
  (unless (ld-database-p database)
    (user-error "Not a database: %S" database))
  (let ((directory (ld-database-repository database backup))
        (metadata-file (ld-database-file database backup)))
    (make-directory directory t)
    (dolist (table (ld-database-tables database))
      (ld-save-table table backup))
    (with-temp-file metadata-file
      (print-all (ld-clone-database database nil)
                 (current-buffer)))
    (message "%d tables saved"
             (length (ld-database-tables database)))
    metadata-file))

(defun ld-repository-read (file)
  "Read and return the first Lisp object in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun ld-load-table (table-identifier)
  "Load TABLE-IDENTIFIER from the repository."
  (let ((path (ld-table-file table-identifier)))
    (unless (file-readable-p path)
      (user-error "Could not load table %S" table-identifier))
    (let ((table (ld-repository-read path)))
      (unless (ld-table-p table)
        (error "Invalid table data in %s" path))
      table)))

(defun ld-load-database (keyword)
  "Load the database identified by KEYWORD.
Return nil when no persisted database exists."
  (unless (keywordp keyword)
    (user-error "Not a database keyword: %S" keyword))
  (let ((path (ld-database-file (list keyword))))
    (when (file-readable-p path)
      (let ((database (ld-repository-read path)))
        (unless (ld-database-p database)
          (error "Invalid database metadata in %s" path))
        (setf (ld-database-tables database)
              (mapcar
               (lambda (table)
                 (ld-load-table (ld-table-identifier table)))
               (ld-database-tables database)))
        database))))

(provide 'ld-repository)

;;; ld-repository.el ends here
