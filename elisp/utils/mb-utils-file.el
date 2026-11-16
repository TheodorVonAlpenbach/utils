;;; mb-utils-file.el --- File and directory helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers for locating, transforming, copying, and renaming files and
;; directories.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun in-directory-p (filename directory)
  "Return non-nil when FILENAME is inside DIRECTORY."
  (file-in-directory-p (expand-file-name filename)
                       (expand-file-name directory)))

(cl-defun expand-directory-name (directory-name &optional parent-directory-name)
  "Expand DIRECTORY-NAME relative to PARENT-DIRECTORY-NAME.
Return the result in directory form, with a trailing directory separator."
  (file-name-as-directory
   (expand-file-name directory-name parent-directory-name)))

(defun filename-base (path)
  "Non-extension part of Common Lisp filename component of PATH."
  (file-name-nondirectory (file-name-sans-extension path)))

(defun directory-truename (directory)
  "Return the canonical name of DIRECTORY without a trailing separator."
  (directory-file-name (file-truename directory)))

(cl-defun find-filename (file &optional (dirs (list "./")))
  "Return the first readable occurrence of FILE in DIRS.
Search DIRS in order and return an absolute file name, or nil when FILE
cannot be found."
  (locate-file file dirs '("") #'file-readable-p))

(cl-defun find-filenames (files &optional (dirs (list ".")))
  "Find each file in FILES in the first matching directory in DIRS.
Return one absolute file name or nil for each member of FILES, preserving
the order of FILES."
  (mapcar (lambda (file) (find-filename file dirs)) files))

(cl-defun find-file* (files &optional (dirs '("./")))
  "Visit the first readable member of FILES found in DIRS.
FILES may be one file name or a list ordered by preference.  Return the
visited buffer, or nil when no candidate is readable."
  (let* ((files (if (listp files) files (list files)))
         (filename (cl-find-if #'identity (find-filenames files dirs))))
    (when filename
      (find-file filename))))

(defun directory-files* (directories &rest args)
  "Return concatenated `directory-files' results for DIRECTORIES.
Pass ARGS unchanged to `directory-files' for each directory."
  (mapcan (lambda (directory)
            (apply #'directory-files directory args))
          directories))

(cl-defun file-name-alter
    (filename new-filename
              &optional (inherit-extension t) (inherit-directory t))
  "Return a file name based on FILENAME and NEW-FILENAME.
Preserve FILENAME's extension when INHERIT-EXTENSION is non-nil and its
directory when INHERIT-DIRECTORY is non-nil."
  (concat (if inherit-directory (or (file-name-directory filename) "") "")
	  new-filename
	  (if inherit-extension
              (let ((extension (file-name-extension filename)))
                (if extension (concat "." extension) ""))
            "")))

(cl-defun file-name-append (filename suffix &optional (inherit-extension t))
  "Insert SUFFIX before FILENAME's extension.
Preserve the extension unless INHERIT-EXTENSION is nil."
  (file-name-alter
   filename
   (concat (file-name-sans-extension filename) suffix)
   inherit-extension nil))

(defun file-name-change-extension (filename new-extension)
  "Return FILENAME with NEW-EXTENSION.

\(file-name-change-extension \"qwe.txt\" \"pdf\"\) ==> \"qwe.pdf\""
  (file-name-with-extension filename new-extension))

(defun rename-files (directory pattern replacement)
  "Rename regular files in DIRECTORY by replacing literal PATTERN.
Replace every occurrence of PATTERN in each base name with REPLACEMENT.
Signal an error rather than overwrite an existing destination."
  (when (string-empty-p pattern)
    (error "PATTERN must not be empty"))
  (dolist (path (directory-files directory t directory-files-no-dot-files-regexp))
    (let ((filename (file-name-nondirectory path)))
      (when (and (file-regular-p path)
                 (string-search pattern filename))
        (rename-file
         path
         (expand-file-name
          (string-replace pattern replacement filename)
          directory))))))

(cl-defun parent-directory (file &optional (n 1))
  "Return the Nth parent directory of FILE.
FILE may name either a file or a directory.  N defaults to one and must
be a non-negative integer.  Stay at the filesystem root when N exceeds
the available number of parents."
  (unless (and (integerp n) (>= n 0))
    (error "Parent count must be a non-negative integer: %S" n))
  (dotimes (_ n file)
    (setq file (or (file-name-parent-directory file) file))))

(provide 'mb-utils-file)

;;; mb-utils-file.el ends here
