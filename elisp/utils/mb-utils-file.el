(defun concat-directories (directory1 directory2)
  (concat (file-name-as-directory directory1)
	  directory2))
;;(concat-directories "c:/emacs-22.1/site-lisp/mb-lisp/utils/" "qwe")

(defun filename-base (path)
  "Non-extension part of Common Lisp filename component of PATH."
  (file-name-nondirectory (file-name-sans-extension path)))
;(filename-base "d/d/a.b")

(cl-defun find-filename (file &optional (dirs (list "./")))
  "Checks if FILE exists as readable in one of DIRS. If so it is
returned. DIRS is sorted according to priority."
  (find-if #'file-readable-p (mapcar #'(lambda (x) (concat x file)) dirs)))
;;(find-filename "TAGS" '("../"))
;;(file-readable-p "./mb-files.l")

(cl-defun find-filenames (files &optional (dirs (list ".")))
  "Find-file first available of FILES possibly in one of DIRS. Both
arguments are lists and their elements are sorted according to
priority."
  (mapcar #'(lambda (x) (find-filename x dirs)) files))
;;(find-filenames '("TAGS") '("../"))

(cl-defun find-file* (files &optional (dirs '("./")))
  "See 'find-filenames' for arguments"
  (when (not (consp files)) (setq files (list files)))
  (awhen (first (find-filenames files dirs))
    (find-file it)))
;;(find-file* "TAGS" '("../"))

(cl-defun directory-files* (dirs &rest args)
  "Returns the files content of a list of dictionaries. See
`directory-files' for ARGS."
  (mapcan #'(lambda (x) (apply #'directory-files x args)) dirs))
;(directory-files* *c++-standard-header-directories* t)

(cl-defun file-name-alter (filename new-filename &optional (inherit-extension t) (inherit-directory t))
  "Returns a new filename based on FILENAME and NEW-FILENAME.
By default the returned name is <directory of
filename><new-filename>.<filename-extension>. If, however,
inherit-extension is nil the '.<filename-extension>' part will
not be added like shown above. And if inherit-directory is nil
the <directory of filename> part will not be added either."
  (concat (if inherit-directory (or (file-name-directory filename) "") "")
	  new-filename
	  (if inherit-extension (aif (file-name-extension filename) (concat "." it) "") "")))

(cl-defun file-name-append (filename suffix &optional (inherit-extension t))
  "Returns a new filename based on FILENAME and SUFFIX.
By default the returned name is
<filename><suffix>.<filename-extension>. For inherit-extension
see `file-name-alter'"
  (file-name-alter filename (concat (file-name-sans-extension filename) suffix) inherit-extension nil))
;;(file-name-append "c:/Documents and Settings/matsb/My Documents/projects/UiO/Var-2012/2270-Satslaere2B/obligatorisk2.midi" "-100")

(provide 'mb-utils-file)
