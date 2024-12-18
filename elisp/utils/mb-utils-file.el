(cl-defun in-directory-p (filename directory)
  "Return not nil if FILENAME is in DIRECTORY."
  (prefix-p (expand-file-name directory)
	    (expand-file-name filename)))
;;(in-directory-p "~/bin/mbtags.sh" "~/")

(cl-defun expand-directory-name (directory-name &optional parent-directory-name)
  "Return the expansion of parent-directory-name with directory-name.
This is similiar to expand-file-name, except that the result is a
true directory name."
  (file-name-as-directory
   (expand-file-name directory-name parent-directory-name)))

(cl-defun concat-directories (directory1 directory2)
  (concat (file-name-as-directory directory1)
	  directory2))
;;(concat-directories "c:/emacs-22.1/site-lisp/mb-lisp/utils/" "qwe")

(cl-defun filename-base (path)
  "Non-extension part of Common Lisp filename component of PATH."
  (file-name-nondirectory (file-name-sans-extension path)))
;(filename-base "d/d/a.b")

(cl-defun directory-truename (directory)
  "Return a canonical form of DIRECTORY.
In this implementation the canonical form is the same as the
result of FILE-TRUENAME without any trailing slash. Example:
~/my/dirctory/ --> /home/mbe/my/directory (not .../directory/)"
  (string-trim-right* (file-truename directory) "/"))

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

(cl-defun file-name-add-extension (filename extension
				&optional (extension-separator "."))
  "Append EXTENSION to FILENAME using EXTENSION-SEPARATOR."
  (format "%s%s%s" filename extension-separator extension))
;;(file-name-add-extension "qwe/qwe" "txt")

(cl-defun file-name-change-extension (filename new-extension)
  "Return FILENAME with NEW-EXTENSION.

\(file-name-change-extension \"qwe.txt\" \"pdf\"\) ==> \"qwe.pdf\""
  (file-name-add-extension
   (file-name-sans-extension filename) new-extension))
;;(file-name-change-extension "qwe/qwe.txt" "cpp")

(cl-defun copy-libs (path1 path2)
  (let* ((apath1 (file-truename path1))
	 (paths1 (mapcar #'file-truename (unix-find path1 :name "*sc[ie]"))))
    (cl-loop for p1 in paths1
	  for relative-path = (substring p1 (length apath1))
	  for p2 = (expand-file-name relative-path path2)
	  do (when (file-newer-than-file-p p1 p2)
	       (copy-file p1 p2)))))
;;(copy-libs "~/tmp/SciLab" "~/sources/SciLab")

(cl-defun rename-files (directory pattern replacement)
  "Renames all files containing PATTERN in DIRECTORY."
  (let* ((paths (file-expand-wildcards  (expand-file-name (format "*%s*" pattern) directory))))
    (cl-loop for path in paths
	  for fn = (file-name-nondirectory path)
	  for new-fn = (replace-regexp-in-string pattern replacement fn)
	  for new-path = (expand-file-name new-fn directory)
	  do (rename-file path new-path))))
;;(rename-files "~/data/FFIAOG/MCMV/M341_Karmoy/MomPks/" "351" "341")

(cl-defun parent-directory-1 (file n)
  "Helper for `parent-directory'."
  (if (plusp n)
    (parent-directory-1
     (file-name-directory (directory-file-name file)) (1- n))
    file))
;;(parent-directory-1 "/a/b/c" 0)

(cl-defun parent-directory (file &optional (n 1))
  "Return parent directory of FILE.
File can be either a file or a directory. With optional N you can
retrive the Nth parent directory."
  (cl-assert (integerp n))
  (parent-directory-1 file n))
;;(parent-directory "/a/b/c")

(provide 'mb-utils-file)
