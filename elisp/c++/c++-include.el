;;;; insert include directives
;; todo: 
;; 1) offer '#include <>' in addition to '#include ".h"'
;; 2) smarter placement
;; 3) take optional argument filename (or just prefix in the .h-case)

(require 'cl)
(require 'mb-utils-regexp)
(require 'mb-utils-file)
(require 'mb-utils-div)
(require 'mb-table)
(require 'mb-utils-search)

(defvar *c++-standard-include-directories* nil
  "List of paths to std headers, '<*>'")
(defvar *c++-my-include-directories* nil
  "List of paths to my std headers, '<*.h>'")
(defvar *c++-local-include-directories* nil
  "List of paths to local headers, '\"*.h\"")
;; (setf
;;  *c++-standard-include-directories* (list "e:/cygwin/usr/include/g++-3/" 
;; 					  "c:/projects/utils/mb_lib/gnu/")
;;  *c++-my-include-directories* (list *c++-mb-lib*)
;;  *c++-local-include-directories* (list "./" "../include/"))
;(nilf *c++-local-include-directories* *c++-my-include-directories* *c++-standard-include-directories*)

(defun c++-include-point () 
  "Return point where a file should be #included. This is at BOB if no
#include exists, else at BOL on line after last #include."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "^#include" nil t)
      (forward-line 1)
      (goto-char (point-min)))
    (point)))
(definteractive c++-include-point)

(defun c++-standard-header-files (dirs)
  "Returns absolute path of all c++ standard headers."
  (remove-if #'(lambda (x) (or (not (regexp-equal "[a-z]+" x))
			       (find x '("CVS" "TAGS") :test #'string=)))
	      (directory-files* dirs t)
	      :key #'file-name-nondirectory))
;(c++-standard-header-files *c++-standard-include-directories*)

(defun c++-local-header-files (dirs)
  "Returns absolute path of all c++ standard headers."
  (remove-if #'(lambda (x) (string/= (filename-extension x) ".h"))
	     (directory-files* dirs t)))
;(c++-local-header-files (list "e:/projects/mb_lib/"))

(defun* c++-header-paths ()
  (list (c++-standard-header-files *c++-standard-include-directories*)
	(c++-local-header-files *c++-my-include-directories*)
	(c++-local-header-files *c++-local-include-directories*)))
;;(c++-header-paths)

(defun c++-standard-path-2-name (path) (filename-base path))
(defun c++-local-path-2-name (path) (file-name-nondirectory path))

(defun c++-header-names (header-paths)
  (list (mapcar #'c++-standard-path-2-name (first header-paths))
	(mapcar #'c++-local-path-2-name (second header-paths))
	(mapcar #'c++-local-path-2-name (third header-paths))))
;;(mapcan #'list (c++-header-names (c++-header-paths)))

(defun c++-include-string (name local-p)
  (if local-p
    (format "#include \"%s\"" name)
    (format "#include <%s>" name)))
;;(c++-include-string "a" nil)

(defun c++-include-string (name local-p)
  (if local-p
    (format "#include \"%s\"" name)
    (format "#include <%s>" name)))
;;(c++-include-string "a" nil)

(defun c++-insert-include ()
  "Includes H-FILE at end of standard include section.
See \c++-goto-include-section-end for default header filename and
other details as well. Uses #'SAVE-EXCURSION, so if point is in part
of buffer where standard include section is not visible, the use does
not see the insertion. However a quick check that the operation was
ok, can easily be done by typing C-x C-x twice.
TODO: handle case where there are no #includes. "
  (interactive)
  (let* ((paths (c++-header-paths))
	 (names (c++-header-names paths))
	 (names-alist (mapcar #'list (apply #'append names)))
	 (name (completing-read "Include header: " names-alist))
	 (local-p (find name (third names) :test #'string=))
	 (include-string (c++-include-string name local-p)))
    (save-excursion
      (if (exists-match-p (regexp-quote include-string))
	(message* "'%s' already inserted on line %d. Nothing done." 
		  include-string (point-2-line))
	(let ((point (c++-include-point)))
	  (goto-char point)
	  (push-mark)
	  (smart-insert include-string)
	  (message* "Inserted '%s' on line %d" include-string (point-2-line)))))))
;(c++-insert-include)

(defun c++-find-include-file ()
  "Includes H-FILE at end of standard include section.
See \c++-goto-include-section-end for default header filename and
other details as well. Uses #'SAVE-EXCURSION, so if point is in part
of buffer where standard include section is not visible, the use does
not see the insertion. However a quick check that the operation was
ok, can easily be done by typing C-x C-x twice.
TODO: handle case where there are no #includes. "
  (interactive)
  (let ((h-file (c++-header-completion)))
    (find-if #'(lambda (x) (file-exists-p x))
	     (c++-absolute-std-paths h-file))))
;(c++-find-include-file)

(provide 'c++-include)
