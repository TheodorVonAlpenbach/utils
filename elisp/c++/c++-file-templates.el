;;;; insert cpp-file template

(defvar c++-file-headersec-template-big "\
/*****************************************************************************/
/*                                                                           */
/* (c) Copyright 2002 by                                                     */
/*     Contango, Smestad, Norway                                             */
/*     All rights reserved. See the FiCopyright.h for more details.          */
/*                                                                           */
/*****************************************************************************/
#include <FiCopyright.h>"
  "Template string for the header section of a h/cpp-file.")

(defvar c++-file-headersec-template ""
  "Template string for the header section of a h/cpp-file.")
;;(setq c++-file-headersec-template "")

(defvar c++-h-file-footersec-template ""
  "Template string for the footer section of a h-file.")

(defvar c++-c-file-footersec-template ""
  "Template string for the header section of a cpp-file.")

(defun c++-insert-default-cons (class) ""
  (smart-insert class "() {}"))

(defun c++-insert-class-def (class)
  "Inserts an empty class definition"
  (interactive "*sclass name: ")
  (smart-insert "class " class " {" )
  (smart-insert "public:")
  (c++-insert-default-cons class)
  (smart-insert "\nprivate:")
  (smart-insert "};"))

(defun file-name-simple (file) ""
  (file-name-nondirectory (file-name-sans-extension file)))
; (file-name-simple "c:/a/b/c")

(defun c++-insert-h-file-template (arg)
  "Inserts minimal body of CLASS in h-file
together with standard header and footer h-file sections."
  (interactive "*P")
  (let* ((default-class (file-name-simple (buffer-name)))
	 (class (if arg
		  (read-from-minibuffer "class name: " default-class) 
		  default-class))
	 (_CLASS_H_ (concat "_" (upcase class) "_H")))
    (smart-insert (concat "#ifndef " _CLASS_H_))
    (smart-insert (concat "#define " _CLASS_H_))
    ;;(smart-insert c++-file-headersec-template)
    (smart-insert)
    (when arg (c++-insert-class-def class))
    (smart-insert)
    (insert c++-c-file-footersec-template)
    (smart-insert (concat "#endif //" _CLASS_H_))
    (previous-line 3)))

(defun c++-insert-c-file-template ()
  "Inserts standard header and footer sections of a cpp-file.
Also assumes that the prefix of the name of this file is the same as
for the corresponding h-file, and #includes the latter."
  (interactive)
  (smart-insert c++-file-headersec-template)
  (c++-insert-include (file-name-simple (buffer-name)))
  (smart-insert)
  (save-excursion
    (end-of-line) (open-line 4) (next-line 2)
    (next-line 2)
    (insert c++-c-file-footersec-template)))

(defun c++-insert-get-method (method type member)
  "Inserts standard get method at point."
  (interactive "*\
sname of get method: (foo)
sreturn type (int): 
sname of member to return (without standard formatting): 
")
  (if (string= method "") (setq method "foo"))
  (if (string= type "") (setq type "int"))
  (if (string= member "")
      (setq member (c++-method-2-member-variable method)))
  (setq type (c++-reference-non-base-type type))
  (smart-insert "const " type " " method "() const {return " member ";}"))

(defun c++-insert-set-method (method type member arg)
  "Inserts standard ste method at point."
  (interactive "*\
score name of get method (fooBar) 
stype of member to set (int): 
sname of member to set (standard formatting): 
sname of argument (standard formatting): 
")
  (if (string= method "") (setq method "foo"))
  (if (string= type "") (setq type "int"))
  (if (string= member "")
      (setq member (c++-method-2-member-variable method)))
  (if (string= arg "")
      (setq arg (c++-method-2-variable method)))
  (setq type (c++-reference-non-base-type type))
  (smart-insert "void set" (capitalize method) "(" type " " arg
		") {" member " = " arg ";}"))

;(c++-insert-get-method "a" "" "")
;(cancel-debug-on-entry 'c++-insert-get-method)

(provide 'c++-file-templates)
