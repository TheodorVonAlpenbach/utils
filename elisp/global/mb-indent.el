;;; mb-indent.el --- Personal Lisp indentation metadata -*- lexical-binding: t; -*-

;;; Commentary:

;; Register indentation specifications used by personal macros and libraries.

;;; Code:

;;;; The main idea is to let general-lisp-indent-function be the
;;;; indent function for all lisp modes. This function checks the
;;;; buffers major mode and calls the basic indent function base on
;;;; this mode.

;;;; There are currently two basic indent functions:
;;;; lisp-indent-function (which comes with standard emacs) and
;;;; common-lisp-indent-function which is a bit more sophisticated and
;;;; handles Common Lisp

;;;; Hm, I'm not sure if this is right after all. I think
;;;; lisp-indent-function is pretty hopeless for loop forms, so you
;;;; wouldn't use lisp-indent-function at all. So how do you then
;;;; distinguish CL concat from EL concat?

;;;; Perhaps the solution is to use some kind of if statement in the
;;;; indent definition.

;;;; The function set-indent is used to put properties on the function
;;;; symbols according to major-mode. 

;; examples:
;; (progn a
;;        b)

;; (prog1 a     (prog1 a b 
;;   b	               b   
;;   c)	               c)  

;; (prog2       (prog2 a     (prog2 a b    (prog2 a b c 
;;     a            b	       c                  c     
;;     b          c	       d)                 d)    
;;   c            d)
;;   d)         

(require 'cl-lib)
(require 'cl-indent)

(setq-default lisp-indent-function #'common-lisp-indent-function)

(defun set-indent (symbol indent)
  "Set the indentation of SYMBOL to INDENT.
SYMBOL can be a symbol or a list of symbols. See function
`lisp-indent-function' for possible INDENT values."
  (let ((indent-fn 'common-lisp-indent-function))
    (if (consp symbol)
        (mapcar (lambda (item) (set-indent item indent)) symbol)
      (put symbol indent-fn
	   (if (symbolp indent)
               (get indent indent-fn)
             indent)))))

(put 'if 'lisp-indent-function 1)
(put 'if 'common-lisp-indent-function-for-elisp 1)

(defun cl-indent (symbol indent)
  "Set Common Lisp indentation for SYMBOL to INDENT."
  (set-indent symbol indent))

(cl-indent 'cl-indent 1)

(cl-indent '(with-other-window with-temp-file* unwind-protect)
  'progn)

(cl-indent '(aif anif>) 'prog1)
(cl-indent '(hif) 'if)
(cl-indent 'concat 'progn)
(cl-indent 'format 'prog1)

(cl-indent
    '(awhen read-string with-buffer with-point
      substring-intv quiz-save-excursion q-try subseq
      concatenate with-out-file length* handles-outflow
      ly-function ly-context copy-object min-elt case<
      progress-bar-init doproduct make-instance
      chrome-closest-in-cells make-array
      cartesian-product3-sum write-gnuplots write-gnuplot
      for- dbf-decode-bytes shx-position string-case
      html-stream string-match* concat* remove-duplicates
      csv-string write-csv
      gui-position-objects-horizontally
      gui-position-objects-vertically
      ld-update split-if delete cl-delete
      select-dao define-url-fn fmt if connect-toplevel
      hwhen htm-table htm-banner htm-form-button
      :export :import-from
      add-hooki product accumulate-sorted-list
      equivalence-class equivalence-class-with-key
      accumulate-list emacsql-mysql
      reduce
      emacsql-mysql emacsql
      cl-member-if-not
      tab-control-string)
  'prog1)
(cl-indent  :table :tr)

(cl-indent '(do-lines defmethod with-infile defclass with-outfile
	     read-csv string-replace-intv remove*
	     put-text-property do-tuples/o draw
	     substitute-if xml-extract-nodes
	     write-csv call-if cl-set-difference
	     db-add-user htm-button defclass
	     cl-position-if cl-member)
  'prog2)

(cl-indent '(for define-derived-mode defun-ajax)
  3)

;;;; non progX indentations
(cl-indent 'loop 'cl-loop)
(cl-indent '(string-replace-map setvector) 'dolist)
(cl-indent 'awhile 'while)
(cl-indent 'acond 'cond)
(cl-indent '(define-condition do-tuples/c defgeneric) 'defun)
(cl-indent '(cl-ppcre:register-groups-bind period-bind)
  'destructuring-bind)

(cl-indent '(find-music collaps-multi-spectrum map-spectra
	     with-transposed-tree grid-transform-data
	     format-list remove-if copy-if draw-if
	     sort cl-sort find-if-not cl-find find-if cl-find-if
	     remove cl-remove cl-position
	     cl-remove-if-not group mapcar cl-mapcar mapcan maptree copy-object-to
	     apply funcall count-if re-search-backward re-search-forward
	     format-timestring)
  1)

(cl-indent '(write-list)
  2)

(cl-indent 'substitute '(4 4 2 &body))

(cl-indent 'defclass '((&whole 4 &rest (&whole 2 &rest 1))
		       &rest (&whole 2 &rest 1)))
;;(cl-indent 'defmethod '(4 4 (&whole 4 &rest 1) &body))
(cl-indent 'generic-flet 'flet)
(cl-indent 'cl-flet 'flet)
(cl-indent 'generic-labels 'labels)
(cl-indent '(symbol-macrolet with-accessors with-slots)
	   'multiple-value-bind)
(cl-indent 'with-added-methods '((1 4 ((&whole 1))) (2 &body)))
(cl-indent 'handler-bind '((&whole 4 &rest 1) 2 &body))
(cl-indent 'handler-case '(4 &rest (&whole 2 &lambda &body)))
(cl-indent 'define-condition '((1 6)
			       (2 6 ((&whole 1)))
			       (3 4 ((&whole 1)))
			       (4 &body)))
(cl-indent 'restart-bind '(((&whole 2 (0 1) (&whole 1))) (2 &body)))

;; This seems to be wrong
(cl-indent 'restart-case '(4 &rest (&whole 2 &lambda &body)))

(cl-indent '(with-condition-restarts with-simple-restart)
	   '((1 4 ((&whole 1))) (2 &body)))

;; here comes my first "indent composition". Nice.
(cl-indent 'define-binary-type '(4 &lambda &rest (&whole 2 &lambda &body)))

;; cl-who
(cl-indent '(:a :select :html :head :title :script :option) (make-list 20 2))

(provide 'mb-indent)

;;; mb-indent.el ends here
