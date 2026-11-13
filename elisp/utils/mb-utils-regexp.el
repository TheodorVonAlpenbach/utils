;;; mb-utils-regexp.el --- Regular-expression utility functions -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'rx)
(require 'subr-x)

;;; Code:

;; General regexps
(defconst *natural-number-regexp* "[123456789][0123456789]*"
  "Regexp matching a positive decimal integer without leading zeroes.")

(cl-defun regexp-or (&rest strings)
  "Return a regexp matching any raw regexp in STRINGS."
  (if strings
      (concat "\\(?:" (string-join strings "\\|") "\\)")
    regexp-unmatchable))
;;(regexp-or "abc" "def")

(cl-defun regexp-or-integers (&rest ints)
  "Return an optimized regexp matching any integer in INTS literally."
  (dolist (integer ints)
    (cl-check-type integer integer))
  (regexp-opt (mapcar #'number-to-string ints)))
;;(apply #'regexp-or-integers (a-b 1 3))

(cl-defun factor-list (list less-than &key (key #'identity))
  "Sort LIST with LESS-THAN and group elements with equivalent keys."
  (group-adjacent
   (cl-sort (copy-sequence list) less-than :key key)
   :test (lambda (left right)
	   (not (or (funcall less-than left right)
		    (funcall less-than right left))))
   :key key))
;;(factor-list '("ab" "b" "ac") #'string< :key (lambda (s) (aref s 0)))

(cl-defun group-adjacent (list &key (test #'eql) (key #'identity))
  "Group adjacent elements of LIST with equal keys.
TEST compares values produced by KEY.  Group and element order are
preserved."
  (if (null list)
      list
    (cl-loop with current = (funcall key (cl-first list))
	  for elt in list
	  if (funcall test (funcall key elt) current)
	  collect elt into acc
	  else
	  collect acc into res and
	  do (setq acc (list elt)) and
	  do (setq current (funcall key elt))
	  finally return (nconc res (list acc)))))
;;(group-adjacent '("ab" "ac" "b") :key #'first-elt :test #'=)
;;(group-adjacent '("") :key #'first-elt :test #'=)

(cl-defun factor-strings (strings)
  "Group nonempty STRINGS by first character and return their suffixes.
Empty strings and empty suffixes are omitted."
  (mapcar (lambda (group)
	    (list (aref (cl-first group) 0)
		  (remove-empty-strings
		   (mapcar (lambda (string) (substring string 1)) group))))
	  (group-adjacent
	   (cl-sort (cl-remove-if #'string-empty-p strings) #'string<)
	   :test #'= :key (lambda (string) (aref string 0)))))
;;(factor-strings '("ab" "b" "ac"))
;;(factor-strings '("ee" "d1" "d3" "4d" "ef" "ag5"))

(cl-defun remove-empty-strings (strings)
  "Return STRINGS without empty string elements."
  (cl-remove-if #'string-empty-p strings))

(defconst mb-regexp--any-character "\\(?:.\\|\n\\)")

(defun mb-regexp--string-trie (strings)
  "Return a trie containing STRINGS."
  (let ((root (make-hash-table :test #'eql)))
    (dolist (string strings root)
      (let ((node root))
	(dotimes (index (length string))
	  (let* ((character (aref string index))
		 (child (gethash character node)))
	    (unless child
	      (setq child (make-hash-table :test #'eql))
	      (puthash character child node))
	    (setq node child)))
	(puthash :terminal t node)))))

(defun mb-regexp--join-alternatives (alternatives)
  "Return a regexp matching any regexp in ALTERNATIVES."
  (pcase alternatives
    (`() regexp-unmatchable)
    (`(,only) only)
    (_ (concat "\\(?:" (string-join alternatives "\\|") "\\)"))))

(defun mb-regexp--trie-complement (node)
  "Return a regexp matching strings not represented below trie NODE."
  (let (children)
    (maphash (lambda (character child)
	       (when (integerp character)
		 (push (cons character child) children)))
	     node)
    (setq children (cl-sort children #'< :key #'car))
    (if (null children)
	(if (gethash :terminal node)
	    (concat mb-regexp--any-character "+")
	  (concat mb-regexp--any-character "*"))
      (let* ((characters (apply #'string (mapcar #'car children)))
	     (alternatives
	      (list (concat (rx-to-string `(not (any ,characters)) t)
			    mb-regexp--any-character "*"))))
	(unless (gethash :terminal node)
	  (push "" alternatives))
	(dolist (child children)
	  (push (concat (regexp-quote (char-to-string (car child)))
			(mb-regexp--trie-complement (cdr child)))
		alternatives))
	(mb-regexp--join-alternatives (nreverse alternatives))))))

(cl-defun mb-regexp-not-strings (strings)
  "Return an anchored regexp matching every string except STRINGS."
  (cl-check-type strings list)
  (dolist (string strings)
    (cl-check-type string string))
  (concat "\\`"
	  (mb-regexp--trie-complement
	   (mb-regexp--string-trie (cl-remove-duplicates strings :test #'equal)))
	  "\\'"))
;(mb-regexp-not-strings '("ee" "d1" "d3" "4d" "" "ef" "ag5"))
;;(mb-regexp-not-strings '("aB"))

(provide 'mb-utils-regexp)

;;; mb-utils-regexp.el ends here
