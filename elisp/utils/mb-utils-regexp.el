(require 'mb-utils-strings)

;; General regexps
(defconst *natural-number-regexp* "[123456789][0123456789]*")
(defconst *empty-regexp* "\\`\\`"
  "Matches absolutely nothing.")

(cl-defun regexp-or (&rest strings)
  "Returns a regexp matching any integer in INTS"
  (concat* strings :in "\\|"))
;;(regexp-or "abc" "def")

(cl-defun regexp-or-integers (&rest ints)
  "Returns a regexp matching any integer in INTS"
  (apply #'regexp-or (mapcar 'number-to-string ints)))
;;(apply #'regexp-or-integers (a-b 1 3))

(cl-defun factor-list (list test &rest args)
  "{aa ab b ba} -> {a{a b} b{nil a}}"
  (apply #'group-adjacent (apply #'sort* list test args) :test test args))
;;(factor-list '("ab" "b" "ac") #'= :key #'first-elt)

(cl-defun group-adjacent (list &key (test #'eql) (key #'identity))
  "(0 0 1 2 2 2 3 3) -> ((0 0) (1) (2 2 2) (3 3))"
  (if (null list)
    list
    (cl-loop with current = (funcall key (first list))
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
  "{aa ab b ba} -> {a{a b} b{nil a}}"
  (mapcar #'(lambda (x) (list (first-elt (first x))
			      (remove-empty-strings
			       (mapcar #'(lambda (y) (remove-nth 0 y)) x))))
   (group-adjacent (sort* strings #'string<) :test #'= :key #'first-elt)))
;;(factor-strings '("ab" "b" "ac"))
;;(factor-strings '("ee" "d1" "d3" "4d" "ef" "ag5"))

(cl-defun remove-empty-strings (strings)
  (remove* "" strings :test #'string=))

(cl-defun mb-regexp-not-strings (strings)
  "Generate a regular expression that do _not_ match any string in
STRINGS."
  (let ((factored-strings (factor-strings (remove* "" strings :test #'string=))))
    ;;    (insert (format "%s" factored-strings))
    (when factored-strings
      (concat* factored-strings
	       :pre (concat* factored-strings :pre "\\\([^" :suf "]"
			     :key #'(lambda (x) (char-to-string (first x))))
	       :suf "\\\)" 
	       :test #'(lambda (x) (and (second x) (remove-empty-strings (second x))))
	       :key #'(lambda (x) 
			(concat "\\|" (char-to-string (first x))
				(mb-regexp-not-strings (second x))))))))
;(mb-regexp-not-strings '("ee" "d1" "d3" "4d" "" "ef" "ag5"))
;;(mb-regexp-not-strings '("aB"))
(provide 'mb-utils-regexp)
