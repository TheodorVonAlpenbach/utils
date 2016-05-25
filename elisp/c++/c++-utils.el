(provide 'c++-utils)
(require 'mb-utils-div)

(defconst c++-base-type-list '("int" "bool" "double" "char")
  "List of base C++ types")

(defun push-front (elt lst)
  "Returns a list with ELT prepended to LST"
  (cons elt lst))

(defun mb-fill-list (elt size)
  "Returns a list with SIZE ELTs: '(ELT ELT ... ELT)"
  (let ((lst))
    (dotimes (i size lst)
      (setq lst (cons elt lst)))))
;(mb-fill-list 'a 10)

(defun mb-insert-between (lst elt)
  "Inserts ELT between each pair of consequtive elements of LST
Example: (mb-insert-between '(a b c) d) -> (a d b d c)" 
  (rest (mapcan 'list (mb-fill-list elt (length lst)) lst)))

;(mapcar* 'list '(a b c) '(_ _ _))
;(mapcan 'list '(a b c) '(_ _ _))
;(apply 'list (mapcar* 'list '(a b c) '(_ _ _)))
;(mb-insert-between '("a" "b" "c") "_")

(defun c++-compose-variable-string (words)
  "Converts list of words to standard variable format.
Example: '(\"foo\" \"bar\" \"quux\") -> foo_bar_quux" 
  (setq words (mb-insert-between words "_"))
  (downcase (apply 'concat words)))
;(c++-compose-variable-string '("foo" "bar" "Quux"))

(defun c++-split-method-string (method)
  "Split a standard method string into a word list
Example: fooBarQuux -> '(\"foo\" \"bar\" \"quux\")" 
  (save-excursion
    (setq case-fold-search nil)
    (let ((lst) (index 0))
      (while (string-match "[a-z]+" method index)
	(insert (substring method index (match-beginning 0)))
	(setq index (match-beginning 0))))))

(defun c++-split-method-string (method) ""
  (let ((lst nil) ( index 0))
    (setq case-fold-search nil)
    (while (string-match "[a-z]+" method index)
      (setq lst (cons (substring method index (match-end 0))
		      lst))
      (setq index (match-end 0)))
    (setq lst (cons (substring method index) lst))
    (if (string= "" (first lst)) (setq lst (rest lst)))
    (reverse lst)))

;(c++-split-method-string "fooBarQuuxXoeitr")
;(debug-on-entry 'c++-split-method-string)
;(cancel-debug-on-entry 'c++-split-method-string)
;	(setq lst (cons (substring method index (match-beginning))
;			lst))
;	(setq index (match-beginning))))))
;(setq case-fold-search nil)
;(string-match "[a-z]+" "foBaarZooooX" 0)
;(substring "foBaarZooooX" 11 (match-end 0))
;(c++-method-2-variable "foB")
;(c++-method-2-member-variable "foB")

(defun c++-method-2-variable (method)
  "Converts standard method format to standard variable format.
Example: fooBarQuux -> foo_bar_quux" 
  (c++-compose-variable-string (c++-split-method-string method)))

(defun c++-method-2-member-variable (method)
  "Converts standard method format to standard member variable format.
Example: fooBarQuux -> foo_bar_quux_" 
  (concat (c++-method-2-variable method) "_"))

(defun c++-reference-non-base-type (type)
  "Returns TYPE + '& if TYPE is not in c++-base-type-list, else TYPE" 
  (if (not (member type c++-base-type-list))
      (concat type "&")
    type))
