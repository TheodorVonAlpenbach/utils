;; the following function will be constructed in several steps:
;; 0 non-interactive with no members and arg A -> const A& always
;; 1 interactive
;; 2 interactive with arguments class argT argN retT
;; 3 also arguments mem1T mem1N 
;; 4a special cases for int: A->const A&, int->const int
;; 4b special cases for bool, double, char etc
;; 4c no 'return' for void
;; 5 offer name suggestions (int -> i, geometry -> g etc)
;; 6 handle constness (mem -> cons, operator)
;; 7 special formatting: compactify if possible:
;;   "r operator...\n{\n...\n} -> r operator...{...}
;; 8 more effective input methods (suggestions?)

(defconst c++-base-type-list '("int" "bool" "double" "char")
  "List of base C++ types")

(defun c++-reference-non-base-type (type)
  "Returns TYPE + '& if TYPE is not in c++-base-type-list, else TYPE" 
  (if (not (member type c++-base-type-list))
      (setq type (concat type "&"))
    (setq type (concat "const " type))))

;(c++-reference-non-base-type "A")
;(c++-reference-non-base-type "double")

;;;lbs endringer

(defun open-insert-and-indent-line (&rest line)
"Opens a new line, inserts any number of strings or characters, and
indents it."
  (newline)
  (apply #'insert line)
  (indent-according-to-mode))

(defun c++-insert-unary-function-6 (class argt argn rett memt memn)
"Inserts the class definition of unary_function object at point. CLASS
denotes the class name, ARGT the function argument template type, ARGN
the function argument template name, RETT the function return type,
MEMT the optional member type, MEMN the optional member name. If MEMT
is given, a constructor signature with empty body is also inserted."

  (interactive "\
*sclass name (foo): 
sargument type (int): 
sargument name (first letter of type): 
sreturn type (bool): 
smember type (no member): 
smember name (first letter of type): ")

  ;; set defaults
  (if (string= class "")
      (setq class "foo"))
  (if (string= argt "")
      (setq argt "int"))
  (if (string= argn "")
      (setq argn (downcase (substring argt 0 1))))
  (if (string= rett "")
      (setq rett "bool"))

  (let ((arg-template argt)
        (ret-template rett)
        (arg-type (c++-reference-non-base-type argt)))
    (open-insert-and-indent-line "struct " class " : unary_function<"
                                 arg-template ", " ret-template ">")
    (open-insert-and-indent-line "{")

    (if (not (string= memt ""))
        (let ((mem-reft (c++-reference-non-base-type memt))
              (mem-name (or (and (string= memn "")
                                 (downcase (substring memt 0 1)))
                            memn)))
          (open-insert-and-indent-line memt " " mem-name "_;")
          (open-insert-and-indent-line class "(" mem-reft " " mem-name ") : "
                                       mem-name "_(" mem-name ") {}")))
           
    (open-insert-and-indent-line rett " operator()("
                                 arg-type " " argn ") const")
    (open-insert-and-indent-line
     (concat "{"
             (and (not (string= rett "void"))
                  "return ")
             ";}"))
    (open-insert-and-indent-line "};")
    (open-line 1)
    (backward-char 8)
    class))

(define-key c++-mode-map "\C-cf" 'c++-insert-unary-function-6)

;; mb original
(defun c++-insert-unary-function-5 (class argt argn rett memt memn)
  "Inserts the class definition of unary_function object at point.
CLASS denotes the class name, ARGT the function argument template
type, ARGN the function argument template name, MEMT the optional
member type, MEMN the optional member name. If MEMT is given, a
constructor signature with empty body is also inserted.
"
  (interactive "\
*sclass name (foo): 
sargument type (int): 
sargument name (first letter of type): 
sreturn type (bool): 
smember type (no member): 
smember name (first letter of type): ")

  ;; set defaults
  (if (string= class "") (setq class "foo"))
  (if (string= argt "") (setq argt "int"))
  (if (string= argn "") (setq argn (downcase (substring argt 0 1))))
  (if (string= rett "") (setq rett "bool"))

  (let ((arg-template argt) (ret-template rett)
	(arg-type (c++-reference-non-base-type argt)))
     (insert "
struct " class " : unary_function<" arg-template ", " ret-template ">
{")

     (if (not (string= memt ""))
	 (progn 
	   (setq memt (c++-reference-non-base-type memt))
	   (if (string= memn "")
	       (setq memn (downcase (substring memt 0 1))))
	   (insert "
  " memt " " memn "_;
  " class "(" memt " " memn ") : " memn "_(" memn ") {}")))
     (insert "
  " rett " operator()(" arg-type " " argn ") const
  {")
     (if (not (string= rett "void")) (insert "return "))
     (insert ";}
};
")
     (backward-char 6)))
  
;(c++-insert-unary-function-0 "C" "T" "x" "R" "M" "m")
;(concat "a" "b")

(defun mb_test (arg) "foobar"
  (c++-insert-unary-function-0 arg arg arg arg arg arg ))
