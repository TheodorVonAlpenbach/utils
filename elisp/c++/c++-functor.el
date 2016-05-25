(defun c++-insert-generator (name rett memt memn)
  "Inserts the definition of a generator \(class\) NAME. RETT denotes
the the function return type, MEMT the optional member type, MEMN the
optional member name. If MEMT is given, a constructor signature with
empty body is also inserted."

  (interactive "\
*sclass name (foo): 
sreturn type (int): 
smember type (no member): 
smember name (first letter of type): ")

  ;; set defaults
  (when (string= name "") (setq name "foo"))
  (when (string= rett "") (setq rett "bool"))

  (smart-insert "struct " name " : generator<" rett "> {")
    
  (when (not (string= memt "")) ;;insert constructor
    (let ((mem-reft (c++-reference-non-base-type memt))
	  (mem-name (or (and (string= memn "")
			     (downcase (substring memt 0 1)))
			memn)))
      (smart-insert memt " " mem-name "_;")
      (smart-insert name "(" mem-reft " " mem-name ") : "
		    mem-name "_(" mem-name ") {}")))
           
  (smart-insert rett " operator()() {")
  (smart-insert (when (string/= rett "void") "return ") ";}")
  (smart-insert "};")
  (open-line 1)
  (backward-char 8)
  name)
;;(c++-insert-generator "foo" "int" "int" "i")

(defun c++-insert-unary-function (class argt argn rett memt memn)
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
  (when (string= class "") (setq class "foo"))
  (when (string= argt "") (setq argt "int"))
  (when (string= argn "") (setq argn (downcase (substring argt 0 1))))
  (when (string= rett "") (setq rett "bool"))

  (let ((arg-template argt)
        (ret-template rett)
        (arg-type (c++-reference-non-base-type argt)))
    (smart-insert "struct " class " : unary_function<" 
		  arg-template ", " ret-template "> {")
    
    (if (not (string= memt ""))
        (let ((mem-reft (c++-reference-non-base-type memt))
              (mem-name (or (and (string= memn "")
                                 (downcase (substring memt 0 1)))
                            memn)))
          (smart-insert memt " " mem-name "_;")
          (smart-insert class "(" mem-reft " " mem-name ") : "
			mem-name "_(" mem-name ") {}")))
           
    (smart-insert rett " operator()("
                                 arg-type " " argn ") const")
    (smart-insert
     (concat "{"
             (and (not (string= rett "void"))
                  "return ")
             ";}"))
    (smart-insert "};")
    (open-line 1)
    (backward-char 8)
    class))

(provide 'c++-functor)
