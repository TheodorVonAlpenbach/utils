(provide 'c++-method)

(defvar c++-method-signature-prefix
  "//-----------------------------------------------------------------------------" 
  "see c++-insert-method")

;(setq c++-method-signature-suffix "//-----------------------------------------------------------------------------")

(defvar c++-method-signature-suffix c++-method-signature-prefix
 "see c++-insert-method") 

(defun c++-insert-method (name rett scope)

"Inserts simple c++ method template: <hp>RETT SCOPE::NAME()<hs>{}
If corresponding h-file exists, all arguments are optional (if then
not given, 'c++-insert-method uses signature of corresponding
h-file). Else defaulting is as follows: NAME = foo, RETT = void, SCOPE
= scope of previous method in buffer if such exists, else global.
Before and after signature variables 'c++-method-signature-prefix and
'c++-method-signature-suffix are inserted respectively.

This version does not default due to corresponding h-file.
"

  (interactive "\
*smethod name (foo): 
sreturn type (void): 
sscope (global): ")			; more intelligent later

  ;; set defaults
  (if (string= name "")
      (setq name "foo"))
  (if (string= rett "")
      (setq rett "void"))

  (smart-insert c++-method-signature-prefix)
  (smart-insert rett)
  (smart-insert scope name "()")
  (smart-insert c++-method-signature-suffix)
  (insert "{\n")
  (smart-insert "}"))

;(debug-on-entry 'c++-insert-method)
;(c++-insert-method "" "" "")

