;;; mb-anaphora.el --- Anaphoric control macros -*- lexical-binding: t; -*-

(require 'cl-lib)

(defmacro aif (test-form then-form &rest else-forms)
  "Evaluate TEST-FORM and bind its value to `it' in the selected branch."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(def-edebug-spec aif t)

(cl-defmacro awhen (test-form &body body)
  "Evaluate BODY with `it' bound to the non-nil value of TEST-FORM."
  `(aif ,test-form
       (progn ,@body)))

(def-edebug-spec awhen t)

(defmacro acond (&rest clauses)
  "Evaluate CLAUSES like `cond', binding each successful test value to `it'."
  (when clauses
    (let ((clause (car clauses))
          (result (cl-gensym "result")))
      `(let ((,result ,(car clause)))
         (if ,result
             (let ((it ,result))
               ,@(cdr clause))
           (acond ,@(cdr clauses)))))))

(def-edebug-spec acond 'cond)

(defmacro asetf (place expression)
  "Set PLACE to EXPRESSION with `it' bound to PLACE's previous value."
  `(let ((it ,place))
     (setf ,place ,expression)))

(provide 'mb-anaphora)

;;; mb-anaphora.el ends here
