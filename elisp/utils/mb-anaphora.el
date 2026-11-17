;;; mb-anaphora.el --- Anaphoric control macros -*- lexical-binding: t; -*-

;;; Commentary:

;; Conditional and generalized-variable macros that expose an intermediate
;; value through the intentionally anaphoric binding `it'.

;;; Code:

(require 'cl-lib)

(defmacro aif (test-form then-form &rest else-forms)
  "Evaluate TEST-FORM and bind its value to `it'.
Evaluate THEN-FORM when the value is non-nil; otherwise evaluate
ELSE-FORMS as an implicit `progn'."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(def-edebug-spec aif t)

(cl-defmacro awhen (test-form &body body)
  "Evaluate BODY with `it' bound to the non-nil value of TEST-FORM."
  `(aif ,test-form
       (progn ,@body)))

(def-edebug-spec awhen t)

(defmacro acond (&rest clauses)
  "Evaluate CLAUSES like `cond', binding a successful test value to `it'.
A clause without body forms returns its test value."
  (when clauses
    (let ((clause (car clauses))
          (result (cl-gensym "result")))
      `(let ((,result ,(car clause)))
         (if ,result
             (let ((it ,result))
               ,(if (cdr clause)
                    `(progn ,@(cdr clause))
                  'it))
           (acond ,@(cdr clauses)))))))

(def-edebug-spec acond 'cond)

(defmacro asetf (place expression)
  "Set PLACE to EXPRESSION with `it' bound to PLACE's previous value.
Evaluate the generalized PLACE and each of its subforms only once."
  `(cl-callf (lambda (it) ,expression) ,place))

(provide 'mb-anaphora)

;;; mb-anaphora.el ends here
