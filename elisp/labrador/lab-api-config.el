;;;; lab-api-config-expand-expression expr
;;;; private prefix: lac

;;; local variables
(defconst lac-default-config-path 
  "c:/Users/mat_ber/Google Drive/site-lisp/mb-lisp/prototypes/lab.config"
  "Default path for macro definition file.")

(defvar lac-definitions nil 
  "A list of form ((LHS RHS) ...)")

(defvar lac-comment-start-symbol "#"
  "Marks start of the comment part of a line.")

(defvar lac-macro-identifier-regexp "[[:alpha:]][[:alnum:]]*"
  "Marks start of the comment part of a line.")

(defvar lac-assignment-operator "="
  "Marks start of the comment part of a line.")

;;; methods
(defun lac-expand (expression definitions)
  "If expression contains a substring that equals some LHS in
`lac-definitions' is is substituted with the corresponding RHS."
  (loop for x in definitions do
	(setf expression (string-replace expression (first x) (second x))))
  expression)
 
(defun lac-definition-from-line (line &optional definitions)
  "Converts LINE to a macro definition, based other macro
definitions. Also, it checks rudimentary syntax."
  (multiple-value-bind (lhs rhs)
      (mapcar #'string-trim (split-string line "="))
    (unless (string-match* lac-macro-identifier-regexp lhs)
      (message "lac-definition-from-line: In line '%s': left-hand-side is not well-formed." line)
      (setq rhs ""))
    (unless rhs
      (message "lac-definition-from-line: In line '%s': missing right-hand-side." line))
    ;; else: we are ok
      (list lhs (lac-expand rhs definitions))))

(defun lac-definitions-from-lines (lines)
  "Converts LINES to a list of macro definitions. See
`lac-definitions-from-file' for more details."
  (loop for l in lines 
	for def = (lac-definition-from-line l definitions)
	if (listp def) collect def into definitions
	finally return definitions))

(defun lac-remove-comments (lines)
  "Removes line substrings starting with `lac-comment-start-symbol'"
  (loop for l in lines 
	for lhs = (string-trim (first (split-string l "#")))
	if (not (empty-string-p lhs)) collect lhs))

(defun lac-definitions-from-string (string)
  "Converts STRING to a list of macro definitions. See
`lac-definitions-from-file' for more details."
  (lac-definitions-from-lines (lac-remove-comments (string-to-lines string))))

(defun* lac-definitions-from-file (&optional (config-file lac-default-config-path))
  "Converts CONFIG-FILE to a list of macro definitions. The file
should be on format LHS1 = RHS1 LHS2 = RHS2 ... The file is
converted to a list of the same assigments ((LHS1 RHS1*) (LHS2
RHS2*) ...), except that the RHSs are modified based on previous
assignments. See `lac-expand' for more details on how the
modication is preformed."
  (with-file-readonly config-file
   (lac-definitions-from-string (buffer-string))))

(defun lab-api-macro-expand (expression &optional reset)
  "Expands EXPRESSION based on substitution rules defined in
`lac-definitions'"
  (when (or reset (not lac-definitions))
    (setq lac-definitions (lac-definitions-from-file)))
  (lac-expand expression lac-definitions))
;;(lab-api-macro-expand "forraveckan" t)

(provide 'lab-api-config)
