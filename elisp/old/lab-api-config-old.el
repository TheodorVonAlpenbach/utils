(defconst lab-default-config-file
  "c:/Users/mat_ber/Google Drive/site-lisp/mb-lisp/prototypes/lab.config")

(defun lab-api-parse-config-line (line)
  "Converts an assignment line \"lhs = rhs\" to a list (lhs rhs).
Also returns nil if the line is a comment line"
  (awhen (and (> (length line) 0)
	      (neq (aref line 0) ?#) ;skip comment lines
	      (split-string line "[[:space:]]*=[[:space:]]*"))
    (list (first it) (second it))))
;;(lab-api-parse-config-line "a = NOW")

(defun lab-api-read-config-lines (string)
  "Reads the lines in STRING and converts them to a list of assignments."
  (copy-if #'identity (mapcar #'lab-api-parse-config-line (string-to-lines string))))

(defun lab-api-substitute-assignment (assignment1 assignment2)
  "Substitute assignment1 into assignment2: If a substring in rhs of
assignment2 equals lhs of assignment1, it is substitued with rhs of
assignment1"
  (setf (second assignment2)
	(string-replace (second assignment2) (first assignment1) (second assignment1))))

(defun lab-api-substitute-assignments-old (assignments)
  "Substitutes the first assignment"
  (when (and (listp assignments) (> (length assignments) 1))
    (lab-api-substitute-assignment (first assignments) (second assignments))
    (lab-api-substitute-assignments (rest assignments))))

(defun lab-api-substitute-assignments (assignments)
  "Substitutes all ASSIGNMENTS with the first assignment. Then
repeats the process from the second assigment and so on"
  (loop for assignment on assignments do
	(loop with x = (first assignment)
	      for y in (rest assignment)
	      if y 
	      do (lab-api-substitute-assignment x y))))

(defun lab-api-parse-assignments (assignments)
  (lab-api-substitute-assignments assignments)
    assignments)

(defun lab-api-parse-config-string (string)
  (let ((assignments (lab-api-read-config-lines string)))
    (lab-api-substitute-assignments assignments)
    assignments))

(defun* lab-api-parse-config-file (&optional (file lab-default-config-file))
  (with-file-readonly file
   (lab-api-parse-config-string (buffer-string))))
;;(lab-api-parse-config-file)

(defun* lab-api-expand-expression (expression &optional (file lab-default-config-file))
  (let ((assignments (lab-api-parse-config-file))
	(exp-assignment (list nil expression)))
    (loop for a in assignments
	  do (lab-api-substitute-assignment a exp-assignment))
    (second exp-assignment)))
;;(lab-api-substitute-expression "forraveckan")

(provide 'lab-api-config)
