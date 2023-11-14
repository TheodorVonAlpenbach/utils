(defun dd-parse-error-line-exception (exception)
  (string-match* "\\.\\([a-zA-Z]+\\):.*" exception :num 1))
;;(dd-parse-error-line-exception "n.c.a.p.ExceptionLogger:34")

(defun dd-parse-error-line-url (url)
  "Remove site part and path parameters"
  (let* ((parts (split-string
		 (string-match* "https://[^/]*/*\\([^?]*\\)"
		   (car (split-string url ",")) :num 1)
		 "/"))
	 (pos (cl-position-if
		  #'(lambda (x) (or (uuid-p x) (string-to-integer x)))
		   parts
		)))
    (concat* (if pos (head pos parts) parts) :in "/")))
;;(dd-parse-error-line-url "https://munch.skolen.cdu.no/api/event/USER_STARTED_PAGE")
;;(dd-parse-error-line-url "https://skolenmin.cdu.no/api/task/commit-answer/4/60d08b1c54d6df3eaaaa42ec/594d3459cb99b500132bc742, pathParameters{componentId=[60d08b1c54d6df3eaaaa42ec], language=[594d3459cb99b500132bc742], version=[4]}, queryParameters={}")

(defun dd-error-line-parts (line)
  (string-match* "\\([^ ]*\\) - \\([0-9]+ [0-9]+\\) - .*\\(https://.*\\)"
    line :num '(1 2 3)))
;;(dd-error-line-parts "2023-01-05 09:28:51 ERROR n.c.a.p.ExceptionLogger:34 - 5235622387300641774 809369629833823069 - Error occured at https://skolenmin.cdu.no/api/group-overview/1091864/component/5f59e5267f52b9da3a0f8258/594d3459cb99b500132bc742?componentVersion=10, pathParameters{componentSourceId=[5f59e5267f52b9da3a0f8258], groupId=[1091864], languageSourceId=[594d3459cb99b500132bc742]}, queryParameters={componentVersion=[10]}")
;;(setf qwe-url (third (dd-error-line-parts "2023-01-05 09:28:51 ERROR n.c.a.p.ExceptionLogger:34 - 5235622387300641774 809369629833823069 - Error occured at https://skolenmin.cdu.no/api/group-overview/1091864/component/5f59e5267f52b9da3a0f8258/594d3459cb99b500132bc742?componentVersion=10, pathParameters{componentSourceId=[5f59e5267f52b9da3a0f8258], groupId=[1091864], languageSourceId=[594d3459cb99b500132bc742]}, queryParameters={componentVersion=[10]}")))
;;(dd-parse-error-line-url qwe-url)

(defun dd-parse-error-line (line)
  (destructuring-bind (exception ignore-for-now url)
      (string-match* "\\([^ ]*\\) - \\([0-9]+ [0-9]+\\) - .*\\(https://.*\\)"
	line :num '(1 2 3))
    (list (dd-parse-error-line-url url)
	  (dd-parse-error-line-exception exception))))
;;(dd-parse-error-line "2023-01-05 09:28:51 ERROR n.c.a.p.ExceptionLogger:34 - 5235622387300641774 809369629833823069 - Error occured at https://skolenmin.cdu.no/api/group-overview/1091864/component/5f59e5267f52b9da3a0f8258/594d3459cb99b500132bc742?componentVersion=10, pathParameters{componentSourceId=[5f59e5267f52b9da3a0f8258], groupId=[1091864], languageSourceId=[594d3459cb99b500132bc742]}, queryParameters={componentVersion=[10]}")
;;(dd-parse-error-line ewq)

(defun dd-parse-error-chunks (chunks)
  (loop for chunk in chunks
	for i from 1
	if (empty-string-p chunk)
	do (message "Chunk %d is empty!" i)
	else
	if (string/= line "--")
	collect (dd-parse-error line)))

(defun dd-parse-error-file (filename)
  (loop for chunk in chunks
	for i from 1
	if (empty-string-p chunk)
	do (message "Chunk %d is empty!" i)
	else
	if (string/= line "--")
	collect (dd-parse-error line)))

(defun dd-error-report (filename)
  (cl-sort (loop for x in (equivalence-class
			      (dd-parse-error-lines lines)
		    :test #'equal :key #'car)
	 for l = (length x)
	 for url = (caar x)
	 collect (list  l url))
    #'> :key #'car))
;;(dd-error-report (subseq (file-lines "~/ada/errors.txt") 0 1500))
;;(dd-error-report qwe)

(defun dd-error-report-string (lines)
  (concat* lines
    :pre "\n"
    :in "\n"
    :key #'(lambda (x) (apply #'format "%d\t%s" x))))
;;(dd-error-report-string (dd-error-report (subseq (file-lines "~/ada/errors.txt") 0 1500)))

;;"2023-01-05 09:28:27 ERROR n.c.a.p.ExceptionLogger:34 - 8815181062283748063 690175184260534731 - Error occured at https://skolenmin.cdu.no/api/task/commit-answer/4/60d08b1c54d6df3eaaaa42ec/594d3459cb99b500132bc742, pathParameters{componentId=[60d08b1c54d6df3eaaaa42ec], language=[594d3459cb99b500132bc742], version=[4]}, queryParameters={}"
;;(setf qwe (head 10 (file-lines "~/ada/errors.txt")))
;;(setf ewq (first qwe))
