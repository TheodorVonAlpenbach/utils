;;;; Longest Common Subsequence (LCS)
(defun lcs-string (string1 string2)
  (let ((solutions (lcs (coerce string1 'list) (coerce string2 'list))))
    (loop for x in solutions
	  collect (coerce x 'string))))
;;(lcs-string "aøsdkfsdføaslkdj aø" "aøsdkføaslskdj aø")

(defun lcs-text (text1 text2)
  (lcs (string-to-lines text1) (string-to-lines text2) #'string-equal))
;;(length (first (lcs-text (buffer-string-no-properties) (buffer-string-no-properties))))

(cl-defun lcs-1 (ex y old &optional (test #'eq))
  "Helper function for `lcs'."
  (loop for ey in y
	for last-old in (cons (list nil) old)
	for curr-old in old
	for last-elt = (list nil) then curr-elt
	for l-old = (length (first curr-old))
	for l-last = (length (first last-elt))
	for curr-elt = (if (funcall test ex ey) 
			 (loop for l in curr-old collect (cons ex l))
			 (if (= l-old l-last)
			   (union last-old last-elt :test #'equal)
			   (if (> l-old l-last)
			     curr-old last-elt)))
	collect curr-elt))
;;(setq qwe (lcs-1 'c '(a e b c) ewq)) 
;;(setq ewq qwe)

(cl-defun lcs (x y &optional (test #'eq))
  "Returns a list of equivalent solution to the Longest Common
Subsequence (LCS) problem. See
http://en.wikipedia.org/wiki/Longest_common_subsequence_problem
for a discussion of the algorithm."
  (loop for ex in x
	for old = (make-list (length y) (list nil)) then new
	for new = (lcs-1 ex y old test)
	finally return (mapcar #'reverse (last-elt new))))
;;(lcs '("a" "b" "c" "d") '("a" "b" "d" "c") #'equal)

(cl-defun diff-simple-1 (buffer1 buffer2 &optional (diff-buffer "*diff-simple*"))
  "Helper function of `diff-simple'"
  (let* ((lines1 (string-to-lines (buffer-string-no-properties buffer1)))
	 (lines2 (string-to-lines (buffer-string-no-properties buffer2)))
	 (diff-lines (minimum (lcs lines1 lines2 #'string-equal) :key #'length)))
    (loop for l in diff-lines
	  collect (pop-until lines1 l #'string-equal) into res1
	  collect (pop-until lines2 l #'string-equal) into res2
	  if lines2 do (pop lines1)
	  if lines2 do (pop lines2)
	  finally return (list res1 res2))))

(cl-defun diff-simple (buffer1 buffer2 &optional (diff-buffer "*diff-simple*"))
  "Rudimenary implementation of `diff'. See `lcs' for core algorithm."
  (with-buffer (get-buffer-create diff-buffer)
    (let* ((lcs (diff-simple-1 buffer1 buffer2 diff-buffer))
	  (lines (loop for a in (first lcs)
		       for b in (second lcs)
		       append (loop for al in a collect (format "- %s" al))
		       append (loop for bl in b collect (format "+ %s" bl))
		       if (or a b) collect "===========================")))
      (insert (concat* lines :in "\n")))))
;;(diff-simple (get-buffer-create "qwe") (get-buffer-create "ewq"))

(provide 'lcs)
