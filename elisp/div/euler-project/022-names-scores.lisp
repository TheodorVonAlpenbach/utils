(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")

(defun 022-parse-names (filespec)
  (loop for x in (mb-split-sequence (file->string filespec) #\,)
	collect (string-trim '(#\") x)))

(defun 022-solution (&optional (filespec  "~/git/utils/elisp/div/euler-project/data/p022_names.txt"))
  "Assume only upcased, double-quoted, comma-separated names in FILESPEC."
  (let ((names (sort (022-parse-names filespec) #'string<)))
    ;; (subseq (sort names #'string<) 1000 1010)
    (position "COLIN" names :test #'string=)
    (loop for name in names
	  for pos from 1
	  sum (* pos (alphabetical-value-string name)))))
;;(022-solution)
;; => 871198282
