;;; Users
;; Some convenient shortcuts to the user record
(defalias 'cram-user-id #'first)
(defalias 'cram-user-name #'second)
;; (defalias 'cram-user-rating #'third)
(defun cram-user-rating (x) (third x))
(defun cram-user-rating-e (x) (first (cram-user-rating x)))
(defun cram-user-rating-d (x) (second (cram-user-rating x)))
(defalias 'cram-user-metadata #'last-elt)

(defun cram-user-last-updated (user)
  (ld-get-metadatum :updated (cram-user-metadata user)))
;;(cram-user-last-updated (cram-db-get-user "Ludvik"))

;;; Problems
(defalias 'cram-problem-id #'first)
(defalias 'cram-problem-source-id #'second)
(defalias 'cram-problem-question #'third)
(defalias 'cram-problem-answer #'fourth)
(defalias 'cram-problem-picture #'fifth)
(defalias 'cram-problem-alternatives #'sixth)
(defalias 'cram-problem-hints #'seventh)
(defalias 'cram-problem-rating #'eighth)
(defun cram-problem-rating-e (x) (first (cram-problem-rating x)))
(defun cram-problem-rating-d (x) (second (cram-problem-rating x)))
(defun cram-problem-metadata (x) (rest (ninth x)))
(defun cram-problem-updated (x) (getf (cram-problem-metadata x) :updated))

(provide 'cram-common)
