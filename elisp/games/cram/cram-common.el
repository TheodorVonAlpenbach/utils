;;; Users
;; Some convenient shortcuts to the user record
(defalias 'cram-user-id #'first)
(defalias 'cram-user-name #'second)
(defalias 'cram-user-age #'third)
(defalias 'cram-user-rating #'fourth)
(defalias 'cram-user-metadata #'fifth)

(defun cram-user-last-updated (user)
  (ld-get-metadatum :updated (cram-user-metadata user)))
;;(cram-user-last-updated (cram-db-get-user "Mats"))

;;; Problems
(defalias 'cram-problem-id #'first)
(defalias 'cram-problem-source-id #'second)
(defalias 'cram-problem-question #'third)
(defalias 'cram-problem-answer #'fourth)
(defalias 'cram-problem-picture #'fifth)
(defalias 'cram-problem-alternatives #'sixth)
(defalias 'cram-problem-hints #'seventh)
(defalias 'cram-problem-rating #'eighth)

(provide 'cram-common)
