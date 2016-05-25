;;; Users
;; Some convenient shortcuts to the user record
(defalias 'maths-user-id #'first)
(defalias 'maths-user-name #'second)
(defalias 'maths-user-age #'third)
(defalias 'maths-user-rating #'fourth)
(defalias 'maths-user-RD #'fifth)
(defalias 'maths-user-metadata #'sixth)
(defun maths-user-last-updated (user)
  (ld-get-metadatum :updated (maths-user-metadata user)))
;;(maths-user-last-updated (maths-db-get-user "Mats"))

;;; Tasks
(defalias 'maths-task-id #'first)
(defalias 'maths-task-operation #'second)
(defalias 'maths-task-level #'third)
(defalias 'maths-task-arguments #'fourth)
(defalias 'maths-task-solution #'fifth)
(defalias 'maths-task-rating #'sixth)
(defalias 'maths-task-RD #'seventh)

(provide 'maths-common)
