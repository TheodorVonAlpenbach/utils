;;; maths-common.el --- Record accessors for Maths -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ld-metadata)

;;; Users
;; Some convenient shortcuts to the user record
(defalias 'maths-user-id #'car)
(defalias 'maths-user-name #'cadr)
(defalias 'maths-user-age #'cl-third)
(defalias 'maths-user-rating #'cl-fourth)
(defalias 'maths-user-RD #'cl-fifth)
(defalias 'maths-user-metadata #'cl-sixth)

(defun maths-user-last-updated (user)
  "Return USER's last-updated timestamp."
  (ld-get-metadatum :updated (maths-user-metadata user)))

;;; Tasks
(defalias 'maths-task-id #'car)
(defalias 'maths-task-operation #'cadr)
(defalias 'maths-task-level #'cl-third)
(defalias 'maths-task-arguments #'cl-fourth)
(defalias 'maths-task-solution #'cl-fifth)
(defalias 'maths-task-rating #'cl-sixth)
(defalias 'maths-task-RD #'cl-seventh)

(provide 'maths-common)

;;; maths-common.el ends here
