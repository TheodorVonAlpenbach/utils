;;; quiz-mail.el --- Compose mail from quiz buffers -*- lexical-binding: t; -*-

;;; Code:

(require 'mail-extr)
(require 'message)
(require 'subr-x)
(require 'quiz-mode)

(declare-function mb-refresh-addresses nil ())

(defcustom quiz-default-mail-group "Hovedstyret av 1914"
  "Default recipient group used by `quiz-mail-buffer'."
  :type 'string
  :group 'quiz)

(defcustom quiz-mail-groups
  (list
   (list
    "Hovedstyret av 1914"
    (list
     "Tore van Dahl <toredah@student.matnat.uio.no>"
     "Per Stovne <perstovne@hotmail.com>"
     "Truls Flatberg <Truls.Flatberg@sintef.no>"
     "Ole Martin Halck <omhalck@online.no>"
     "Truls Flatberg <trulsf@ifi.uio.no>"
     "Thomas Bergkirk <thomas.bergkirk@rubicontv.no>")))
  "Named recipient groups for quiz mail.
Each entry has the form (GROUP-NAME ADDRESSES)."
  :type '(repeat
          (list
           (string :tag "Group")
           (repeat :tag "Addresses" string)))
  :group 'quiz)

(defun quiz-mail-address (address)
  "Return the email component of ADDRESS."
  (let ((email
         (cadr
          (mail-extract-address-components
           address))))
    (unless (and email
                 (string-match-p
                  "\\`[^@[:space:]]+@[^@[:space:]]+\\'"
                  email))
      (user-error
       "Invalid mail address: %S"
       address))
    email))

(define-obsolete-function-alias
  'purify-address #'quiz-mail-address "2026-11-11")

(defun quiz-mail-recipients (&optional group)
  "Return comma-separated recipients for GROUP."
  (let* ((name
          (or group quiz-default-mail-group))
         (entry
          (assoc name quiz-mail-groups))
         (addresses
          (cadr entry)))
    (unless entry
      (user-error
       "Unknown quiz mail group: %s"
       name))
    (unless addresses
      (user-error
       "Quiz mail group has no recipients: %s"
       name))
    (mapconcat
     #'quiz-mail-address
     addresses
     ", ")))

(defun quiz-mail-buffer (&optional refresh)
  "Compose a message containing the current quiz buffer.
With REFRESH non-nil, refresh external address data first when that
optional integration is available."
  (interactive "P")
  (when refresh
    (unless (fboundp 'mb-refresh-addresses)
      (user-error
       "External address refresh is unavailable"))
    (mb-refresh-addresses))
  (let ((recipients
         (quiz-mail-recipients))
        (subject
         (buffer-name))
        (body
         (buffer-substring-no-properties
          (point-min)
          (point-max))))
    (message-mail recipients subject)
    (message-goto-body)
    (insert body)
    (current-buffer)))

(provide 'quiz-mail)

;;; quiz-mail.el ends here
