;;; quiz-format.el --- Quiz item vocabulary and formatting -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(define-obsolete-variable-alias
  '*quiz-items-long* 'quiz-item-types "2026-11-11")
(define-obsolete-variable-alias
  '*quiz-items-short* 'quiz-item-prefixes "2026-11-11")

(defconst quiz-item-types
  '((:question "Q")
    (:answer "A")
    (:source "S")
    (:comment "C")
    (:extra "E")
    (:theme "T")
    (:difficulty "D")
    (:alternatives "X"))
  "Quiz item types and their one-character prefixes.")

(defconst quiz-item-prefixes
  (mapconcat #'cadr quiz-item-types "")
  "All recognized quiz item prefixes.")

(defun quiz-item-prefix (item)
  "Return the prefix for quiz ITEM.
Signal a user error when ITEM is not recognized."
  (or (cadr (assq item quiz-item-types))
      (user-error "Unknown quiz item type: %S" item)))

(cl-defun quiz-format-item (item number &optional (text ""))
  "Format ITEM NUMBER and TEXT as one quiz item."
  (unless (and (integerp number)
               (> number 0))
    (user-error "Quiz item number must be positive: %S"
                number))
  (unless (stringp text)
    (user-error "Quiz item text must be a string: %S"
                text))
  (format
   "%s%d: %s\n"
   (quiz-item-prefix item)
   number
   text))

(define-obsolete-function-alias
  'quiz-item #'quiz-format-item "2026-11-11")

(defun quiz-item-q (number &optional text)
  "Format question NUMBER with TEXT."
  (quiz-format-item :question number (or text "")))

(defun quiz-item-a (number &optional text)
  "Format answer NUMBER with TEXT."
  (quiz-format-item :answer number (or text "")))

(defun quiz-item-s (number &optional text)
  "Format source NUMBER with TEXT."
  (quiz-format-item :source number (or text "")))

(defun quiz-item-c (number &optional text)
  "Format comment NUMBER with TEXT."
  (quiz-format-item :comment number (or text "")))

(defun quiz-item-t (number &optional text)
  "Format theme NUMBER with TEXT."
  (quiz-format-item :theme number (or text "")))

(defun quiz-item-d (number &optional text)
  "Format difficulty NUMBER with TEXT."
  (quiz-format-item :difficulty number (or text "")))

(defun quiz-item-e (number &optional text)
  "Format extra field NUMBER with TEXT."
  (quiz-format-item :extra number (or text "")))

(defun quiz-item-x (number &optional text)
  "Format alternatives NUMBER with TEXT."
  (quiz-format-item :alternatives number (or text "")))

(cl-defun quiz-item-start-regexp
    (&optional (prefixes quiz-item-prefixes) (forward t))
  "Return a regexp matching quiz items with PREFIXES.
When FORWARD is nil, return the legacy backward-search form."
  (unless (and (stringp prefixes)
               (not (string-empty-p prefixes)))
    (user-error "Quiz item prefixes cannot be empty"))
  (if forward
      (format "^[%s][0-9]+: " (regexp-quote prefixes))
    (format "\n*[%s][0-9]+: " (regexp-quote prefixes))))

(define-obsolete-function-alias
  'quiz-regexp-item-start
  #'quiz-item-start-regexp
  "2026-11-11")

(provide 'quiz-format)

;;; quiz-format.el ends here
