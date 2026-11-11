;;; quiz-layout.el --- Paragraph layout for quiz items -*- lexical-binding: t; -*-

;;; Code:

(require 'quiz-edit)
(require 'quiz-navigation)

(define-obsolete-variable-alias
  'quiz-hanging-space
  'quiz-item-hanging-space
  "2026-11-11")

(defcustom quiz-item-hanging-space 1
  "Number of spaces between an item header and its text."
  :type 'natnum
  :group 'quiz)

(defun quiz-paragraph-indent
    (&optional position prefixes)
  "Return the header width for the item at POSITION.
PREFIXES limits which quiz item prefixes are recognized."
  (let ((number
         (quiz-current-item-number
          position prefixes)))
    (unless number
      (user-error "Point is not in a numbered quiz item"))
    (+ 2
       (length
        (number-to-string number)))))

(defun quiz-hanging-column
    (&optional position prefixes)
  "Return the text column for the item at POSITION.
PREFIXES limits which quiz item prefixes are recognized."
  (+ quiz-item-hanging-space
     (quiz-paragraph-indent
      position prefixes)))

(defun quiz-fill-item (&optional justify)
  "Fill the current quiz item.
JUSTIFY has the same meaning as in `fill-paragraph'."
  (interactive "P")
  (let ((start
         (quiz-current-item-region-start))
        (end
         (quiz-current-item-region-end)))
    (unless (and start end)
      (user-error "Point is not in a quiz item"))
    (save-excursion
      (let ((fill-prefix
             (make-string
              (quiz-hanging-column)
              ?\s)))
        (fill-region-as-paragraph
         start end justify)))))

(provide 'quiz-layout)

;;; quiz-layout.el ends here
