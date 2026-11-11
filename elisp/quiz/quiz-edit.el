;;; quiz-edit.el --- Structural editing for quiz files -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'quiz-format)
(require 'quiz-navigation)

(declare-function quiz-get-buffer "quiz" (&optional buffer-name))

(defun quiz-edit--positive-number (number)
  "Return NUMBER after requiring a positive integer."
  (unless (and (integerp number)
               (> number 0))
    (user-error "Quiz number must be positive: %S"
                number))
  number)

(defun quiz-edit--item-number-regexp (&optional prefixes)
  "Return an item regexp capturing the number for PREFIXES."
  (format
   "^[%s]\\([0-9]+\\): "
   (regexp-quote
    (or prefixes quiz-item-prefixes))))

(defun quiz-current-item ()
  "Return the type of the quiz item at point."
  (save-excursion
    (when (quiz-beginning-of-item)
      (car
       (cl-find
        (char-to-string (char-after))
        quiz-item-types
        :key #'cadr
        :test #'string=)))))

(defun quiz-current-item-p (item)
  "Return non-nil when point is in a quiz item of type ITEM."
  (eq (quiz-current-item) item))

(defun quiz-current-item-number
    (&optional position prefixes)
  "Return the item number at or nearest POSITION.
PREFIXES limits which item prefixes are recognized."
  (save-excursion
    (when position
      (goto-char position))
    (let ((regexp
           (quiz-edit--item-number-regexp prefixes)))
      (cond
       ((and (quiz-beginning-of-item prefixes)
             (looking-at regexp))
        (string-to-number (match-string 1)))
       ((progn
          (goto-char (or position (point)))
          (re-search-forward regexp nil t))
        (string-to-number (match-string 1)))))))

(defun quiz-next-question-number (&optional position)
  "Return the question number to insert at POSITION."
  (if-let ((number
            (quiz-current-item-number position)))
      (1+ number)
    1))

(define-obsolete-function-alias
  'quiz-read-n-prev-question
  #'quiz-next-question-number
  "2026-11-11")

(defun quiz-set-item-number (number)
  "Set the current quiz item's number to NUMBER."
  (interactive "nItem number: ")
  (quiz-edit--positive-number number)
  (unless (quiz-beginning-of-item)
    (user-error "Point is not in a quiz item"))
  (unless (looking-at
           (quiz-edit--item-number-regexp))
    (user-error "Malformed quiz item header"))
  (replace-match
   (number-to-string number)
   t t nil 1)
  number)

(define-obsolete-function-alias
  'quiz-item-set-number
  #'quiz-set-item-number
  "2026-11-11")

(defun quiz-set-question-number
    (number &optional position)
  "Set every item in the question at POSITION to NUMBER."
  (interactive "nQuestion number: ")
  (quiz-edit--positive-number number)
  (save-excursion
    (when position
      (goto-char position))
    (let ((beginning
           (quiz-beginning-of-question))
          (end
           (copy-marker
            (quiz-end-of-question))))
      (goto-char beginning)
      (while (re-search-forward
              (quiz-edit--item-number-regexp)
              end t)
        (replace-match
         (number-to-string number)
         t t nil 1))
      (set-marker end nil)))
  number)

(define-obsolete-function-alias
  'quiz-question-set-number
  #'quiz-set-question-number
  "2026-11-11")
(define-obsolete-function-alias
  'quiz-question-set-numbers
  #'quiz-set-question-number
  "2026-11-11")

(defun quiz-renumber-questions (&optional base)
  "Renumber all questions consecutively, starting with BASE.
Return the number of questions changed."
  (interactive "p")
  (let ((number
         (quiz-edit--positive-number
          (or base 1)))
        (count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              quiz-question-start-regexp
              nil t)
        (quiz-set-question-number
         number
         (match-beginning 0))
        (setq number (1+ number)
              count (1+ count))))
    count))

(define-obsolete-function-alias
  'quiz-list-set-numbers
  #'quiz-renumber-questions
  "2026-11-11")

(defun quiz-reset-question-numbers ()
  "Renumber every question starting with one."
  (interactive)
  (quiz-renumber-questions 1))

(defun quiz-edit--prepare-insertion ()
  "Ensure point is preceded by a blank line when necessary."
  (unless (bobp)
    (unless (bolp)
      (insert "\n"))
    (unless
        (save-excursion
          (forward-line -1)
          (looking-at-p "[ \t]*$"))
      (insert "\n"))))

(defun quiz-insert-question-items
    (items &optional position number)
  "Insert quiz ITEMS at POSITION using NUMBER.
ITEMS is a list of (TYPE . TEXT) pairs. Return point immediately
after the inserted question."
  (unless items
    (user-error "A quiz question must contain at least one item"))
  (dolist (item items)
    (quiz-item-prefix (car-safe item))
    (unless (stringp (cdr-safe item))
      (user-error "Quiz item text must be a string: %S"
                  item)))
  (when position
    (goto-char position))
  (let ((item-number
         (quiz-edit--positive-number
          (or number
              (quiz-next-question-number
               (point))))))
    (quiz-edit--prepare-insertion)
    (dolist (item items)
      (insert
       (quiz-format-item
        (car item)
        item-number
        (cdr item))))
    (newline)
    (point)))

(defun quiz-insert-question-base
    (question answer source &optional position)
  "Insert QUESTION, ANSWER, and SOURCE at POSITION."
  (quiz-insert-question-items
   `((:question . ,question)
     (:answer . ,answer)
     (:source . ,source))
   position))

(defun quiz-insert-question-template (&optional position)
  "Insert an empty extended question template at POSITION."
  (quiz-insert-question-items
   '((:question . "")
     (:answer . "")
     (:comment . "")
     (:extra . "")
     (:theme . "")
     (:difficulty . ""))
   position))

(define-obsolete-function-alias
  'quiz-insert-question-pugg
  #'quiz-insert-question-template
  "2026-11-11")

(defun quiz-insert-empty-question ()
  "Insert an empty question and move to its question text."
  (interactive)
  (quiz-insert-question-template)
  (quiz-beginning-of-item-text "Q"))

(defun quiz-insert-question
    (question answer source
     &optional buffer position)
  "Append QUESTION, ANSWER, and SOURCE to BUFFER.
Insert at POSITION when it is non-nil and return the final point."
  (let ((target
         (or buffer
             (quiz-get-buffer))))
    (with-current-buffer target
      (quiz-insert-question-base
       question answer source
       (or position (point-max))))))

(provide 'quiz-edit)

;;; quiz-edit.el ends here
