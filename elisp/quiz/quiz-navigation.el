;;; quiz-navigation.el --- Navigate quiz questions and items -*- lexical-binding: t; -*-

;;; Code:

(require 'quiz-format)

(defconst quiz-question-start-regexp
  "^Q[0-9]+: "
  "Regexp matching the beginning of a quiz question.")

(defun quiz-navigation--positive-count (count)
  "Return COUNT or one, requiring a positive integer."
  (let ((value (or count 1)))
    (unless (and (integerp value)
                 (> value 0))
      (user-error "Navigation count must be positive: %S"
                  value))
    value))

(defun quiz-beginning-of-list ()
  "Move to the beginning of the quiz list."
  (goto-char (point-min)))

(defun quiz-end-of-list ()
  "Move to the end of the quiz list."
  (goto-char (point-max)))

(defun quiz-forward-question (&optional count)
  "Move forward COUNT question beginnings.
Return the new point, or nil after moving to the end of the buffer
when there are too few remaining questions."
  (interactive "p")
  (if (and count
           (< count 0))
      (quiz-backward-question (- count))
    (let ((remaining (or count 1))
          found)
      (unless (natnump remaining)
        (user-error "Navigation count must be nonnegative: %S"
                    remaining))
      (if (zerop remaining)
          (point)
        (end-of-line)
        (while (and (> remaining 0)
                    (re-search-forward
                     quiz-question-start-regexp nil t))
          (setq found (match-beginning 0)
                remaining (1- remaining)))
        (if (zerop remaining)
            (goto-char found)
          (goto-char (point-max))
          nil)))))

(defun quiz-backward-question (&optional count)
  "Move backward COUNT question beginnings.
Return the new point, or nil after moving to the beginning of the
buffer when there are too few preceding questions."
  (interactive "p")
  (if (and count
           (< count 0))
      (quiz-forward-question (- count))
    (let ((remaining (or count 1))
          found)
      (unless (natnump remaining)
        (user-error "Navigation count must be nonnegative: %S"
                    remaining))
      (if (zerop remaining)
          (point)
        (beginning-of-line)
        (while (and (> remaining 0)
                    (re-search-backward
                     quiz-question-start-regexp nil t))
          (setq found (match-beginning 0)
                remaining (1- remaining)))
        (if (zerop remaining)
            (goto-char found)
          (goto-char (point-min))
          nil)))))

(defun quiz-beginning-of-question (&optional count)
  "Move to the beginning of the current question.
With COUNT greater than one, move backward to earlier question
beginnings."
  (let ((remaining
         (quiz-navigation--positive-count count))
        found)
    (beginning-of-line)
    (when (looking-at quiz-question-start-regexp)
      (setq found (point)
            remaining (1- remaining)))
    (while (and (> remaining 0)
                (re-search-backward
                 quiz-question-start-regexp nil t))
      (setq found (match-beginning 0)
            remaining (1- remaining)))
    (goto-char (or found (point-min)))))

(defun quiz-end-of-question (&optional count)
  "Move to the end of the current question.
With COUNT greater than one, move to the end of a later question.
The returned position is immediately after the final non-whitespace
character."
  (let ((remaining
         (quiz-navigation--positive-count count)))
    (quiz-beginning-of-question)
    (while (> remaining 1)
      (unless (quiz-forward-question)
        (user-error "No later quiz question"))
      (setq remaining (1- remaining)))
    (forward-line 1)
    (if (re-search-forward
         quiz-question-start-regexp nil t)
        (goto-char (match-beginning 0))
      (goto-char (point-max)))
    (skip-chars-backward " \t\r\n")
    (point)))

(defun quiz-goto-question (number)
  "Move to the question labeled NUMBER.
Return the new point, or nil when the question does not exist."
  (interactive "nQuestion number: ")
  (unless (and (integerp number)
               (> number 0))
    (user-error "Question number must be positive: %S"
                number))
  (goto-char (point-min))
  (when (re-search-forward
         (format "^Q%d: " number) nil t)
    (goto-char (match-beginning 0))))

(define-obsolete-function-alias
  'quiz-item-beg-regexp
  #'quiz-item-start-regexp
  "2026-11-11")

(defun quiz-forward-item (&optional prefixes)
  "Move to the next item whose prefix is in PREFIXES."
  (end-of-line)
  (when (re-search-forward
         (quiz-item-start-regexp
          (or prefixes quiz-item-prefixes))
         nil t)
    (goto-char (match-beginning 0))))

(defun quiz-backward-item (&optional prefixes)
  "Move to the previous item whose prefix is in PREFIXES."
  (beginning-of-line)
  (when (re-search-backward
         (quiz-item-start-regexp
          (or prefixes quiz-item-prefixes))
         nil t)
    (goto-char (match-beginning 0))))

(defun quiz-beginning-of-item (&optional prefixes)
  "Move to the beginning of the current quiz item."
  (interactive)
  (end-of-line)
  (when (re-search-backward
         (quiz-item-start-regexp
          (or prefixes quiz-item-prefixes))
         nil t)
    (goto-char (match-beginning 0))))

(defun quiz-beginning-of-item-text (&optional prefixes)
  "Move to the beginning of the current quiz item's text."
  (when (quiz-beginning-of-item prefixes)
    (re-search-forward
     (quiz-item-start-regexp
      (or prefixes quiz-item-prefixes))
     (line-end-position)
     t)))

(defun quiz-current-item-region-start (&optional prefixes)
  "Return the beginning of the current quiz item."
  (save-excursion
    (quiz-beginning-of-item prefixes)))

(defun quiz-current-item-region-end (&optional prefixes)
  "Return the end of the current quiz item's text.
Trailing whitespace between items is excluded."
  (save-excursion
    (when (quiz-beginning-of-item prefixes)
      (forward-line 1)
      (if (re-search-forward
           (quiz-item-start-regexp quiz-item-prefixes)
           nil t)
          (goto-char (match-beginning 0))
        (goto-char (point-max)))
      (skip-chars-backward " \t\r\n")
      (point))))

(defun quiz-end-of-item (&optional prefixes)
  "Move to the end of the current quiz item's text."
  (let ((end
         (quiz-current-item-region-end prefixes)))
    (when end
      (goto-char end))))

(defun quiz-goto-question-end ()
  "Move to the end of the current question."
  (interactive)
  (quiz-end-of-question))

(defun quiz-question-end (&optional position)
  "Return the end of the question containing POSITION."
  (save-excursion
    (when position
      (goto-char position))
    (quiz-end-of-question)))

(provide 'quiz-navigation)

;;; quiz-navigation.el ends here
