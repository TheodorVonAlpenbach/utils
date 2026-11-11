;;; quiz-cleanup.el --- Cleanup commands for quiz files -*- lexical-binding: t; -*-

;;; Code:

(require 'subr-x)
(require 'quiz-edit)
(require 'quiz-layout)
(require 'quiz-navigation)

(defconst quiz-mojibake-replacements
  '(("Ã¥" . "å")
    ("Ã¸" . "ø")
    ("Â«" . "«")
    ("Â»" . "»")
    ("Ã¶" . "ö")
    ("Ã¦" . "æ"))
  "Legacy mojibake strings and their decoded replacements.")

(defconst quiz-cleanup-terminal-character-regexp
  "[[:alnum:])»\"]"
  "Regexp matching a retained final item character.")

(defun quiz-repair-mojibake ()
  "Repair known legacy mojibake in the current buffer."
  (interactive)
  (save-excursion
    (dolist (replacement quiz-mojibake-replacements)
      (goto-char (point-min))
      (while (search-forward (car replacement) nil t)
        (replace-match (cdr replacement) t t)))))

(define-obsolete-function-alias
  'quiz-clean-up-mule-shit
  #'quiz-repair-mojibake
  "2026-11-11")

(defun quiz-cleanup--find-item (prefix)
  "Move to the next item with PREFIX and return its beginning."
  (when (re-search-forward
         (quiz-item-start-regexp prefix)
         nil t)
    (goto-char (match-beginning 0))))

(defun quiz-remove-item (prefix)
  "Remove the next quiz item with PREFIX.
Return non-nil when an item was removed."
  (interactive
   (list
    (read-string "Item prefix: ")))
  (when (quiz-cleanup--find-item prefix)
    (let ((start (point))
          (end (quiz-current-item-region-end prefix)))
      (goto-char end)
      (when (eq (char-after) ?\n)
        (forward-char 1))
      (delete-region start (point))
      t)))

(defun quiz-remove-A ()
  "Remove the next answer item."
  (interactive)
  (quiz-remove-item "A"))

(defun quiz-remove-S ()
  "Remove the next source item."
  (interactive)
  (quiz-remove-item "S"))

(defun quiz-remove-items (prefix)
  "Remove all quiz items with PREFIX.
Return the number of items removed."
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (quiz-remove-item prefix)
        (setq count (1+ count))))
    count))

(defun quiz-remove-A-buffer ()
  "Remove every answer item in the current buffer."
  (interactive)
  (quiz-remove-items "A"))

(defun quiz-remove-S-buffer ()
  "Remove every source item in the current buffer."
  (interactive)
  (quiz-remove-items "S"))

(defun quiz-cleanup--answer-text-position ()
  "Return a marker at the current question's answer text."
  (save-excursion
    (let ((end
           (quiz-cleanup--question-limit)))
      (quiz-beginning-of-question)
      (unless (re-search-forward
               (quiz-item-start-regexp "A")
               end t)
        (user-error "Current question has no answer item"))
      (goto-char (match-beginning 0))
      (quiz-beginning-of-item-text "A")
      (copy-marker (point) t))))

(defun quiz-cleanup--question-limit ()
  "Return the raw end boundary of the current question."
  (save-excursion
    (quiz-beginning-of-question)
    (forward-line 1)
    (if (re-search-forward
         quiz-question-start-regexp nil t)
        (match-beginning 0)
      (point-max))))

(defun quiz-move-active-region-to-a (beg end)
  "Move the region from BEG to END into the current answer item."
  (interactive "r")
  (unless (quiz-current-item-p :question)
    (user-error "Region must be in a question item"))
  (let ((answer-position
         (quiz-cleanup--answer-text-position))
        (text
         (string-trim
          (buffer-substring-no-properties beg end))))
    (delete-region beg end)
    (goto-char beg)
    (just-one-space)
    (goto-char answer-position)
    (insert text)
    (unless (or (eolp)
                (looking-at-p "[ \t]"))
      (insert " "))
    (set-marker answer-position nil)))

(defun quiz-cleanup--goto-item (prefix)
  "Move to the item with PREFIX in the current question."
  (let ((end
         (quiz-cleanup--question-limit)))
    (quiz-beginning-of-question)
    (unless (re-search-forward
             (quiz-item-start-regexp prefix)
             end t)
      (user-error
       "Current question has no %s item"
       prefix))
    (goto-char (match-beginning 0))))

(defun quiz-cleanup--trim-item-end (prefix)
  "Trim trailing punctuation from the current question's PREFIX item."
  (quiz-cleanup--goto-item prefix)
  (quiz-end-of-item prefix)
  (while
      (and (char-before)
           (not
            (string-match-p
             quiz-cleanup-terminal-character-regexp
             (char-to-string (char-before)))))
    (delete-char -1)))

(defun quiz-cleanup--capitalize-item (prefix)
  "Capitalize the first word of the current question's PREFIX item."
  (quiz-cleanup--goto-item prefix)
  (quiz-beginning-of-item-text prefix)
  (capitalize-word 1))

(defun quiz-super-save ()
  "Normalize the current question, save, and return to the other window."
  (interactive)
  (when (use-region-p)
    (quiz-move-active-region-to-a
     (region-beginning)
     (region-end)))
  (quiz-cleanup--trim-item-end "Q")
  (unless (eq (char-before) ??)
    (insert "?"))
  (quiz-cleanup--capitalize-item "Q")
  (quiz-fill-item)
  (quiz-cleanup--trim-item-end "A")
  (quiz-cleanup--capitalize-item "A")
  (quiz-fill-item)
  (save-buffer)
  (unless (one-window-p)
    (other-window 1)))

(provide 'quiz-cleanup)

;;; quiz-cleanup.el ends here
