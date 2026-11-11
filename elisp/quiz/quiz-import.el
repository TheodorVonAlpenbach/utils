;;; quiz-import.el --- Import pasted quiz text -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'quiz-format)

(defun quiz-import-aftenposten-parse-question (text)
  "Parse one Aftenposten question from TEXT.
Return a cons cell containing the question and answer text."
  (let ((lines
         (split-string
          (string-trim text)
          "\n"
          nil
          "[ \t]+"))
        question
        answer)
    (when (and lines
               (string-match
                "\\`[0-9]+\\.[ \t]*\\(.+\\)\\'"
                (car lines)))
      (setq question
            (match-string 1 (car lines))))
    (dolist (line (cdr lines))
      (when (string-match
             "\\`Riktig svar:[ \t]*\\(.+\\)\\'"
             line)
        (setq answer
              (match-string 1 line))))
    (unless (and question answer)
      (user-error
       "Malformed Aftenposten quiz question: %S"
       text))
    (cons question answer)))

(defun quiz-import-aftenposten-question
    (beg end number &optional source)
  "Convert the Aftenposten question from BEG to END using NUMBER.
Include SOURCE when it is non-nil. Return point after the inserted
quiz question."
  (let* ((parsed
          (quiz-import-aftenposten-parse-question
           (buffer-substring-no-properties
            beg end)))
         (replacement
          (concat
           (quiz-item-q number (car parsed))
           (quiz-item-a number (cdr parsed))
           (if source
               (quiz-item-s number source)
             ""))))
    (delete-region beg end)
    (goto-char beg)
    (insert replacement)
    (point)))

(define-obsolete-function-alias
  'quiz-convert-aftenposten-nyhetsquiz-question-to-qz-question
  #'quiz-import-aftenposten-question
  "2026-11-11")

(defun quiz-import-aftenposten-buffer (&optional source)
  "Convert pasted Aftenposten questions in the current buffer.
Include SOURCE in each generated question when it is non-nil. Parse
and validate every question before modifying the buffer."
  (interactive)
  (let* ((blocks
          (split-string
           (buffer-substring-no-properties
            (point-min)
            (point-max))
           "\n[ \t]*\n+"
           t
           "[[:space:]]+"))
         (questions
          (mapcar
           #'quiz-import-aftenposten-parse-question
           blocks)))
    (unless questions
      (user-error
       "Buffer contains no Aftenposten quiz questions"))
    (erase-buffer)
    (cl-loop
     for (question . answer) in questions
     for number from 1
     do
     (insert
      (quiz-item-q number question)
      (quiz-item-a number answer))
     (when source
       (insert
        (quiz-item-s number source)))
     (newline))
    (goto-char (point-min))
    (length questions)))

(define-obsolete-function-alias
  'quiz-convert-aftenposten-nyhetsquiz-to-qz-file
  #'quiz-import-aftenposten-buffer
  "2026-11-11")

(provide 'quiz-import)

;;; quiz-import.el ends here
