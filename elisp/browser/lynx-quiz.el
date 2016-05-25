(require 'quiz (expand-file-name "quiz/quiz.el" *mb-lisp-dir*))
(require 'mb-utils-math)
(require 'mb-buffer)

(defvar *lynx-save-quiz-buffer-after-nth-question* 5 
  "Auto saves after this many appended questions to default quiz buffer")

(defun lynx-insert-region-in-quiz-outline (beg end)
  (interactive "r")
  (lynx-insert-region-in-quiz-buffer beg end (quiz-get-buffer)))

(defun lynx-insert-region-in-quiz-scratch (beg end)
  (interactive "r")
  (lynx-insert-region-in-quiz-buffer beg end (quiz-get-buffer *quiz-scratch-buffername*)))

(defun lynx-insert-region-in-quiz-buffer (beg end quiz-buffer)
  "Insert region as the text in a quiz question. If a prefix argument
N is given, the function saves the quiz buffer after each Nth question
generated. If N is negative, if never saves buffer. If prefix argument
is not given the function saves quiz buffer after each
`*quiz-save-after-nth-question*'
TODO: move some of the expressions into #'QUIZ-INSERT-QUESTION."
  (let* ((point (quiz-insert-question
		 (string-trim (buffer-substring beg end)) ""
		 *lynx-current-url*
		 quiz-buffer)))
    (switch-to-buffer-other-window quiz-buffer)
    (message "%s" point)
    (goto-char point)
    (quiz-beginning-of-item-text "Q")
    ;; save each 5th question with backup
    (if (modp (quiz-read-n-prev-question) 0 (or current-prefix-arg 5))
      (save-buffer))
    (set-window-size 12)))

(provide 'lynx-quiz)
