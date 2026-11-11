;;; quiz-mode.el --- Major mode for quiz files -*- lexical-binding: t; -*-

;;; Code:

(declare-function quiz-fill-item "quiz-layout" (&optional justify))
(declare-function quiz-insert-empty-question "quiz-edit" ())
(declare-function quiz-mail-buffer "quiz-mail" (&optional refresh))
(declare-function quiz-move-active-region-to-a "quiz-cleanup" (beg end))
(declare-function quiz-summary-mail "quiz-summary" (days))
(declare-function quiz-summary-print "quiz-summary" (days))
(declare-function quiz-super-save "quiz-cleanup" ())

(defgroup quiz nil
  "Editing quiz files."
  :group 'applications)

(defcustom quiz-mode-input-method "norwegian-keyboard"
  "Input method activated when entering `quiz-mode'.
Set this to nil to leave the current input method unchanged."
  :type '(choice (const :tag "Do not change input method" nil)
                 string)
  :group 'quiz)

(defconst quiz-font-lock-keywords
  '(("^Q[1-9][0-9]*:" 0 font-lock-function-name-face append)
    ("^A[1-9][0-9]*:" 0 font-lock-variable-name-face append)
    ("^S[1-9][0-9]*:" 0 font-lock-type-face append)
    ("^C[1-9][0-9]*:" 0 font-lock-comment-face append)
    ("^E[1-9][0-9]*:" 0 font-lock-constant-face append)
    ("^T[1-9][0-9]*:" 0 font-lock-keyword-face append)
    ("^D[1-9][0-9]*:" 0 font-lock-warning-face append)
    ("^X[1-9][0-9]*:" 0 font-lock-string-face append))
  "Font-lock rules for quiz item headers.")

(defvar quiz-mode-map
  (let ((map (make-sparse-keymap))
        (local-prefix-map (make-sparse-keymap))
        (summary-map (make-sparse-keymap))
        (insert-map (make-sparse-keymap))
        (edit-map (make-sparse-keymap)))
    (define-key map "\C-\\" local-prefix-map)
    (define-key local-prefix-map "s" summary-map)
    (define-key local-prefix-map "i" insert-map)
    (define-key local-prefix-map "e" edit-map)
    (define-key local-prefix-map "m" #'quiz-mail-buffer)
    (define-key local-prefix-map "\C-\\" #'quiz-super-save)
    (define-key insert-map "e" #'quiz-insert-empty-question)
    (define-key summary-map "m" #'quiz-summary-mail)
    (define-key summary-map "p" #'quiz-summary-print)
    (define-key edit-map "m" #'quiz-move-active-region-to-a)
    map)
  "Keymap for `quiz-mode'.")

(define-derived-mode quiz-mode text-mode "Quiz"
  "Major mode for editing quiz files."
  (setq-local buffer-offer-save t)
  (setq-local font-lock-defaults '(quiz-font-lock-keywords))
  (setq-local fill-paragraph-function #'quiz-fill-item)
  (when quiz-mode-input-method
    (activate-input-method quiz-mode-input-method)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qz\\'" . quiz-mode))

(provide 'quiz-mode)

;;; quiz-mode.el ends here
