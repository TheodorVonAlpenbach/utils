;;; quiz-config.el --- Paths and buffers for quiz files -*- lexical-binding: t; -*-

;;; Code:

(require 'subr-x)
(require 'quiz-mode)

(define-obsolete-variable-alias
  '*quiz-buffer* 'quiz-buffer-name "2026-11-11")
(define-obsolete-variable-alias
  '*quiz-dir* 'quiz-directory "2026-11-11")
(define-obsolete-variable-alias
  '*quiz-without-source-dir*
  'quiz-without-source-directory
  "2026-11-11")
(define-obsolete-variable-alias
  '*quiz-scratch-buffername*
  'quiz-scratch-buffer-name
  "2026-11-11")

(defcustom quiz-buffer-name "*quiz*"
  "Name of the general-purpose quiz buffer."
  :type 'string
  :group 'quiz)

(defcustom quiz-directory
  (expand-file-name "projects/quiz/" "~")
  "Directory containing dated quiz files."
  :type 'directory
  :group 'quiz)

(defcustom quiz-without-source-directory
  (expand-file-name "without-source/" quiz-directory)
  "Directory for quiz exports without source fields."
  :type 'directory
  :group 'quiz)

(defcustom quiz-scratch-buffer-name "scratch.qz"
  "File and buffer name used for scratch quiz questions."
  :type 'string
  :group 'quiz)

(defcustom quiz-popup-buffer-size 12
  "Preferred height for a popup quiz buffer."
  :type 'integer
  :group 'quiz)

(defun quiz-file-path (file-name &optional directory)
  "Return the path for FILE-NAME in DIRECTORY."
  (unless (and (stringp file-name)
               (not (string-empty-p file-name)))
    (user-error "Quiz file name cannot be empty"))
  (expand-file-name
   file-name
   (or directory quiz-directory)))

(defun quiz-config--date-string (date)
  "Return DATE as an ISO date string."
  (cond
   ((null date)
    (format-time-string "%F"))
   ((and (stringp date)
         (string-match-p
          "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
          date))
    date)
   ((stringp date)
    (format-time-string "%F" (date-to-time date)))
   (t
    (format-time-string "%F" date))))

(defun quiz-get-file (&optional date directory)
  "Return the dated quiz path for DATE in DIRECTORY."
  (quiz-file-path
   (format
    "mq-%s.qz"
    (quiz-config--date-string date))
   directory))

(defun quiz-get-buffer (&optional buffer-name)
  "Return the quiz buffer named BUFFER-NAME.
Visit the corresponding file below `quiz-directory' when necessary,
and ensure the resulting buffer uses `quiz-mode'."
  (let* ((name
          (or buffer-name
              (format
               "mq-%s.qz"
               (quiz-config--date-string nil))))
         (buffer
          (or (get-buffer name)
              (find-file-noselect
               (quiz-file-path name)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'quiz-mode)
        (quiz-mode)))
    buffer))

(provide 'quiz-config)

;;; quiz-config.el ends here
