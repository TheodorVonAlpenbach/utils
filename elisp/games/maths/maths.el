;;; maths.el --- Interactive Maths game interface -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'thingatpt)
(require 'maths-backend)
(require 'maths-config)
(require 'maths-common)
(require 'mb-utils-strings)
(require 'mb-utils-time)

(declare-function rating-plot-history "rating-plot")

(defun maths-operator-string (operator)
  "Return OPERATOR's printable name."
  (symbol-name operator))

(defun maths-format-task (operation arguments &optional solution)
  "Format a Maths task from OPERATION, ARGUMENTS, and SOLUTION."
  (format "%d %s %d = %s"
          (car arguments)
          (maths-operator-string (maths-operator operation))
          (cadr arguments)
          (or solution "")))

(defun maths-format-match (user task)
  "Format USER and TASK for presentation."
  (concat
   (format "Your rating: %d (RD = %d)\n"
           (maths-user-rating user)
           (maths-user-RD user))
   (format "Task rating: %d (RD = %d)\n"
           (maths-task-rating task)
           (maths-task-RD task))
   "\n"
   (format "Problem #%d:\n" (maths-task-id task))
   (maths-format-task
    (maths-task-operation task)
    (maths-task-arguments task))))

(cl-defun maths-new (&optional (level 1))
  "Draw and display a new task.
LEVEL is retained for compatibility."
  (ignore level)
  (if-let ((user (or (maths-current-user)
                     (maths-register-new-user))))
      (if-let ((task
                (maths-draw-task
                 :rating (maths-user-rating user))))
          (progn
            (erase-buffer)
            (insert (maths-format-match user task))
            (time-set-reference))
        (message "Could not draw a Maths task"))
    (message "No Maths user is selected")))

(defun maths-read-answer ()
  "Return the number at point as the submitted answer."
  (number-at-point))

(cl-defun maths-short-judgement
    (score answer solution
           &optional
           (correct-color "ForestGreen")
           (error-color "red"))
  "Format a short judgement for SCORE, ANSWER, and SOLUTION."
  (if (= answer solution)
      (format
       "%s Your score was %.2f"
       (propertize
        "Correct!" 'face
        (list :foreground correct-color :weight 'bold))
       score)
    (format
     "%s Correct answer is %S"
     (propertize
      "Incorrect!" 'face
      (list :foreground error-color :weight 'bold))
     solution)))

(defun maths--rating-differences (new-ratings old-ratings)
  "Subtract OLD-RATINGS from NEW-RATINGS componentwise."
  (cl-mapcar
   (lambda (new old)
     (cl-mapcar #'- new old))
   new-ratings old-ratings))

(defun maths-long-judgement
    (answer time solution score old-ratings new-ratings)
  "Format a detailed judgement and rating change summary."
  (let ((differences
         (maths--rating-differences new-ratings old-ratings)))
    (cl-destructuring-bind
        ((user-rating user-RD) (task-rating task-RD))
        (mapcar
         (lambda (rating) (mapcar #'round rating))
         new-ratings)
      (cl-destructuring-bind
          ((user-rating-diff user-RD-diff)
           (task-rating-diff task-RD-diff))
          (mapcar
           (lambda (rating) (mapcar #'round rating))
           differences)
        (concat
         "\n\n"
         (maths-short-judgement score answer solution)
         "\n"
         (format "Time spent: %.1f seconds\n" (/ time 1000.0))
         "\n"
         (format "Your new rating is %d (%d)\n"
                 user-rating user-rating-diff)
         (format "Your new RD is %d (%d)\n"
                 user-RD user-RD-diff)
         (format "Task's new rating is %d (%d)\n"
                 task-rating task-rating-diff)
         (format "Task's new RD is %d (%d)\n"
                 task-RD task-RD-diff))))))

(defun maths-submit ()
  "Submit the current answer and display its judgement."
  (let* ((time (time-elapsed nil))
         (answer (maths-read-answer))
         (old-ratings (maths-current-ratings))
         (score (maths-report-answer answer time))
         (solution
          (maths-task-solution (maths-current-task)))
         (new-ratings (maths-current-ratings)))
    (if (and (maths-auto-continue-p)
             (not current-prefix-arg))
        (progn
          (message
           "%s"
           (maths-short-judgement score answer solution))
          (maths-new))
      (insert
       (maths-long-judgement
        answer time solution score old-ratings new-ratings)))))

(defun maths-answer-submitted-p ()
  "Return non-nil when the current buffer contains a judgement."
  (string-match-p "Time spent:" (buffer-string)))

;;; Interactive commands

(defun maths-enter ()
  "Submit the current answer or draw the next task."
  (interactive)
  (if (maths-answer-submitted-p)
      (maths-new)
    (maths-submit)))

(defun maths-register-new-user ()
  "Prompt for and register a new Maths user."
  (interactive)
  (let ((user-name (read-from-minibuffer "New user name: "))
        (age
         (string-to-number
          (read-from-minibuffer "Age: "))))
    (unless (> age 0)
      (user-error "Age must be a positive number"))
    (when
        (yes-or-no-p
         (format
          "Are you sure that you want to register a new user named %S? "
          user-name))
      (if (maths-db-get-user user-name)
          (progn
            (message "User name %S is already registered" user-name)
            nil)
        (let ((user (maths-add-user user-name age)))
          (maths-set-current-user user)
          (message "User %s registered" user-name)
          user)))))

(defun maths-change-user ()
  "Prompt for a different current user."
  (interactive)
  (let ((others
         (maths-db-user-names
          :sans (maths-user-name (maths-current-user)))))
    (if others
        (let ((selected
               (completing-read
                "Change user to: " others nil t)))
          (maths-set-current-user selected)
          (maths-new))
      (message "There are currently no other users"))))

(defun maths-toggle-auto-continue ()
  "Toggle automatic continuation after an answer."
  (interactive)
  (setq *maths-auto-continue* (not *maths-auto-continue*)))

(defun maths-auto-continue-p ()
  "Return non-nil when automatic continuation is enabled."
  *maths-auto-continue*)

(defun maths--map-table-column (function column table)
  "Apply FUNCTION to COLUMN in each row of TABLE."
  (mapcar
   (lambda (row)
     (let ((copy (copy-sequence row)))
       (setf (nth column copy)
             (funcall function (nth column copy)))
       copy))
   table))

(cl-defun maths-list-task-ratings
    (&optional (buffer-name "*Task Ratings*"))
  "Display task ratings in BUFFER-NAME."
  (interactive)
  (with-output-to-temp-buffer buffer-name
    (princ
     (tab-format
      (maths--map-table-column
       (lambda (task)
         (apply #'maths-format-task task))
       1 (maths-db-task-ratings))
      :header '("ID" "Task" "Rating" "RD" "Updated")
      :column-separator " | "))
    (switch-to-buffer-other-window buffer-name)))

(defun maths-quit ()
  "Save Maths and close its main buffer."
  (interactive)
  (maths-save)
  (kill-buffer +maths-buffer+))

(defun maths--unsuppress-keymap (map keys)
  "Bind each character in KEYS to self-insertion in MAP."
  (cl-loop
   for character across keys
   for key = (char-to-string character)
   do
   (define-key
    map key
    (let ((character character))
      (lambda ()
        (interactive)
        (insert character))))))

(defun maths-plot-user-ratings ()
  "Plot the current user's ratings."
  (interactive)
  (require 'rating-plot)
  (rating-plot-history
   (maths-db-user-ratings (maths-current-user))))

(defun maths--make-mode-map ()
  "Create the keymap for `maths-mode'."
  (let ((map (make-keymap))
        (list-map (make-sparse-keymap))
        (user-map (make-sparse-keymap)))
    (suppress-keymap map)
    (maths--unsuppress-keymap map "+-0123456789")

    (define-key map "l" list-map)
    (define-key list-map "r" #'maths-list-user-ratings)
    (define-key list-map "R" #'maths-list-task-ratings)
    (define-key list-map "p" #'maths-plot-user-ratings)
    (define-key list-map "P" 'maths-plot-task-ratings)

    (define-key map "u" user-map)
    (define-key user-map "n" #'maths-register-new-user)
    (define-key user-map "c" #'maths-change-user)
    (define-key user-map "d" 'maths-delete-user)
    (define-key user-map "s" 'maths-show-user-settings)

    (define-key map "n" #'maths-enter)
    (define-key map "a" #'maths-toggle-auto-continue)
    (define-key map "?" 'maths-help)
    (define-key map "h" 'maths-help)
    (define-key map "q" #'maths-quit)
    (define-key map [return] #'maths-enter)
    (define-key map [kp-enter] #'maths-enter)
    (define-key map [backspace] #'delete-backward-char)
    (define-key map [delete] #'delete-forward-char)
    map))

(defvar maths-mode-map
  (maths--make-mode-map)
  "Keymap for `maths-mode'.")

;;; Mode line

(defun maths-mode-line-user-description ()
  "Return a dynamic mode-line description of the current user."
  '(:eval
    (when-let ((user (maths-current-user)))
      (format " [%s (R:%d RD:%d)]"
              (maths-user-name user)
              (maths-user-rating user)
              (maths-user-RD user)))))

(defun maths-mode-line ()
  "Return `mode-line-format' with Maths user information."
  (let* ((line (copy-tree mode-line-format))
         (index
          (1+ (or
               (cl-position
                'mode-line-buffer-identification line)
               0))))
    (append
     (cl-subseq line 0 (min index (length line)))
     (list (maths-mode-line-user-description))
     (nthcdr index line))))

(defun maths-set-mode-line ()
  "Install Maths user information in the current mode line."
  (setq-local mode-line-format (maths-mode-line)))

(define-derived-mode maths-mode special-mode "Maths"
  "Major mode for the Maths game.
\\{maths-mode-map}"
  (setq buffer-read-only nil)
  (maths-set-mode-line))

(defun maths-init ()
  "Initialize the Maths database."
  (maths-init-database))

(cl-defun maths (&optional (level 1))
  "Start a Maths session at LEVEL."
  (interactive)
  (maths-init)
  (switch-to-buffer +maths-buffer+)
  (maths-mode)
  (maths-new level))

(provide 'maths)

;;; maths.el ends here
