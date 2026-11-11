;;; cram.el --- Interactive Cram interface -*- lexical-binding: t; -*-

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'subr-x)
(require 'url-util)
(require 'cram-backend)
(require 'cram-config)
(require 'cram-common)
(require 'mb-utils-strings)
(require 'mb-utils-time)

(declare-function evil-emacs-state "evil")
(declare-function gnuplot-ratings "maths-plot")

(defun cram--use-emacs-state ()
  "Enter Evil's Emacs state when Evil is available."
  (when (fboundp 'evil-emacs-state)
    (evil-emacs-state)))

(defun cram-format-problem (question &optional max-length)
  "Format QUESTION, truncating it to MAX-LENGTH when supplied."
  (concat
   (if max-length
       (substring question 0 (min max-length (length question)))
     question)
   (if max-length "" "\n")))

(defun cram-format-match (user problem)
  "Format USER and PROBLEM for presentation."
  (concat
   (format "Your rating: %d (RD = %d)\n"
           (cram-user-rating-e user)
           (cram-user-rating-d user))
   (format "Problem rating: %d (RD = %d)\n"
           (or (cram-problem-rating-e problem) -1)
           (or (cram-problem-rating-d problem) -1))
   "\n"
   (format "Problem #%d:\n" (cram-problem-id problem))
   (cram-format-problem (cram-problem-question problem))))

(cl-defun cram-new (&optional (level 1))
  "Draw and display a new problem.
LEVEL is retained for compatibility."
  (ignore level)
  (if-let ((user (or (cram-current-user)
                     (cram-register-new-user))))
      (progn
        (erase-buffer)
        (cram-problem-mode)
        (if-let ((problem
                  (cram-draw-problem
                   :method :cram
                   :rating (cram-user-rating user))))
            (progn
              (insert (cram-format-match user problem))
              (when-let ((picture (cram-problem-picture problem)))
                (find-file-other-window
                 (expand-file-name
                  picture
                  (expand-file-name
                   "images"
                   (ld-database-repository *current-database*))))
                (other-window 1)))
          (message
           "Could not draw problem; is the database initialized?"))
        (time-set-reference)
        (cram--use-emacs-state))
    (message "No Cram user is selected")))

(defun cram-read-response ()
  "Return the current line as a trimmed response."
  (string-trim
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun cram-format-alternatives (alternatives)
  "Format ALTERNATIVES for a judgement."
  (or alternatives "None."))

(cl-defun cram-short-judgement
    (score response answer alternatives
           &optional
           (correct-color "ForestGreen")
           (error-color "red"))
  "Format a short judgement for SCORE and RESPONSE.
ANSWER and ALTERNATIVES describe the accepted solution."
  (ignore response)
  (if (> score 0)
      (format
       "%s Your score was %.2f\nFull answer: %s\nAlternatives: %s"
       (propertize
        "Correct!" 'face
        (list :foreground correct-color :weight 'bold))
       score answer (cram-format-alternatives alternatives))
    (format
     "%s Correct response is %S\nAlternatives: %S"
     (propertize
      "Incorrect!" 'face
      (list :foreground error-color :weight 'bold))
     (substring-no-properties answer)
     (cram-format-alternatives alternatives))))

(defun cram--rating-differences (new-ratings old-ratings)
  "Subtract OLD-RATINGS from NEW-RATINGS componentwise."
  (cl-mapcar
   (lambda (new old)
     (cl-mapcar #'- new old))
   new-ratings old-ratings))

(defun cram-long-judgement
    (response time answer alternatives score old-ratings new-ratings)
  "Format a detailed judgement and rating change summary."
  (let ((differences
         (cram--rating-differences new-ratings old-ratings)))
    (cl-destructuring-bind
        ((user-rating user-RD) (problem-rating problem-RD))
        (mapcar
         (lambda (rating) (mapcar #'round rating))
         new-ratings)
      (cl-destructuring-bind
          ((user-rating-diff user-RD-diff)
           (problem-rating-diff problem-RD-diff))
          (mapcar
           (lambda (rating) (mapcar #'round rating))
           differences)
        (concat
         "\n\n"
         (cram-short-judgement
          score response answer alternatives)
         "\n"
         (format "Time spent: %.1f seconds\n" (/ time 1000.0))
         "\n"
         (format "Your new rating is %d (%d)\n"
                 user-rating user-rating-diff)
         (format "Your new RD is %d (%d)\n"
                 user-RD user-RD-diff)
         (format "Problem's new rating is %d (%d)\n"
                 problem-rating problem-rating-diff)
         (format "Problem's new RD is %d (%d)\n"
                 problem-RD problem-RD-diff))))))

(defun cram-submit ()
  "Submit the current response and display its judgement."
  (let* ((time (time-elapsed nil))
         (response (cram-read-response))
         (old-ratings (cram-current-ratings))
         (score (cram-report-response response time))
         (problem (cram-current-problem))
         (answer (cram-problem-answer problem))
         (alternatives (cram-problem-alternatives problem))
         (new-ratings (cram-current-ratings)))
    (if (and (cram-auto-continue-p)
             (not current-prefix-arg))
        (progn
          (message
           "%s"
           (cram-short-judgement
            score response answer alternatives))
          (cram-new))
      (insert
       (cram-long-judgement
        response time answer alternatives score
        old-ratings new-ratings))))
  (cram-answer-mode)
  (cram--use-emacs-state))

(defun cram-response-submitted-p ()
  "Return non-nil when the current buffer contains a judgement."
  (string-match-p "Time spent:" (buffer-string)))

;;; Interactive commands

(defun cram-enter ()
  "Submit a response or draw the next problem."
  (interactive)
  (if (eq major-mode 'cram-problem-mode)
      (cram-submit)
    (cram-new)))

(defun cram-register-new-user ()
  "Prompt for and register a new Cram user."
  (interactive)
  (let ((user-name (read-from-minibuffer "New user name: ")))
    (when
        (yes-or-no-p
         (format
          "Are you sure that you want to register a new user named %S? "
          user-name))
      (if (cram-db-get-user user-name)
          (progn
            (message "User name %S is already registered" user-name)
            nil)
        (let ((user (cram-add-user user-name)))
          (cram-set-current-user user)
          (message "User %s registered" user-name)
          user)))))

(defun cram-change-user ()
  "Prompt for a different current user."
  (interactive)
  (let ((others
         (cram-db-user-names
          :sans (cram-user-name (cram-current-user)))))
    (if others
        (let ((selected
               (completing-read
                "Change user to: " others nil t)))
          (cram-set-current-user selected)
          (cram-new))
      (message "There are currently no other users"))))

(defun cram-toggle-auto-continue ()
  "Toggle automatic continuation after a response."
  (interactive)
  (setq *cram-auto-continue* (not *cram-auto-continue*)))

(defun cram-auto-continue-p ()
  "Return non-nil when automatic continuation is enabled."
  *cram-auto-continue*)

(defun cram-help ()
  "Report that Cram help is not implemented."
  (interactive)
  (message "Not implemented"))

(defun cram-browse-wikipedia ()
  "Open the current answer on Norwegian Wikipedia."
  (interactive)
  (browse-url
   (format
    "https://no.wikipedia.org/wiki/%s"
    (url-hexify-string
     (cram-problem-answer (cram-current-problem))))))

(defun cram-browse-store-norske ()
  "Open the current answer in Store norske leksikon."
  (interactive)
  (browse-url
   (format
    "https://snl.no/%s"
    (url-hexify-string
     (cram-problem-answer (cram-current-problem))))))

(defun cram--map-table-column (function column table)
  "Apply FUNCTION to COLUMN in each row of TABLE."
  (mapcar
   (lambda (row)
     (let ((copy (copy-sequence row)))
       (setf (nth column copy)
             (funcall function (nth column copy)))
       copy))
   table))

(cl-defun cram-list-last-matches
    (&optional
     (buffer-name "*Last matches*")
     (max-question-length 20))
  "Display a table of recent matches in BUFFER-NAME."
  (interactive)
  (with-output-to-temp-buffer buffer-name
    (princ
     (tab-format
      (cram--map-table-column
       (lambda (question)
         (cram-format-problem question max-question-length))
       0 (cram-db-last-matches))
      :header '("Timestamp" "Problem" "Answer" "Solution" "Score")
      :column-separator " | "))
    (switch-to-buffer-other-window buffer-name)))

(cl-defun cram-list-problem-ratings
    (&optional
     (buffer-name "*Problem Ratings*")
     (max-question-length 70))
  "Display problem ratings in BUFFER-NAME."
  (interactive)
  (with-output-to-temp-buffer buffer-name
    (princ
     (tab-format
      (cram--map-table-column
       (lambda (question)
         (cram-format-problem question max-question-length))
       1 (cram-db-problem-ratings))
      :header '("ID" "Problem" "Solution" "Rating" "RD" "Updated")
      :column-separator " | "))
    (switch-to-buffer-other-window buffer-name)))

(defun cram-quit ()
  "Save Cram and close its main buffer."
  (interactive)
  (cram-save)
  (kill-buffer +cram-buffer+))

(defun cram--unsuppress-keymap (map keys)
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

(defun cram-plot-user-ratings ()
  "Plot the current user's ratings."
  (interactive)
  (require 'maths-plot)
  (gnuplot-ratings
   (cram-db-user-ratings (cram-current-user))))

(defun cram--make-answer-mode-map ()
  "Create the keymap for `cram-answer-mode'."
  (let ((map (make-keymap))
        (list-map (make-sparse-keymap))
        (user-map (make-sparse-keymap))
        (browse-map (make-sparse-keymap)))
    (suppress-keymap map)
    (cram--unsuppress-keymap map "+-0123456789")

    (define-key map "l" list-map)
    (define-key list-map "r" #'cram-list-user-ratings)
    (define-key list-map "R" #'cram-list-problem-ratings)
    (define-key list-map "p" #'cram-plot-user-ratings)
    (define-key list-map "P" 'cram-plot-problem-ratings)
    (define-key list-map "m" #'cram-list-last-matches)

    (define-key map "u" user-map)
    (define-key user-map "n" #'cram-register-new-user)
    (define-key user-map "c" #'cram-change-user)
    (define-key user-map "d" 'cram-delete-user)
    (define-key user-map "s" 'cram-show-user-settings)

    (define-key map "n" #'cram-enter)
    (define-key map "a" #'cram-toggle-auto-continue)
    (define-key map "?" #'cram-help)
    (define-key map "h" #'cram-help)
    (define-key map "q" #'cram-quit)
    (define-key map [return] #'cram-enter)
    (define-key map [kp-enter] #'cram-enter)
    (define-key map [backspace] #'delete-backward-char)
    (define-key map [delete] #'delete-forward-char)

    (define-key map "b" browse-map)
    (define-key browse-map "w" #'cram-browse-wikipedia)
    (define-key browse-map "s" #'cram-browse-store-norske)
    map))

(defvar cram-answer-mode-map
  (cram--make-answer-mode-map)
  "Keymap for `cram-answer-mode'.")

;;; Mode line

(defun cram-mode-line-user-description ()
  "Return a dynamic mode-line description of the current user."
  '(:eval
    (when-let ((user (cram-current-user)))
      (format " [%s (R:%d RD:%d)]"
              (cram-user-name user)
              (cram-user-rating-e user)
              (cram-user-rating-d user)))))

(defun cram-mode-line ()
  "Return `mode-line-format' with Cram user information."
  (let* ((line (copy-tree mode-line-format))
         (index
          (1+ (or
               (cl-position
                'mode-line-buffer-identification line)
               0))))
    (append
     (cl-subseq line 0 (min index (length line)))
     (list (cram-mode-line-user-description))
     (nthcdr index line))))

(defun cram-set-mode-line ()
  "Install Cram's user information in the current mode line."
  (setq-local mode-line-format (cram-mode-line)))

(define-derived-mode cram-answer-mode special-mode "Cram answer"
  "Major mode for a submitted Cram answer.
\\{cram-answer-mode-map}"
  (setq buffer-read-only nil)
  (cram-set-mode-line))

(define-derived-mode cram-problem-mode text-mode "Cram problem"
  "Major mode for entering a Cram response.
\\{cram-problem-mode-map}"
  (setq buffer-read-only nil)
  (cram-set-mode-line)
  (cram--use-emacs-state))

(define-key cram-problem-mode-map [return] #'cram-enter)

(defun cram-init (&optional force)
  "Reset Cram caches and initialize its database.
Pass FORCE to `cram-init-database'."
  (setq *cram-last-update* (the-creation)
        *cram-match-cache* nil)
  (cram-init-database force))

(cl-defun cram-1 (filter &optional (level 1))
  "Start Cram with source FILTER and initial LEVEL."
  (setq *cram-ref-filter* filter)
  (cram-init t)
  (switch-to-buffer +cram-buffer+)
  (cram-new level))

(cl-defun cram (&optional (level 1))
  "Start the default Cram session at LEVEL."
  (interactive)
  (cram-1 "^sk-ref-[3]" level))

(cl-defun serbo-cram (&optional (level 1))
  "Start the Serbian/Croatian Cram session at LEVEL."
  (interactive)
  (cram-1 "^sk-ref-[0-9]" level))

(defalias 'cram-serbo #'serbo-cram)

(cl-defun quiz-cram (&optional (level 1))
  "Start the general quiz Cram session at LEVEL."
  (interactive)
  (cram-1 "^csv-ref-[1]" level))

(defalias 'cram-quiz #'quiz-cram)

(cl-defun quiz-plants (&optional (level 1))
  "Start the plant quiz at LEVEL."
  (interactive)
  (cram-1 "^csv-planter-" level))

(cl-defun quiz-norske-fugler (&optional (level 1))
  "Start the Norwegian bird quiz at LEVEL."
  (interactive)
  (cram-1 "^csv-norske-fugler-" level))

(provide 'cram)

;;; cram.el ends here
