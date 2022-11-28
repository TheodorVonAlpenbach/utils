(require 'cram-config)
(require 'cram-common)

(defun cram-format-problem (question &optional max-length)
  (format "%s%s"
    (if max-length
      (substring question 0 (min max-length (length question)))
      question)
    (if max-length "" "\n")))
;;(cram-format-problem "qwe" 2)

(defun cram-format-match (user problem)
  (concat
    (format "Your rating: %d (RD = %d)\n"
      (cram-user-rating-e user) (cram-user-rating-d user))
    (format "Problem rating: %d (RD = %d)\n"
      (or (cram-problem-rating-e problem) -1)
      (or (cram-problem-rating-d problem) -1))
    "\n"
    (format "Problem #%d:\n" (cram-problem-id problem))
    (cram-format-problem (cram-problem-question problem))))
;;(cram-format-match (cram-current-user) (first (cram-db-problems)))

(cl-defun cram-new (&optional (level 1))
  "Insert new problem and start the clocks"
  (let ((user (or (cram-current-user)
		  (cram-register-new-user))))
    (erase-buffer)

    (cram-problem-mode)
    (aif (cram-draw-problem :method :cram ; :worst
			    :rating (cram-user-rating user))
      (progn (insert (cram-format-match user it))
	     (awhen (cram-problem-picture it)
	       (find-file-other-window
		(expand-file-name
		 it
		 (concat-directories
		  (ld-database-repository *current-database*) "images")))
	       (other-window 1)))
      (message "Could not draw problem. Is the database initialized?"))
    (time-set-reference)
    (evil-emacs-state)))
;;(cram-draw-problem)

(defun cram-read-response ()
  "Parses the submitted response"
  (string-trim (line-string)))


(defun format-alternatives (alternatives)
  (or alternatives "None."))

(cl-defun short-judgement (score response answer alternatives
				 &optional (correct-color "ForestGreen")
				 (error-color "red"))
  (if (plusp score)
    (format "%s Your score was %.2f\nFull answer: %s\nAlternatives: %s"
      (propertize "Correct!" 'face
		  (list :foreground correct-color :weight 'bold))
      score answer (format-alternatives alternatives))
    (format "%s Correct response is %S\nAlternatives: %S"
      (propertize "Incorrect!" 'face
		  (list :foreground error-color :weight 'bold))
      (string-remove-props answer)
      (format-alternatives alternatives))))

(cl-defun long-judgement (response time answer alternatives
			  score old-ratings new-ratings)
  (let ((diff-ratings (maptree* #'- new-ratings old-ratings)))
    (cl-destructuring-bind ((user-rating user-RD) (problem-rating problem-RD))
	(maptree #'round new-ratings)
      (cl-destructuring-bind ((user-rating-diff user-RD-diff)
			   (problem-rating-diff problem-RD-diff))
	  (maptree #'round diff-ratings)
	(concat
	  "\n\n"
	  (short-judgement score response answer alternatives) "\n"
	  (format "Time spent: %.1f seconds\n" (/ time 1000.0))
	  "\n"
	  (format "Your new rating is %d (%d)\n" user-rating user-rating-diff)
	  (format "Your new RD is %d (%d)\n" user-RD user-RD-diff)
	  (format "Problem's new rating is %d (%d)\n"
	    problem-rating problem-rating-diff)
	  (format "Problem' new RD is %d (%d)\n"
	    problem-RD problem-RD-diff)
	  ;; only in debugging stage
	  ;; (format "\nDebug sorted problem table\n%S"
	  ;;   (cl-sort (cram-db-problems) #'string> :key #'cram-problem-updated))
	  )))))

(defun cram-submit ()
  "It's a bit confusing with problem-entry in DB, problem as is, and
the *cram-current-problem* which is yet another structure."
  (let* ((time (time-elapsed nil))
	 (response (cram-read-response))
	 ; ;current ratings will become the old
	 (old-ratings (cram-current-ratings))
	 (score (cram-report-response response time))
	 (answer (cram-problem-answer (cram-current-problem)))
	 (alternatives(cram-problem-alternatives (cram-current-problem)))
	 (new-ratings (cram-current-ratings)))
    (if (and (cram-auto-continue-p) (not current-prefix-arg))
      (progn (message (short-judgement score response answer alternatives))
	     (cram-new))
      (insert (long-judgement response time answer alternatives score
			      old-ratings new-ratings))))
    (cram-answer-mode)
    (evil-emacs-state))

(defun cram-response-submitted-p ()
  (string-match "Spent" (buffer-string)))

;;; Interactive functions
(defun cram-enter ()
  (interactive)
  (if (eql major-mode 'cram-problem-mode)
    (cram-submit)
    (cram-new)))

(defun cram-register-new-user ()
  (interactive)
  (let ((user-name (read-from-minibuffer "New user name: ")))
    (when (yes-or-no-p
	   (format "Are you sure that you want to register a new user with name '%s'? "
	     user-name))
      (aif (cram-add-user user-name)
	(prog1 it
	  (cram-set-current-user it)
	  (message "User %s registered" user-name))
	(message "Couldn't register user since user name '%s' is already registered" user-name)))))
;;(cram-register-new-user)

(defun cram-change-user ()
  (interactive)
  (let ((others (cram-db-user-names :sans (cram-user-name (cram-current-user)))))
    (if others
      (awhen (completing-read "Change user to: " others)
	(cram-set-current-user it)
	(cram-new))
      (message "There are currently no other users but you!"))))

(defun cram-toggle-auto-continue ()
  (interactive)
  (setf *cram-auto-continue* (not *cram-auto-continue*)))
;;(cram-toggle-auto-continue)

(defun cram-auto-continue-p ()
  *cram-auto-continue*)

(defun cram-help ()
  (interactive)
  (message "Not implemented"))

(defun cram-browse-wikipedia ()
  (interactive)
  (browse-url (format "https://no.wikipedia.org/wiki/%s"
		(cram-problem-answer (cram-current-problem)))))

(defun cram-browse-store-norske ()
  (interactive)
  (browse-url (format "https://snl.no/%s"
		(cram-problem-answer (cram-current-problem)))))


(cl-defun cram-list-last-matches
    (&optional (buffer-name "*Last matches") (max-question-length 20))
  "Prints a table of last matches"
  (interactive)
  (with-output-to-temp-buffer buffer-name
    (princ (tab-format
	    (mapcol (bind #'cram-format-problem max-question-length)
		    0 (cram-db-last-matches))
	    :header '("Timestamp" "Problem" "Answer" "Solution" "Score")
	    :column-separator " | "))
    (switch-to-buffer-other-window buffer-name)))
;;(cram-list-last-matches)

(cl-defun cram-list-problem-ratings
    (&optional (buffer-name "*Problem Ratings*") (max-question-length 70))
  "Prints a table of all problems with their corresponding rating."
  (interactive)
  (with-output-to-temp-buffer buffer-name
    (princ (tab-format
	    (mapcol (bind #'cram-format-problem max-question-length)
		    1 (cram-db-problem-ratings))
	    :header '("ID" "Problem" "Solution" "Rating" "RD" "Updated")
	    :column-separator " | "))
    (switch-to-buffer-other-window buffer-name)))

(defun cram-quit ()
  (interactive)
  (cram-save)
  (kill-buffer +cram-buffer+))

(defun unsuppress-keymap (map keys)
  (cl-loop for char across keys
	   for key = (char-to-string char)
	   do (define-key map key (lexical-let ((char char))
				    #'(lambda () 
					(interactive)
					(insert char))))))
;;(unsuppress-keymap cram-mode-map "0123456789")

(defun cram-plot-user-ratings ()
  (interactive)
  (gnuplot-ratings (cram-db-user-ratings (cram-current-user))))

(defun cram-answer-mode-map ()
  (let ((map (make-keymap))
	(list-map (make-sparse-keymap))
	(user-map (make-sparse-keymap))
	(browse-map (make-sparse-keymap)))

    (suppress-keymap map)
    (unsuppress-keymap map "+-0123456789")

    ;; Lists
    (define-key map "l" list-map)
    (define-key list-map "r" 'cram-list-user-ratings)
    (define-key list-map "R" 'cram-list-problem-ratings)    
    (define-key list-map "p" 'cram-plot-user-ratings)
    (define-key list-map "P" 'cram-plot-problem-ratings)
    (define-key list-map "m" 'cram-list-last-matches)

    ;; User
    (define-key map "u" user-map)
    (define-key user-map "n" 'cram-register-new-user)
    (define-key user-map "c" 'cram-change-user)    
    (define-key user-map "d" 'cram-delete-user)
    (define-key user-map "s" 'cram-show-user-settings)

    (define-key map "n" 'cram-enter)

    (define-key map "a" 'cram-toggle-auto-continue)
    (define-key map "?" 'cram-help)
    (define-key map "h" 'cram-help)
    (define-key map "q" 'cram-quit)

    (define-key map [return] 'cram-enter)
    (define-key map [kp-enter] 'cram-enter)
    (define-key map [backspace] 'delete-backward-char)
    (define-key map [delete] 'delete-forward-char)

    (define-key map "b" browse-map)
    (define-key browse-map "w" 'cram-browse-wikipedia)
    (define-key browse-map "s" 'cram-browse-store-norske)

    map))

(defvar cram-answer-mode-map ;;(nilf cram-answer-mode-map)
  (cram-answer-mode-map)
  "Keymap containing cram commands.")
;;(setf cram-answer-mode-map (cram-answer-mode-map)) 

;;; Mode line
(cl-defun cram-mode-line-user-description ()
  "Adopted the design from `Info-mode' (`Info-set-mode-line')."
  '(:eval (let ((user (cram-current-user)))
	    (format " [%s (R:%d RD:%d)]" 
	      (cram-user-name user)
	      (cram-user-rating-e user)
	      (cram-user-rating-d user)))))
;;(cram-mode-line-user-description)

(cl-defun cram-mode-line ()
  "Adopted the design from `Info-mode' (`Info-set-mode-line')."
  (let* ((line (copy-tree mode-line-format)))
    (list-insert (cram-mode-line-user-description)
		 (1+ (or (position 'mode-line-buffer-identification line) 0))
		 line)
    line))

(cl-defun cram-set-mode-line ()
  "Adopted the design from `Info-mode' (`Info-set-mode-line')."
  (setf mode-line-format (cram-mode-line)))

(define-derived-mode cram-answer-mode special-mode "Cram answer"
   "Cram mode
\\{cram-answer-mode-map}"
   (setf buffer-read-only nil)
   (cram-set-mode-line))

(define-derived-mode cram-problem-mode text-mode "Cram problem"
  "Cram problem mode, same as text mode, except return submits the response.
\\{cram-problem-mode-map}"
  (setf buffer-read-only nil)
  (cram-set-mode-line)
  (evil-emacs-state)
  (define-key cram-problem-mode-map [return] #'cram-enter))

(defun cram-init (&optional force)  
  (setf *cram-last-update* (the-creation))
  (setf *cram-match-cache* nil)
  (require 'cram-db)
  (cram-init-database force)
  (require 'cram-backend))

(cl-defun cram-1 (filter &optional (level 1))
  (setf *cram-ref-filter* filter)
  (cram-init t)
  (switch-to-buffer +cram-buffer+)
  (cram-new level))

(cl-defun cram (&optional (level 1))
  (interactive)
  (cram-1 "^sk-ref-[3]"))
;;(cram)

(cl-defun serbo-cram (&optional (level 1))
  (interactive)
  (cram-1 "^sk-ref-[0-9]"))
(defalias 'cram-serbo 'serbo-cram)

(cl-defun quiz-cram (&optional (level 1))
  (interactive)
  (cram-1 "^csv-ref-[1]"))
(defalias 'cram-quiz 'quiz-cram)

(cl-defun quiz-plants (&optional (level 1))
  (interactive)
  (cram-1 "^csv-planter-"))

(cl-defun quiz-norske-fugler (&optional (level 1))
  (interactive)
  (cram-1 "^csv-norske-fugler-"))

(provide 'cram)
