(require 'cram-config)
(require 'cram-backend)
(require 'cram-common)

(defun cram-format-problem (question &optional answer)
  (format "%s\n" question))
;;(cram-format-problem "qwe")

(defun cram-format-match (user problem)
  (concat
   (format "Your rating: %d (RD = %d)\n"
     (cram-user-rating-e user) (cram-user-rating-d user))
   (format "Problem rating: %d (RD = %d)\n"
     (cram-problem-rating-e problem) (cram-problem-rating-d problem))
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
    (insert (cram-format-match
	     user
	     (cram-draw-problem :method :worst
				:rating (cram-user-rating user))))
    (time-set-reference)
    (evil-emacs-state)))
;;(cram-draw-problem)

(defun cram-read-response ()
  "Parses the submitted response"
  (string-trim (line-string)))

(cl-defun short-judgement (score response answer
				 &optional (correct-color "ForestGreen")
				 (error-color "red"))
  (if (equal response answer)
    (format "%s Your score was %.2f"
      (propertize "Correct!" 'face
		  (list :foreground correct-color :weight 'bold)) score)
    (format "%s Correct response is %S"
      (propertize "Incorrect!" 'face
		  (list :foreground error-color :weight 'bold)) answer)))

(cl-defun long-judgement (response time answer score old-ratings new-ratings)
  (let ((diff-ratings (maptree* #'- new-ratings old-ratings)))
    (destructuring-bind ((user-rating user-RD) (problem-rating problem-RD))
	(maptree #'round new-ratings)
      (destructuring-bind ((user-rating-diff user-RD-diff)
			   (problem-rating-diff problem-RD-diff))
	  (maptree #'round diff-ratings)
	(concat
	  "\n\n"
	  (short-judgement score response answer) "\n"
	  (format "Time spent: %.1f seconds\n" (/ time 1000.0))
	  "\n"
	  (format "Your new rating is %d (%d)\n" user-rating user-rating-diff)
	  (format "Your new RD is %d (%d)\n" user-RD user-RD-diff)
	  (format "Problem's new rating is %d (%d)\n"
	    problem-rating problem-rating-diff)
	  (format "Problem' new RD is %d (%d)\n"
	    problem-RD problem-RD-diff))))))

(defun cram-submit ()
  "It's a bit confusing with problem-entry in DB, problem as is, and
the *cram-current-problem* which is yet another structure."
  (let* ((time (time-elapsed nil))
	 (response (cram-read-response))
	 ; ;current ratings will become the old
	 (old-ratings (cram-current-ratings))
	 (score (cram-report-response response time))
	 (answer (cram-problem-answer (cram-current-problem)))
	 (new-ratings (cram-current-ratings)))
    (if (and (cram-auto-continue-p) (not current-prefix-arg))
      (progn (message (short-judgement score response answer))
	     (cram-new))
      (insert (long-judgement response time answer score
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

;;; tab format. TODO move this somewhere else
(cl-defun tab-column-type (column)
  (cond
    ((every #'integerp column) 'integer)
    ((and (every #'numberp column)
	  (some #'floatp column)) 'float)
    ((every #'stringp column) 'string)))
;;(tab-column-type '("qe" "qe"))

(cl-defun tab-column-width (column &optional (type (tab-column-type column)))
  (let ((c (case type 
	     (string column)
	     (integer (mapcar #'number-to-string column)))))
    (apply #'max (mapcar #'length c))))
;;(tab-column-width '(1 123))

(defun tab-flag (width &optional type)
  (format "%%%s%ds" (if (eql type 'integer) "" "-") width))

(cl-defun tab-control-string (widths &key (type 'string) (separator " "))
  (let ((types (if (atom type) (make-list (length widths) type) type)))
    (concat* (mapcar* #'tab-flag widths types) :in separator)))
;;(tab-control-string '(4 5 1) :type '(integer integer string))

(cl-defun tab-format (string-table &key header (column-separator " ") (underline-char ?=))
  (let ((first-row (first string-table)))
    (when header
      (assert (and (= (length first-row) (length header))
		   (eql (tab-column-type header) 'string))))
    (let* ((columns (transpose string-table))
	   (types (mapcar #'tab-column-type columns))
	   (cwidths (mapcar* #'tab-column-width columns types))
	   (hwidths (and header (mapcar #'length header)))
	   (widths (if header (mapcar* #'max cwidths hwidths) cwidths))
	   (header (if header (concat (apply #'format (tab-control-string widths :separator column-separator) header) "\n") "")))
      (concat* string-table
	:pre (if underline-char (format "%s%s\n" header (make-string (length header) underline-char)) header)
	:key #'(lambda (x) (apply #'format (tab-control-string widths :type types :separator column-separator) x))
	:in "\n"))))
;;(insert (tab-format '(("foo" 1 "bar") ("qwe" 1233456 "qwebar")) :header '("qwe" "ewq" "qwebar")))
;;(tab-format '((1 "bar") (1233456 "qwebar")) :header '("numb" "string2") :column-separator "|")

;;; Stats
(cl-defun cram-list-user-ratings (&optional (buffer-name "*User Ratings*"))
  (interactive)
  (with-output-to-temp-buffer buffer-name
    (princ (tab-format (eval-when (load eval)
			 (ld-select :users 
				    :columns (:name (round :rating) (round :RD) ::updated)
				    :order-by :rating
				    :order :desc)) 
		       :header '("Name" "Rating" "RD" "Last update")
		       :column-separator " | "))
    (switch-to-buffer-other-window buffer-name)))
;;(cram-list-user-ratings)

(defun cram-db-problem-ratings ()
  "Returns a tree of problem ratings"
  (eval-when (load eval)
    (cl-sort (ld-select :problem
			:columns (:id :question :answer :rating ::updated))
      #'> :key #'first)))
;;(cram-db-problem-ratings)

(cl-defun cram-list-problem-ratings (&optional (buffer-name "*Problem Ratings*"))
  "Prints a table of all problems with their corresponding rating."
  (interactive)
  (with-output-to-temp-buffer buffer-name
    (princ (tab-format
	    (mapcol (bind #'apply #'cram-format-problem 1)
		    1 (cram-db-problem-ratings))
	    :header '("ID" "Problem" "Rating" "RD" "Updated")
	    :column-separator " | "))
    (switch-to-buffer-other-window buffer-name)))

(defun cram-quit ()
  (interactive)
  (cram-save)
  (kill-buffer +cram-buffer+))

(defun unsuppress-keymap (map keys)
  (loop for char across keys
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
	(user-map (make-sparse-keymap)))

    (suppress-keymap map)
    (unsuppress-keymap map "+-0123456789")

    ;; Lists
    (define-key map "l" list-map)
    (define-key list-map "r" 'cram-list-user-ratings)
    (define-key list-map "R" 'cram-list-problem-ratings)    
    (define-key list-map "p" 'cram-plot-user-ratings)
    (define-key list-map "P" 'cram-plot-problem-ratings)

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
	      (cram-user-rating user)
	      (cram-user-RD user)))))
;;(cram-mode-line-user-description)

(cl-defun cram-mode-line ()
  "Adopted the design from `Info-mode' (`Info-set-mode-line')."
  (let* ((line (copy-tree mode-line-format)))
    (list-insert (cram-mode-line-user-description)
		 (1+ (position 'mode-line-buffer-identification line))
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

(cl-defun cram (&optional (level 1))
  (interactive)
  (cram-init t)
  (switch-to-buffer +cram-buffer+)
  (cram-new level))
;;(cram)

(provide 'cram)
