(require 'maths-config)
(require 'maths-backend)
(require 'maths-common)

(defun maths-operator-string (operator)
  (symbol-name operator))
;;(mapcar #'maths-operator-string '(:addition :substraction :multiplication :division))

(defun maths-format-task (operation arguments &optional solution)
  (format "%d %s %d = %s" 
    (first arguments)
    (maths-operator-string (maths-operator operation))
    (second arguments)
    (or solution "")))
;;(maths-format-task :addition '(1 1) 2)

(defun maths-format-match (user task)
  (concat
   (format "Your rating: %d (RD = %d)\n" (maths-user-rating user) (maths-user-RD user))
   (format "Task rating: %d (RD = %d)\n" (maths-task-rating task) (maths-task-RD task))
   "\n"
   (format "Problem #%d:\n" (maths-task-id task))
   (maths-format-task (maths-task-operation task) (maths-task-arguments task))))

(cl-defun maths-new (&optional (level 1))
  "Inserts new match, and starts the clocks"
  (let ((user (or (maths-current-user)
		  (maths-register-new-user))))
    (erase-buffer)
    (insert (maths-format-match user (maths-draw-task :rating (maths-user-rating user))))
    (time-set-reference)))

(defun maths-read-answer ()
  "Parses the submitted answer"
  (save-excursion
    (backward-char 1)
    (number-at-point)))

(cl-defun short-judgement (score answer solution &optional (correct-color "ForestGreen") (error-color "red"))
  (if (= answer solution)
    (format "%s Your score was %.2f" (propertize "Correct!" 'face (list :foreground correct-color :weight 'bold)) score)
    (format "%s Correct answer is %S" (propertize "Incorrect!" 'face (list :foreground error-color :weight 'bold)) solution)))

(cl-defun long-judgement (answer time solution score old-ratings new-ratings)
  (let ((diff-ratings (maptree* #'- new-ratings old-ratings)))
    (destructuring-bind ((user-rating user-RD) (task-rating task-RD))
	(maptree #'round new-ratings)
      (destructuring-bind ((user-rating-diff user-RD-diff) (task-rating-diff task-RD-diff))
	  (maptree #'round diff-ratings)
	(concat "\n\n"
		(short-judgement score answer solution) "\n"
		(format "Time spent: %.1f seconds\n" (/ time 1000.0))
		"\n"
		(format "Your new rating is %d (%d)\n" user-rating user-rating-diff)
		(format "Your new RD is %d (%d)\n" user-RD user-RD-diff)
		(format "Task's new rating is %d (%d)\n" task-rating task-rating-diff)
		(format "Task' new RD is %d (%d)\n" task-RD task-RD-diff))))))

(defun maths-submit ()
  "It's a bit confusing with task-entry in DB, task as is, and
the *maths-current-task* which is yet another structure."
  (let* ((time (time-elapsed nil))
	 (answer (maths-read-answer))
	 (old-ratings (maths-current-ratings)) ;current ratings will become the old
	 (score (maths-report-answer answer time))
	 (solution (maths-task-solution (maths-current-task)))
	 (new-ratings (maths-current-ratings)))
    (if (and (maths-auto-continue-p) (not current-prefix-arg))
      (progn (message (short-judgement score answer solution))
	     (maths-new))
      (insert (long-judgement answer time solution score old-ratings new-ratings)))))

(defun maths-answer-submitted-p ()
  (string-match "Spent" (buffer-string)))

;;; Interactive functions
(defun maths-enter ()
  (interactive)
  (if (maths-answer-submitted-p)
    (maths-new)
    (maths-submit)))

(defun maths-register-new-user ()
  (interactive)
  (let ((user-name (read-from-minibuffer "New user name: "))
	(age (string-to-number (read-from-minibuffer "Age: "))))
    (assert (and (integerp age) (plusp age)) t "Illegal age")
    (when (yes-or-no-p (format "Are you sure that you want to register a new user with name '%s'? " user-name))
      (aif (maths-add-user user-name age)
	(prog1 it
	  (maths-set-current-user it)
	  (message "User %s registered" user-name))
	(message "Couldn't register user since user name '%s' is already registered" user-name)))))
;;(maths-register-new-user)

(defun maths-change-user ()
  (interactive)
  (let ((others (maths-db-user-names :sans (maths-user-name (maths-current-user)))))
    (if others
      (awhen (completing-read "Change user to: " others)
	(maths-set-current-user it)
	(maths-new))
      (message "There are currently no other users but you!"))))

(defun maths-toggle-auto-continue ()
  (interactive)
  (setf *maths-auto-continue* (not *maths-auto-continue*)))
;;(maths-toggle-auto-continue)

(defun maths-auto-continue-p ()
  *maths-auto-continue*)

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
(cl-defun maths-list-user-ratings (&optional (buffer-name "*User Ratings*"))
  (interactive)
  (with-output-to-temp-buffer buffer-name
    (princ (tab-format (eval-when (load eval)
			 (ld-select :users 
				    :columns (:name :age (round :rating) (round :RD) ::updated)
				    :order-by :rating
				    :order :desc)) 
		       :header '("Name" "Age" "Rating" "RD" "Last update")
		       :column-separator " | "))
    (switch-to-buffer-other-window buffer-name)))
;;(maths-list-user-ratings)

(defun maths-db-task-ratings ()
  "Returns a tree of task ratings"
  (eval-when (load eval)
    (ld-select :tasks
	       :columns (:id
			 (list :operation :arguments :solution)
			 (round :rating)
			 (round :RD)
			 ::updated)
	       :order-by :rating
	       :order :desc)))

(cl-defun maths-list-task-ratings (&optional (buffer-name "*Task Ratings*"))
  "Prints a table of all tasks with their corresponding rating."
  (interactive)
  (with-output-to-temp-buffer buffer-name
    (princ (tab-format
	    (mapcol (bind #'apply #'maths-format-task 1)
		    1 (maths-db-task-ratings))
	    :header '("ID" "Task" "Rating" "RD" "Updated")
	    :column-separator " | "))
    (switch-to-buffer-other-window buffer-name)))

(defun maths-quit ()
  (interactive)
  (maths-save)
  (kill-buffer +maths-buffer+))

(defun unsuppress-keymap (map keys)
  (loop for char across keys
	for key = (char-to-string char)
	do (define-key map key (lexical-let ((char char))
				 #'(lambda () 
				     (interactive)
				     (insert char))))))
;;(unsuppress-keymap maths-mode-map "0123456789")

(defun maths-plot-user-ratings ()
  (interactive)
  (gnuplot-ratings (maths-db-user-ratings (maths-current-user))))

(defun maths-mode-map ()
  (let ((map (make-keymap))
	(list-map (make-sparse-keymap))
	(user-map (make-sparse-keymap)))

    (suppress-keymap map)
    (unsuppress-keymap map "+-0123456789")

    ;; Lists
    (define-key map "l" list-map)
    (define-key list-map "r" 'maths-list-user-ratings)
    (define-key list-map "R" 'maths-list-task-ratings)    
    (define-key list-map "p" 'maths-plot-user-ratings)
    (define-key list-map "P" 'maths-plot-task-ratings)

    ;; User
    (define-key map "u" user-map)
    (define-key user-map "n" 'maths-register-new-user)
    (define-key user-map "c" 'maths-change-user)    
    (define-key user-map "d" 'maths-delete-user)
    (define-key user-map "s" 'maths-show-user-settings)

    (define-key map "n" 'maths-enter)

    (define-key map "a" 'maths-toggle-auto-continue)
    (define-key map "?" 'maths-help)
    (define-key map "h" 'maths-help)
    (define-key map "q" 'maths-quit)

    (define-key map [return] 'maths-enter)
    (define-key map [kp-enter] 'maths-enter)
    (define-key map [backspace] 'delete-backward-char)
    (define-key map [delete] 'delete-forward-char)
    map))

(defvar maths-mode-map ;;(nilf maths-mode-map)
  (maths-mode-map)
  "Keymap containing maths commands.")
;;(setf maths-mode-map (maths-mode-map))

;;; Mode line
(cl-defun maths-mode-line-user-description ()
  "Adopted the design from `Info-mode' (`Info-set-mode-line')."
  '(:eval (let ((user (maths-current-user)))
	    (format " [%s (R:%d RD:%d)]" 
	      (maths-user-name user)
	      (maths-user-rating user)
	      (maths-user-RD user)))))
;;(maths-mode-line-user-description)

(cl-defun maths-mode-line ()
  "Adopted the design from `Info-mode' (`Info-set-mode-line')."
  (let* ((line (copy-tree mode-line-format)))
    (list-insert (maths-mode-line-user-description)
		 (1+ (position 'mode-line-buffer-identification line))
		 line)
    line))

(cl-defun maths-set-mode-line ()
  "Adopted the design from `Info-mode' (`Info-set-mode-line')."
  (setf mode-line-format (maths-mode-line)))

(define-derived-mode maths-mode special-mode "Maths"
   "Maths mode
\\{maths-mode-map}"
   (setf buffer-read-only nil)
   (maths-set-mode-line))

(cl-defun maths (&optional (level 1))
  (interactive)
  (maths-init)
  (switch-to-buffer +maths-buffer+)
  (maths-mode)
  (maths-new level))
;;(maths)

(provide 'maths)
