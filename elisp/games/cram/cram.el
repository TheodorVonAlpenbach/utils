(require 'cram-config)
(require 'cram-backend)
(require 'cram-common)

(defun cram-format-task (operation arguments &optional solution)
  (format "%d %s %d = %s" 
    (first arguments)
    (cram-operator-string (cram-operator operation))
    (second arguments)
    (or solution "")))
;;(cram-format-task :addition '(1 1) 2)

(defun cram-format-match (user task)
  (concat
   (format "Your rating: %d (RD = %d)\n" (cram-user-rating user) (cram-user-RD user))
   (format "Task rating: %d (RD = %d)\n" (cram-task-rating task) (cram-task-RD task))
   "\n"
   (format "Problem #%d:\n" (cram-task-id task))
   (cram-format-task (cram-task-operation task) (cram-task-arguments task))))

(cl-defun cram-new (&optional (level 1))
  "Inserts new match, and starts the clocks"
  (let ((user (or (cram-current-user)
		  (cram-register-new-user))))
    (erase-buffer)
    (insert (cram-format-match user (cram-draw-task :rating (cram-user-rating user))))
    (time-set-reference)))

(defun cram-read-answer ()
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

(defun cram-submit ()
  "It's a bit confusing with task-entry in DB, task as is, and
the *cram-current-task* which is yet another structure."
  (let* ((time (time-elapsed nil))
	 (answer (cram-read-answer))
	 (old-ratings (cram-current-ratings)) ;current ratings will become the old
	 (score (cram-report-answer answer time))
	 (solution (cram-task-solution (cram-current-task)))
	 (new-ratings (cram-current-ratings)))
    (if (and (cram-auto-continue-p) (not current-prefix-arg))
      (progn (message (short-judgement score answer solution))
	     (cram-new))
      (insert (long-judgement answer time solution score old-ratings new-ratings)))))

(defun cram-answer-submitted-p ()
  (string-match "Spent" (buffer-string)))

;;; Interactive functions
(defun cram-enter ()
  (interactive)
  (if (cram-answer-submitted-p)
    (cram-new)
    (cram-submit)))

(defun cram-register-new-user ()
  (interactive)
  (let ((user-name (read-from-minibuffer "New user name: "))
	(age (string-to-number (read-from-minibuffer "Age: "))))
    (assert (and (integerp age) (plusp age)) t "Illegal age")
    (when (yes-or-no-p (format "Are you sure that you want to register a new user with name '%s'? " user-name))
      (aif (cram-add-user user-name age)
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
				    :columns (:name :age (round :rating) (round :RD) ::updated)
				    :order-by :rating
				    :order :desc)) 
		       :header '("Name" "Age" "Rating" "RD" "Last update")
		       :column-separator " | "))
    (switch-to-buffer-other-window buffer-name)))
;;(cram-list-user-ratings)

(defun cram-db-task-ratings ()
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

(cl-defun cram-list-task-ratings (&optional (buffer-name "*Task Ratings*"))
  "Prints a table of all tasks with their corresponding rating."
  (interactive)
  (with-output-to-temp-buffer buffer-name
    (princ (tab-format
	    (mapcol (bind #'apply #'cram-format-task 1)
		    1 (cram-db-task-ratings))
	    :header '("ID" "Task" "Rating" "RD" "Updated")
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

(defun cram-mode-map ()
  (let ((map (make-keymap))
	(list-map (make-sparse-keymap))
	(user-map (make-sparse-keymap)))

    (suppress-keymap map)
    (unsuppress-keymap map "+-0123456789")

    ;; Lists
    (define-key map "l" list-map)
    (define-key list-map "r" 'cram-list-user-ratings)
    (define-key list-map "R" 'cram-list-task-ratings)    
    (define-key list-map "p" 'cram-plot-user-ratings)
    (define-key list-map "P" 'cram-plot-task-ratings)

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

(defvar cram-mode-map ;;(nilf cram-mode-map)
  (cram-mode-map)
  "Keymap containing cram commands.")
;;(setf cram-mode-map (cram-mode-map))

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

(define-derived-mode cram-mode special-mode "Cram"
   "Cram mode
\\{cram-mode-map}"
   (setf buffer-read-only nil)
   (cram-set-mode-line))

(cl-defun cram (&optional (level 1))
  (interactive)
  (cram-init)
  (switch-to-buffer +cram-buffer+)
  (cram-mode)
  (cram-new level))
;;(cram)

(provide 'cram)