(defconst +default-number-of-pegs+ 4)
(defconst +number-of-pegs+ +default-number-of-pegs+)
(defvar *current-fasit* nil)

(defconst +colors+
  '((:red ?r)
    (:yellow ?y)
    (:limegreen ?g)
    (:blue ?c)
    (:black ?b)
    (:white ?w))
  "Each color is defined as a pair (COLOR-KEYWORD COLOR-CHARACTER)")

(cl-defun peg-char->color (char-or-string &optional (colors +colors+))
  (find (if (stringp char-or-string) (char char-or-string 0) char-or-string)
	colors :key #'second))
;;(mapcar #'peg-char->color "rygcbw")

(defun draw-list (sequence n &optional with-redrawal-p)
  (if with-redrawal-p
    (loop repeat n collect (elt sequence (random (length sequence))))
    (loop with seq = (copy-sequence sequence)
	  repeat n collect (draw-random seq))))
;;(sort (draw-list '(1 2 3 4 5 6 7 8 9) 9 t) #'<)

(cl-defun draw-pegs (&optional (n +number-of-pegs+) (allow-same-color-p t))
  (draw-list (copy-tree +colors+) n allow-same-color-p))
;;(draw-pegs 4 t)

(defun count-whites (row fasit)
  "Assumes that the 'black' positions have been removed"
  (loop for r in row
	for pos = (position r fasit :test #'equal)
	if pos do (draw-nth pos fasit) and sum 1))
;;(count-whites '(2 1 3 4) '(1 5 1 2))

(defun check-peg-row (row fasit)
  "Returns the number of black pegs and white pegs for a ROW in
comparison with the current mastermind FASIT"
  (let ((n (length row)))
    (destructuring-bind (row* fasit*)
	(filter-duplicates row fasit :test #'equal)
      (list (- n (length row*))
	    (count-whites row* fasit*)))))
;;(check-peg-row '(2 1 3 4) '(0 5 1 3))

(defun read-pegs ()
  "Parses the current peg row."
  (save-excursion
    (move-beginning-of-line 1)
    (mapcar #'peg-char->color
	    (loop repeat +number-of-pegs+ collect (symbol-name (read (current-buffer)))))))

(defun insert-judgement (judgement)
  "JUDGEMENT is a list (blacks whites)"
  (move-beginning-of-line 1)
  (forward-sexp +number-of-pegs+)
  (kill-line 1)
  (insert (format "  %d %d" (first judgement) (second judgement))))
;;(insert-judgement '(1 1))

(defun mm-solution-found-p (judgement)
  (= +number-of-pegs+ (first judgement)))

(defun judge-peg-row ()
  (interactive)
  (if *current-fasit*
    (let* ((row (read-pegs))
	   (judgment (check-peg-row row *current-fasit*)))
      (insert-judgement judgment)
      (if (mm-solution-found-p judgment)
	(insert "\n\nCongratulations, you solved the problem! Press 'n' for starting a new game, or 'q' to quit Mastermind.")))
    (message "Error! no current fasit is available")))

(defun mastermind-buffer ()
  (get-buffer-create "*Mastermind*"))
;;(mastermind-buffer)

(defun mm-clear-board ()
  (kill-region (point-min) (point-max)))

(defun mm-submit-row ()
  (interactive)
  (judge-peg-row)
  (newline))

(cl-defun mm-insert-instructions (allow-same-color-p)
  (insert (format "Play Mastermind. Insert %d pegs from the following colors:\n" +number-of-pegs+))
  (insert-all-pegs)
  (newline)
  (insert "Type the letter in the peg symbol to insert a peg. Press Enter to view the result. The number of correct placed pegs is followed by the number of present, though incorrect placed, pegs. ")
  (insert (format "In this game, the solution %s duplicate colors." (if allow-same-color-p "contains" "does not contain")))
  (newline)
  (insert "\n"))

(cl-defun mm-new (&optional (force-new-game-p nil) (show-instructions-p t))
  (interactive)
  (when (or force-new-game-p
	    (yes-or-no-p "Do you want to quit this game? "))
    (let* ((legal-prefix (when (integerp current-prefix-arg) current-prefix-arg))
	   (allow-same-color-p (and legal-prefix (minusp legal-prefix))))
      (if (and legal-prefix 
	       (not (zerop legal-prefix)))
	(setf +number-of-pegs+ (abs legal-prefix)))
      (mm-clear-board)
      (when show-instructions-p (mm-insert-instructions allow-same-color-p))
      (setf *current-fasit* (draw-pegs +number-of-pegs+ allow-same-color-p)))))

(defun mm-quit ()
  (interactive)
  (kill-buffer (mastermind-buffer)))

(defun char->color (color-char)
  (first (find color-char +colors+ :key #'second)))
;;(char->color ?b)

(cl-defun insert-peg (color-char &optional (color (char->color color-char)))
  "Inserts peg with color corresponding to COLOR-CHAR"
  (insert (propertize (char-to-string color-char)
		      'face (list :foreground (invert-color color)
				  :background (keyword-name color)
				  :box "black")))
  (insert " "))
;;(insert-peg ?r)

(cl-defun insert-all-pegs (&optional (colors +colors+))
  "Function for playing around with different ways to display pegs.
The function should be evaluated in a mode free from any
disturbing font locks. It will not work properly in this
Emacs-Lisp mode, for instance."
  (loop for (color char) in colors do (insert-peg char color)))

(defun rgb-complement (rgb-color)
  "I thought this functionality was part of Emacs. But where is it. 
This is a pure matematical interpretation, but it works for now."
  (mapcar (bind #'- #xffff 1) '(0 0 0)))

(defun invert-color (color)
  "A more convenient way to calculate the the hex value
`color-complement' of a color in some format."
  (rgb-complement (sstring color)))
;;(mapcar #'invert-color '(white :white "white"))

;;; Mastermind mode
(define-derived-mode mastermind-mode text-mode "MM"
   "Master mind mode
\\{mastermind-mode-map}")

(defun mastermind-mode-map ()
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (loop for (color char) in +colors+
	  do (define-key map (char-to-string char)
	       (lexical-let ((color color) (char char))
		 #'(lambda ()
		     (interactive)
		     (insert-peg char color)))))
    (define-key map "?" 'mastermind-help)
    (define-key map "h" 'mastermind-help)
    (define-key map "n" 'mm-new)
    (define-key map "q" 'mm-quit)
    (define-key map [return] 'mm-submit-row)
    (define-key map [backspace] 'backward-kill-sexp)
    (define-key map [delete] 'forward-kill-sexp)
    map))
;;(mastermind-mode-map)

(defvar mastermind-mode-map ;;(nilf mastermind-mode-map)
  (mastermind-mode-map)
  "Keymap containing mastermind commands.")
;;(setf mastermind-mode-map (mastermind-mode-map))

(defun mastermind ()
  "Start a new Mastermind game. If a prefix argument is given, then duplicate colors are allowed"
  (interactive)
  (switch-to-buffer (mastermind-buffer))
  (mastermind-mode)
  (mm-new t))
;;(mastermind)

(provide 'mastermind)
