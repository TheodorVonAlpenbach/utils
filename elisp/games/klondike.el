(require 'deck)
(require 'card)

;;;; STATE is a list (HIDDEN VISIBLE OUT HAND)
;;;; HIDDEN and VISIBLE are both lists of length 7
;;;; OUT is a list of length 4
;;;; HAND is a ring of variable length ("all the other cards")
;;;; See 

(defalias 'klondike-hidden #'first)
(defalias 'klondike-visible #'second)
(defalias 'klondike-out #'third)
(defalias 'klondike-hand #'fourth)
;;(klondike-hidden *klondike-state*)

(defun klondike-init ()
  (let ((deck (deck-shuffle)))
    (list (loop for i from 1 to 7 collect (pop-list deck (1- i)))
	  (mapcar #'list (pop-list deck 7))
	  (list -1 -2 -3 -4)
	  (push-back nil deck))))
;;(klondike-init)

(defvar *klondike-state* nil)
;;(setf *klondike-state* (klondike-init))

(defun klondike-format-card (card)
  
  (concat (propertize " " 'font-lock-face '(:height 3.0))
	  (card-format card)))
;;(klondike-format-card 0)

(defun klondike-format-out (out)
  (concat* out :key #'klondike-format-card))
;;(klondike-format-out (klondike-out *klondike-state*))

(defun klondike-format-visible (visible)
  (concat* (transpose
	    (loop with r = (min-value visible :test #'> :key #'length)
		  for c in visible
		  collect (reverse (append (make-list (- r (length c)) nil) c))))
    :in "\n"
    :key #'(lambda (x) (concat* x :key #'klondike-format-card))))
;;(klondike-insert-visible '(nil (1) (2) (3) (4) (7 5) (16)))

(defun klondike-format-hand (hand)
  (if (= (length hand) 1)
    "Empty"
    (if (null (car hand))
      "Turn hand!"
      (klondike-format-card (car hand)))))
;;(klondike-format-hand (klondike-hand *klondike-state*))

(cl-defun klondike-insert-state (&optional (state *klondike-state*))
  (insert (klondike-format-out (klondike-out state)))
  (newline 2)
  (insert (klondike-format-visible (klondike-visible state)))
  (newline 2)
  (insert (klondike-format-hand (klondike-hand state)))
  (newline 2))
;;(klondike-insert-state)

(cl-defun klondike-rotate-hand (&optional (n 1) (state *klondike-state*))
  (rotatef-list (klondike-hand state) n))
;;(klondike-rotate-hand)
;;(length (klondike-hand *klondike-state*))

(defun klondike-legal-out-p (n m)
  "Return true if card N can be placed on top of out-card M.
If M is negative, there is no out-card present, so N then needs
to be an ace in order for the function to return true."
  (if (minusp m)
    (card-ace-p n)
    (and (= (card-color n) (card-color m))
	 (= n (1+ m)))))
;;(klondike-legal-out-p 0 -1)

(defun klondike-legal-visible-p (n m)
  (if m
    (and (xor (card-red-p n) (card-red-p m))
	 (zerop (mod (- m n 1) 13)))
    (card-king-p n)))
;;(klondike-legal-visible-p 1 15)

(cl-defun klondike-push-card-out (n out)
  (let ((i (card-color n)))
    (when (klondike-legal-out-p n (nth i out))
      (setf (nth i out) n))))

(cl-defun klondike-push-card-visible (ns i visible)
  "Try to push card or cards N onto the Ith column in visible."
  (when (klondike-legal-visible-p (last-elt ns) (car (nth i visible)))
    (push-list ns (nth i visible))))

(cl-defun klondike-move-hand (&optional column (state *klondike-state*))
  "Move top card from hand and to either visible or out.
If COLUMN is nil it moves the card to the apropriate OUT deck.
Else move card to visible's COLUMNth deck."
  (aif (car (klondike-hand state))
    (if (if column
	  (klondike-push-card-visible (list it) column (klondike-visible state))
	  (klondike-push-card-out it (klondike-out state)))
      (pop (klondike-hand state))
      (message "Illegal move!"))
    (message "Hand is empty!")))
;;(klondike-move-hand 0)

(cl-defun klondike-find-visible (n visible)
  "Return (COLUMN ROW) or NIL,
where COLUMN and row are the visible the column and row number,
respectively."
  (loop for col in visible
	for i from 0
	if (position n col) return (list i it)))
;;(klondike-find-visible (card-parse "2d") (klondike-visible *klondike-state*))

(cl-defun klondike-move-visible (n &optional column (state *klondike-state*))
  "Move card N from hand and to either visible or out.
If COLUMN is nil, it moves the card to the apropriate OUT deck.
This action requires that no card is pushed on it. Else move card
and all cards pushed on it to visible's COLUMNth deck."
  (let ((visible (klondike-visible state)))
    (aif (klondike-find-visible n visible)
      (destructuring-bind (i j) it
	(if (if column
	      (klondike-push-card-visible
	       (subseq (nth i visible) 0 (1+ j)) column visible)
	      (and (zerop j)
		   (klondike-push-card-out
		    (car (nth i visible)) (klondike-out state))))
	  (progn
	    (setf (nth i visible) (nthcdr (1+ j) (nth i visible)))
	    (unless (nth i visible)
	      (awhen (pop (nth i (klondike-hidden state)))
		(push it (nth i visible)))))
	  (message "Illegal move!")))
      (message "Card is not visible!"))))
;;(klondike-move-visible "2h")

;;; User interface
(defconst +klondike-buffer+ "*Klondike*")

(defun klondike-ui-refresh ()
  (interactive)
  (with-buffer +klondike-buffer+
    (kill-region (point-min) (point-max))
    (klondike-insert-state)))

(defun klondike-ui-new ()
  (setf *klondike-state* (klondike-init))
  (klondike-ui-refresh))

(defun klondike-ui-restart ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to quit this solitaire?")
    (klondike-ui-new)))

(defun klondike-ui-quit ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to quit this solitaire?")
    (kill-buffer)))

(defun klondike-ui-hand-out ()
  (interactive)
  (klondike-move-hand)
  (klondike-ui-refresh))

(defun klondike-ui-hand-out-visible ()
  (interactive)
  (let ((col (read-from-minibuffer "Move hand to visible column: ")))
    (klondike-move-hand (klondike-parse-column col)))
  (klondike-ui-refresh)
  (klondike-finished))

(defun klondike-ui-hand-next-card ()
  (interactive)
  (klondike-rotate-hand 1)
  (klondike-ui-refresh))

(defun klondike-ui-hand-previous-card ()
  (interactive)
  (klondike-rotate-hand -1)
  (klondike-ui-refresh))

(defun klondike-parse-column (col)
  (if (string-match "^[a-g]$" (downcase col))
    (- (char col 0) ?a)
    (if (string-match "^[0-6]$" col)
      (string-to-number col)
      (message "Illegal column descriptor"))))

(defun klondike-ui-move-visible-to-column ()
  (interactive)
  (let ((scard (read-from-minibuffer "Move visible card: "))
	(col (read-from-minibuffer "... to column: ")))
    (klondike-move-visible (card-parse scard) (klondike-parse-column col)))
  (klondike-ui-refresh))

(cl-defun klondike-finished (&optional (out (klondike-out *klondike-state*)))
  (when (every #'card-king-p out)
    (message "Klondike accomplished, congratulations!")))
;;(klondike-finished '(22 25 38 51))

(cl-defun klondike-push-visible-out (&optional (state *klondike-state*))
  "Move a visible card to the out card row if possible or return nil.
The current version will select the out card candidate in the
order from left to right."
  (loop for col in (klondike-visible state)
	until (klondike-move-visible (last-elt col) nil (klondike-out state))))

(defun klondike-ui-move-visible-out ()
  (interactive)
  (klondike-push-visible-out *klondike-state*)
  (klondike-ui-refresh)
  (klondike-finished))

(define-derived-mode klondike-mode text-mode "Klondike"
  "Klondike mode, same as text mode, except return submits the response.
\\{klondike-mode-map}"
  ;; (setf buffer-read-only t)
  (let ((hand-map (make-sparse-keymap))
	(visible-map (make-sparse-keymap)))
    (define-key evil-normal-state-local-map "n" #'klondike-ui-restart)
    (define-key evil-normal-state-local-map "q" #'klondike-ui-quit)
    (define-key evil-normal-state-local-map "r" #'klondike-ui-refresh)
    ;; hand map
    (define-key evil-normal-state-local-map "h" hand-map)
    (define-key hand-map "o" #'klondike-ui-hand-out)
    (define-key hand-map "v" #'klondike-ui-hand-out-visible)
    (define-key hand-map "n" #'klondike-ui-hand-next-card)
    (define-key hand-map "p" #'klondike-ui-hand-previous-card)
    ;; visible map
    (define-key evil-normal-state-local-map "v" visible-map)
    (define-key visible-map "o" #'klondike-ui-move-visible-out)
    (define-key visible-map "v" #'klondike-ui-move-visible-to-column)))

(cl-defun klondike (&optional (unused 1))
  (interactive)
  (switch-to-buffer +klondike-buffer+)
  (setq buffer-face-mode-face
	'(:family "DejaVu Sans" :height 200 :width semi-condensed))
  (klondike-mode)
  (klondike-ui-new))
;;(klondike)

(provide 'klondike)
