(require 'deck)

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
	  deck)))
;;(klondike-init)

(defvar *klondike-state* nil)
;;(setf *klondike-state* (klondike-init))

(defun klondike-format-card (card)
  (format " %s" (deck-format-card card)))
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
    :key #'(lambda (x) (concat* x :in " " :key #'klondike-format-card))))
;;(klondike-insert-visible '(nil (1) (2) (3) (4) (7 5) (16)))

(defun klondike-format-hand (hand)
  (if (null hand)
    "Empty"
    (klondike-format-card (car hand))))
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

(cl-defun klondike-move-hand (&optional column (state *klondike-state*))
  "Move top card from hand and to either visible or out.
If COLUMN is nil it moves the card to the apropriate OUT deck.
Else move card to visible's COLUMNth deck."
  (awhen (pop (klondike-hand state))
    (if column
      (push it (nth column (klondike-visible state)))
      (setf (nth (deck-color it) (klondike-out state)) it))))
;;(klondike-move-hand 0)

(cl-defun klondike-move-visible (card &optional column (state *klondike-state*))
  "Move CARD from hand and to either visible or out.
If COLUMN is nil, it moves the card to the apropriate OUT deck.
This action requires that no card is pushed on it. Else move card
and all cards pushed on it to visible's COLUMNth deck."
  (let ((cards (loop with c = (deck-parse-card card)
		     for visible-column in-ref (klondike-visible state)
		     for hidden-column in-ref (klondike-hidden state)
		     for pos = (position c visible-column)
		     if pos
		     return (prog1
				(pop-list visible-column (1+ pos))
			      (awhen (pop hidden-column)
				(push it visible-column))))))

    (if column
      (push-list cards (nth column (klondike-visible state)))
      (setf (nth (deck-color (car cards)) (klondike-out state))
	    (car cards)))))
;;(klondike-move-visible "9d")

;;; User interface
(defconst +klondike-buffer+ "*Klondike*")

(defun klondike-new ()
  (setf *klondike-state* (klondike-init))
  (klondike-refresh))

(defun klondike-restart ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to quit this solitaire?")
    (klondike-new)))

(defun klondike-quit ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to quit this solitaire?")
    (kill-buffer)))

(defun klondike-refresh ()
  (interactive)
  (with-buffer +klondike-buffer+
    (kill-region (point-min) (point-max))
    (klondike-insert-state)))

(defun klondike-hand-out ()
  (interactive)
  (klondike-move-hand)
  (klondike-refresh))

(defun klondike-hand-out-visible ()
  (interactive)
  (let ((col (read-from-minibuffer "Move hand to visible column: ")))
    (klondike-move-hand (string-to-number col)))
  (klondike-refresh))

(defun klondike-hand-next-card ()
  (interactive)
  (klondike-rotate-hand 1)
  (klondike-refresh))

(defun klondike-hand-previous-card ()
  (interactive)
  (klondike-rotate-hand -1)
  (klondike-refresh))

(defun klondike-move-visible-to-column ()
  (interactive)
  (let ((scard (read-from-minibuffer "Move visible card: "))
	(col (read-from-minibuffer "... to column: ")))
    (klondike-move-visible scard (string-to-number col)))
  (klondike-refresh))

(defun klondike-move-visible-out ()
  (interactive)
  (let ((scard (read-from-minibuffer "Move out card: ")))
    (klondike-move-visible scard))
  (klondike-refresh))

(define-derived-mode klondike-mode text-mode "Klondike"
  "Klondike mode, same as text mode, except return submits the response.
\\{klondike-mode-map}"
  ;; (setf buffer-read-only t)
  (let ((hand-map (make-sparse-keymap))
	(visible-map (make-sparse-keymap)))
    (define-key evil-normal-state-local-map "n" #'klondike-restart)
    (define-key evil-normal-state-local-map "q" #'klondike-quit)
    (define-key evil-normal-state-local-map "r" #'klondike-refresh)
    ;; hand map
    (define-key evil-normal-state-local-map "h" hand-map)
    (define-key hand-map "o" #'klondike-hand-out)
    (define-key hand-map "v" #'klondike-hand-out-visible)
    (define-key hand-map "n" #'klondike-hand-next-card)
    (define-key hand-map "p" #'klondike-hand-previous-card)
    ;; visible map
    (define-key evil-normal-state-local-map "v" visible-map)
    (define-key visible-map "o" #'klondike-move-visible-out)
    (define-key visible-map "v" #'klondike-move-visible-to-column)))

(cl-defun klondike (&optional (unused 1))
  (interactive)
  (switch-to-buffer +klondike-buffer+)
  (setq buffer-face-mode-face
	'(:family "DejaVu Sans" :height 200 :width semi-condensed))
  (klondike-mode)
  (klondike-new))
;;(klondike)

(provide 'klondike)
