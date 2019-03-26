(require 'deck)

;;;; STATE is a list (HIDDEN VISIBLE OUT HAND)
;;;; HIDDEN and VISIBLE are both lists of length 7
;;;; OUT is a list of length 4
;;;; HAND is a ring of variable length ("all the other cards")
;;;; See 

(defvar *klondike-state* nil)
;;(setf *klondike-state* (klondike-init))

(defalias 'klondike-hidden #'first)
(defalias 'klondike-visible #'second)
(defalias 'klondike-out #'third)
(defalias 'klondike-hand #'fourth)
;;(klondike-hidden *klondike-state*)

(defun klondike-init ()
  (let ((deck (deck-shuffle))
	(hand (make-ring 24)))
    (loop for x in deck do (ring-insert hand x))
    (list (loop for i from 1 to 7 collect (pop-list deck i))
	  (mapcar #'list (pop-list deck 7))
	  '(0 0 0 0)
	  hand)))
;;(klondike-init)

(defun klondike-format-card (card)
  (format " %s" (deck-format-card card)))
;;(klondike-format-card 0)

(defun klondike-insert-out (out)
  (loop for c in out
	for i from 0
	do (insert " ")
	do (if (plusp c)
	     (deck-insert-card c)
	     (deck-insert-card-1 nil (nth i +deck-color+)))))

(defun klondike-format-out (out)
  (concat* out :in " " :key #'klondike-format-card))
;;(klondike-format-out '(0 1 -1 3))

(defun klondike-format-visible (visible)
  (concat* (transpose
	    (loop with r = (min-value visible :test #'> :key #'length)
		  for c in visible
		  collect (reverse (append (make-list (- r (length c)) nil) c))))
    :in "\n"
    :key #'(lambda (x) (concat* x :in " " :key #'klondike-format-card))))
;;(klondike-insert-visible '(nil (1) (2) (3) (4) (7 5) (16)))

(defun klondike-format-hand (hand)
  (if (ring-empty-p hand)
    "Empty"
    (klondike-format-card (ring-ref hand 0))))
;;(klondike-insert-hand (klondike-hand *klondike-state*))A♥

(cl-defun klondike-insert-state (&optional (state *klondike-state*))
  (insert (klondike-format-out (klondike-out state)))
  (newline 2)
  (insert (klondike-format-visible (klondike-visible state)))
  (newline 2)
  (insert (klondike-format-hand (klondike-hand state)))
  (newline 2))
;;(klondike-insert-state)

(cl-defun klondike-move-hand (&optional column (state *klondike-state*))
  (let ((hand (klondike-hand state)))
    (when (plusp (ring-length hand))
      (let ((card (ring-remove hand 0)))
	(if column
	  (push card (nth column (klondike-visible state)))
	  (setf (nth (deck-color card) (klondike-out state)) card))))))
;;(klondike-move-hand 0)

(cl-defun klondike-move-visible (card &optional column (state *klondike-state*))
  "CARD is a symbol or string: as, ah, ad, ac, 2s, 2h, ..., kc"
  (let ((cards (loop with c = (deck-parse-card card)
		     for col in-ref (klondike-visible state)
		     for pos = (position c col)
		     if pos return (pop-list col (1+ pos)))))

    (if column
      (push-list cards (nth column (klondike-visible state)))
      (setf (nth (deck-color (car cards)) (klondike-out state))
	    (car cards)))))
;;(klondike-move-visible "9d")
