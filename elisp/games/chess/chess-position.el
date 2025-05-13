(require 'chess-side)
(require 'sranges)

;;; Chess Position
(cl-defstruct (chess-position
	    (:conc-name cp-))
  (history '())
  (white (cs-white))
  (black (cs-black))
  board
  (evaluation 0))

(cl-defun cp-all-pieces (chess-position)
  (append (cs-pieces (cp-white chess-position))
	  (cs-pieces (cp-black chess-position))))
;;(cp-all-pieces (cp-new))

(cl-defun cp-new () 
  (let* ((cp (make-chess-position)))
    (setf (cp-board cp) (make-sboard (cp-all-pieces cp)))
    cp))
;;(cp-new)

(cl-defun cp-last-move (cp) (first (cp-history cp)))

(cl-defun cp-side (cp white-p)
  (if white-p (cp-white cp) (cp-black cp)))

(cl-defun cp-remove-piece (cp piece)
  (cs-remove-piece
   (cp-side cp (eq (chess-piece-side piece) 'white))
   piece))

(cl-defun cp-white-to-move-p (chess-position)
  (evenp (length (cp-history chess-position))))
;;(cp-white-to-move-p (cp-new))

(cl-defun cp-side-to-move (cp)
  (cp-side cp (cp-white-to-move-p cp)))
;;(cp-side-to-move (cp-new))

(cl-defun cp-print-ply (cp &optional (n 0) first-move-p)
  (let ((cm (nth* (- n) (cp-history cp))))
    (format "%s%s"
      (if (eq (cm-side cm) 'white)
	(format "%d." (1+ (/ n 2)))
	"")
      (cm-to-string cm first-move-p))))

(cl-defun cp-print-history (cp)
  (concat* (cl-loop for i below (length (cp-history cp))
			   collect (cp-print-ply cp i))
	   :in " "))

(cl-defun cp-print (cp &optional history)
  "Iff history then print all moves, else print only last move"
  (format "%s\n%s"
    (bm-print (pieces-to-board-matrix (cp-all-pieces cp)))
    (if history
      (cp-print-history cp)
      (cp-print-ply cp 0 t))))
;(cp-print (cp-move* (cp-new) '((g8 f6) (b1 c3) (e2 e4))) t)

(cl-defun cp-move (cp cm)
  (aif (cm-execute cm)
    (cp-remove-piece cp it))
  (push cm (cp-history cp))
  cp)

(cl-defun cp-take-back (cp cm)
  (aif (cm-take-back cm)
    (cp-add-piece cp it))
  (pop (cp-history cp))
  cp)

(cl-defun cp-move* (cp moves)
  (cl-loop for m in moves
	do (cp-move cp (snumber (first m)) (snumber (second m))))
  cp)
;;(cp-move* (cp-new) '((b1 c3) (g8 f6) (d2 d4)))

(cl-defun cp-test-move (from to)
  (let* ((cp (cp-new)))
    (cp-move cp (snumber from) (snumber to))
    cp))

;;(cp-print (cp-test-move 'e2 'e4))

;;; Possible moves
(cl-defun cp-possible-moves-grouped-by-piece (cp)
  (cl-loop for piece in-ref (cs-pieces (cp-side-to-move cp))
	for r = (actual-range piece (cp-board cp))
	if r collect r))

(cl-defun cp-possible-moves (cp &optional grouped-by-piece)
  (let ((moves (cp-possible-moves-grouped-by-piece cp)))
    (if grouped-by-piece moves (flatten moves))))
;;(mapcar #'cm-print (cp-possible-moves (cp-new) t))

(cl-defun cp-show-possible-moves ())

(let
    ((n nil)
     (cp* nil))
  (cl-defun cp-show-possible-moves (&optional cp)

    (when cp
      (setf cp* (if (chess-position-p cp) cp (cp-new)))
      (setf n 0))

    (when (not cp*)
      (setf cp* (cp-new))
      (setf n 0))

    (let* ((moves (cp-possible-moves cp*))
	   (cm (nth n moves))
	   (cmp (cm-print cm)))
      (cp-move cp* cm)
      (cl-incf n)
      (when (>= n (length moves))
	(setf n 0))
      (let ((s (cp-print cp*)))
	(cp-take-back cp* cm)
	s))))
;;(cp-show-possible-moves)

(provide 'chess-position)
