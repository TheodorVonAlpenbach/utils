(require 'chess-piece)
(require 'sboard)

(cl-defstruct (chess-move 
	    (:conc-name cm-))
  board ;sboard
  from ;snumber
  to ;snumber
  piece ;chess-piece
  capture ;chess-piece
  check ;bool
  promotion; chess-piece
)

(cl-defun cm-new (cb from to)
  "Returns nil if trying to capture own piece"
  (let ((piece (sb-get cb from))
	(capture (sb-get cb to)))
    (unless 
	(or (and piece capture
		 (eq (chess-piece-side piece) ;cannot capture own piece
		     (chess-piece-side capture)))
	    (and (eq (chess-piece-type piece) 'pawn)
		 ;; destination can't be blocked
		 (or (sb-get cb to))
		 ;; square in front can't be blocked either
		 (or (sb-get cb (+ from 8)))))
      (make-chess-move
       :board cb
       :from from
       :to to
       :piece piece
       :capture capture
       ;;:check
       ))))

(cl-defun cm-pawn-2rows-p (cm)
  (= (- (cm-to cm) (cm-from cm)) 16))

(cl-defun cm-pawn-jump-p (cm)
  (and (cm-pawn-2rows-p cm)
       (sb-get (cm-board cm) (+ from 8))))

(cl-defun cm-new-pawn (cb from to)
  "Special constructor for pawn move"
  (awhen (cm-new cb from to)
    (let ((same-column-p (snumber-on-same-column-p from to)))
      (if (cm-capture cm)
       (unless same-column-p it)
       (when same-column-p
	 (unless (cm-pawn-jump-p it) it))))))

(cl-defun cm-new-promotions (cm)
  "Returns a list of (four) moves involving promotion.
Assumes that the move is legal"
  (cl-loop for type in '(queen rook bishop knight)
	do (setf (cm-promotion cm) 
		 (make-chess-piece type (chess-piece-side (cm-piece cm)) (cm-to cm)))
	collect cm))

(cl-defun cm-execute (cm)
  "Returns the captured piece (or nil if no capture)"
  (let* ((piece (or (cm-promotion cm) (cm-piece cm)))
	 (cb (cm-board cm))
	 (capture (cm-capture cm)))
    (sb-clear cb (cm-from cm))
    (sb-set cb (cm-to cm) piece)
    (setf (chess-piece-snumber piece) (cm-to cm))
    capture))

(cl-defun cm-take-back (cm)
  "Returns the captured piece (or nil if no capture)"
  (let* ((piece (cm-piece cm))
	 (capture (cm-capture cm))
	 (cb (cm-board cm)))
    (sb-set cb (cm-from cm) piece)
    (sb-set cb (cm-to cm) capture)
    (setf (chess-piece-snumber piece) (cm-from cm))
    capture))

(cl-defun cm-side (cm)
  (chess-piece-side (cm-piece cm)))

(cl-defun cm-print (cm)
  (list (cm-from cm) (cm-to cm) (cm-piece cm) (cm-capture cm) (cm-promotion cm)))

(cl-defun cm-to-string (cm &optional first-move-p style)
  "Optional argument and STYLE is currently unsupported.
Assumes move has been executed "
  (chess-piece-print (cm-piece cm) first-move-p nil))
;;(cm-to-string (parse-chess-move "Nb1"))

(cl-defun cm-range-print (cms)
  (print-board-positions (mapcar #'cm-to cms)))

(provide 'chess-move)

