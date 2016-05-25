(defstruct 
  (chess-side
   (:conc-name cs-)
   (:constructor 
    cs-new (piece-descriptions side
			       &aux 
			       (pieces (make-pieces piece-descriptions side))))
   (:constructor
    cs-white (&optional (piece-descriptions white-piece-descriptions) &aux 
			(side 'white)
			(pieces (make-pieces piece-descriptions side))))
   (:constructor
    cs-black (&optional (piece-descriptions black-piece-descriptions)
			&aux 
			(side 'black)
			(pieces (make-pieces piece-descriptions side)))))
  
  (pieces (copy-pieces white-pieces))
  (side 'white)
  (may-castle t))
;;(setf (chess-piece-snumber (first (cs-pieces (cs-white)))) 'qwe)

(defun cs-remove-piece (cs piece)
  (setf (cs-pieces cs) (delete piece (cs-pieces cs))))
;;(let ((cs (cs-white))) (cs-remove-piece cs (first (cs-pieces cs))) cs)

(provide 'chess-side)