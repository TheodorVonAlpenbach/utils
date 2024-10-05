(require 'position)
(require 'chess-piece)

(cl-defun make-sboard (pieces)
  (let ((sboard (make-vector 64 nil)))
    (cl-loop for p in pieces
	  do (setf (aref sboard (chess-piece-snumber p)) p))
    sboard))
;;(make-sboard all-pieces)

(cl-defun sb-default ()
  (make-sboard all-pieces))
;;(sb-default)

;;some board utils
(cl-defun sb-set (cb snumber x)
  (setf (aref cb snumber) x))

(cl-defun sb-clear (cb snumber)
  (sb-set cb snumber nil))

(cl-defun sb-get (cb snumber)
  (aref cb snumber))

(cl-defun sb-print (sb)
  )

(cl-defun pieces-to-board-matrix (pieces)
  (let ((bm (make-board-matrix)))
    (cl-loop for p in pieces
	  do (bm-setf bm 
		      (square (chess-piece-snumber p))
		      (chess-piece-char p)))
    bm))

(provide 'sboard)
