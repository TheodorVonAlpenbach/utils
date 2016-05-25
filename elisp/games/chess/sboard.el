(require 'position)

(defun make-sboard (pieces)
  (let ((sboard (make-vector 64 nil)))
    (loop for p in pieces
	  do (setf (aref sboard (chess-piece-snumber p)) p))
    sboard))
;;(make-sboard all-pieces)

(defun sb-default ()
  (make-sboard all-pieces))

;;some board utils
(defun sb-set (cb snumber x)
  (setf (aref cb snumber) x))

(defun sb-clear (cb snumber)
  (sb-set cb snumber nil))

(defun sb-get (cb snumber)
  (aref cb snumber))

(defun sb-print (sb)
  )

(defun pieces-to-board-matrix (pieces)
  (let ((bm (make-board-matrix)))
    (loop for p in pieces
	  do (bm-setf bm 
		      (square (chess-piece-snumber p))
		      (chess-piece-char p)))
    bm))

(provide 'sboard)