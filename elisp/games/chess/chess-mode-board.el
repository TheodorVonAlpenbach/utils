;;;; TODO
;;;; print sboard
;;;; show moves

;;; Utils
(defalias 'rowwise 'first)
(defalias 'columnwise 'second)

(defvar chess-board-margins '((0 0) (0 0)) "Format is ((LEFT RIGHT) (TOP BOTTOM))")

(defvar chess-board-square-size '(5 3) "(WIDTH HEIGHT)")

(defconst chess-board-square-dark-color "light sky blue")
(defconst chess-board-square-light-color "light blue")
(defconst chess-board-white-face '((:foreground "black") (:background "white")))
(defconst chess-board-black-face '((:foreground "white") (:background "black")))

(require 'sboard)
(require 'chess-piece)
(defconst chess-board-sboard (sb-default))

(defun chess-board-face (white-p)
  (if white-p
    chess-board-white-face
    chess-board-black-face))

(defun chess-board-canvas-size (direction)
  (+ (apply #'+ (funcall direction chess-board-margins))
     (* 8 (funcall direction chess-board-square-size))))
;;(mapcar #'chess-board-canvas-size (list rowwise columnwise))

(defun* chess-board-canvas-make (&optional (char 32))
  "Default is the single space character"
  (concat* (make-list (chess-board-canvas-size columnwise)
		      (make-string (chess-board-canvas-size rowwise) char))
	   :in "\n"))

(defun* chess-board-canvas-make (&optional (char 32))
  "Default is the single space character"
  (let* ((s (make-string (rowwise chess-board-square-size) char))
	 (dark-square-line (propertize s 'face `((:background ,chess-board-square-dark-color))))
	 (light-square-line (propertize s 'face `((:background ,chess-board-square-light-color))))
	 (even-line (concat* (loop for i below 4 collect dark-square-line collect light-square-line)))
	 (odd-line (concat* (loop for i below 4 collect light-square-line collect dark-square-line)))
	 (lines (loop with height = (columnwise chess-board-square-size)
		      for i below 8
		      append (make-list 3 (if (evenp i) even-line odd-line)))))
    (concat* (nreverse lines) :in "\n" :suf "\n")))

(defun chess-board-canvas-insert ()
  (insert (chess-board-canvas-make)))


;;; Conversions
(defun esquare (square)
  (list (first square) (- 7 (second square))))

(defun edge-start-position (offset n edge-length)
  (+ offset (* n edge-length)))

(defun edge-mid (edge-length) (/ edge-length 2))
(defun square-mid () (mapcar #'edge-mid chess-board-square-size))
;;(square-mid)

(defun chess-board-start-position ()
  (mapcar #'first chess-board-margins))
;;(chess-board-start-position)

(defun chess-board-square-start-position (square)
  (mapcar* #'edge-start-position (chess-board-start-position) square chess-board-square-size))
;;(chess-board-square-start-position (esquare '(0 0)))

(defun chess-board-square-center (square)
  (mapcar* #'+ (chess-board-square-start-position square) (square-mid)))
;;(chess-board-square-center (esquare '(0 0)))


;;; Other queries
(defun chess-board-square-dark-p (square-description)
  (evenp (snumber square-description)))

<(defun* chess-board-insert-colored-string (string square face &optional (start-pos 1))
  (let ((s (propertize string 'face face))
	(rc (chess-board-square-center (esquare square))))
    (save-excursion
      (goto-line (1+ (second rc)))
      (forward-char (first rc))
      (insert s)
      (delete-char (length s)))))
;;(chess-board-insert-colored-string "N" '(0 0) chess-board-white-face)

(defun* chess-board-insert-piece (piece &optional (start-pos 1))
  (chess-board-insert-colored-string (upcase (string (chess-piece-char piece))) 
				     (square (chess-piece-snumber piece))
				     (chess-board-face (chess-piece-white-p piece))))
;;(chess-board-insert-piece (first all-pieces))

(defun* chess-board-print (pieces &optional (buffer "*chess-print*"))
  (switch-to-buffer buffer)
  (fundamental-mode)
  (chess-board-canvas-insert)
  (loop for p in pieces do
	(chess-board-insert-piece p)))
;;(chess-board-print all-pieces)
 
(provide 'chess-mode-board)
