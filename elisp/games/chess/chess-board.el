(require 'chess-utils)
(require 'chess-square)

;;; CHESS-BOARD-SQUARE (CBS)
(defconst cbs-default-size '(9 5))

(defstruct (chess-board-square
	    (:conc-name cbs-))
  (size cbs-default-size)
  (address '(0 0)) ;;a SQUARE element
  color
  (symbol "")
  (symbol-alignment :center))

(defun* cbs-new (&optional (symbol "") dark-p)
  (make-chess-board-square 
   ;;default color is only for testing
   :color (if dark-p "blue" "light sky blue")
   :symbol symbol))
;;(cbs-new)

(defun* cbs-clone (cbs &key size address color symbol symbol-alignment)
  (let ((clone (copy-chess-board-square cbs)))
    (when size (setf (cbs-size clone) size))
    (when address (setf (cbs-address clone) address))
    (when color (setf (cbs-color clone) color))
    (when symbol (setf (cbs-symbol clone) symbol))
    (when symbol-alignment (setf (cbs-symbol-alignment clone) symbol-alignment))
    clone))
;;(cbs-clone (cbs-new))

(defun cbs-width (cbs) 
  (car (cbs-size cbs)))

(defun cbs-height (cbs)
  (cadr (cbs-size cbs)))
;;(cbs-height (cbs-new))

(defun cbs-line (cbs)
  "Returns a property string representing a cbs line"
  (propertize (make-string (cbs-width cbs) ? )
	      'face `((:background ,(cbs-color cbs)))))
;;(cbs-line (cbs-new))

(defun cbs-center (cbs)
  (mapcar (bind #'/ 2) (cbs-size cbs)))
;;(cbs-center (cbs-new))		                                                      
					                                                      
(defun cbs-symbol-ecl (cbs)		                                                      
  (if (eq (cbs-symbol-alignment cbs) :center)
    (cbs-center cbs)			                                                      
    (error "Method does not support alignment %S" (cbs-symbol-alignment cbs))))
;;(cbs-center (cbs-new))

(defun* cbs-position (cbs)
  "Returns the relative position of SQUARE to CBS."
  (mapcar* #'* (cbs-size cbs) (cbs-address cbs)))
;;(cbs-position (make-chess-board-square :address '(1 2)))

(defun cbs-insert-at (cbs column-line &optional overwrite-p)
  "Inserts a CBS at COLUMN and LINE."
  (let ((ecl (ecl-add column-line (cbs-position cbs))))
    (loop for i from (ecl-line ecl) below (+ (ecl-line ecl) (cbs-height cbs))
	  do (insert-at (cbs-line cbs) (list (ecl-column ecl) i) overwrite-p))
    (insert-at (cbs-symbol cbs) (ecl-add ecl (cbs-symbol-ecl cbs)) t)))
;;(cbs-insert-at (make-chess-board-square :address '(1 2) :symbol "Q") '(2 57))

(defun* cbs-insert (cbs &optional overwrite-p (point (point)))
  "Inserts a CBS at point"
  (cbs-insert-at cbs (column-line point)))
;;(cbs-insert (cbs-new))


;;; CHESS-BOARD-CANVAS (CBC)
(defconst cbc-default-light-square-color "light sky blue")
(defconst cbc-default-dark-square-color "light blue")

(defconst cbc-default-dimensions '(8 8))

(defstruct (chess-board-canvas
	    (:conc-name cbc-))
  (squares))
;;(make-chess-board-canvas)

(defun* cbc-new (&optional (square-size cbs-default-size) 
			   (light-color cbc-default-light-square-color)
			   (dark-color cbc-default-dark-square-color)
			   (reversed-p nil)
			   (dimensions cbc-default-dimensions))
  (make-chess-board-canvas 
   :squares (maptree #'(lambda (square)
			 (make-chess-board-square
			  :size square-size 
			  :address square
			  :color (if (square-light-p square)
				   light-color dark-color)))
		     (squares dimensions)
		     2)))
;;(cbc-new)

(defun cbc-insert-at (cbc column-line &optional reversed-p)
  "Inserts a CBS at point"
  (maptree (bind #'cbs-insert-at column-line)
	   (if reversed-p
	     (cbc-squares cbc)
	     (reverse (cbc-squares cbc)))
	   2))

(defun* cbc-insert (cbc &optional (point (point)))
  "Inserts a CBC at point"
  (cbc-insert-at cbc (column-line point)))
;;(cbc-insert (cbc-new))

;;; CHESS-BOARD-BORDER (CBB)
(defconst cbb-default-border-color "brown")
(defstruct (chess-board-border
	    (:conc-name cbb-))
  (canvas) ;;an (1 n) or (n 1) CBS
  (margin-squares (list nil nil))
  )

(defun* cbb-new (symbols cbs-template margins orientation)
  "Creates a new chess board border (CBB) .
SYMBOLS is a list of strings (each of length, typically 1)
BORDER-SQUARE is a CBS template for border squares
MARGINS is a integers specifying the border margin widths
ORIENTATION is either :vertical or :horizontal"
  (let* ((cbss (list (loop for s in symbols
			   for i from 0
			   for address = (if (eq orientation :horizontal) (list i 0))
			   collect (cbs-clone cbs-template :address address :symbol s))))
	 (margins-size  (mapcar #'(lambda (x) 
				    (if (eq orientation :horizontal)
				      (list x (second (cbs-size cbs-template)))
				      (list (first (cbs-size cbs-template)) x)))
				margins))
	 (cbs-margins (mapcar #'(lambda (x) (cbs-clone cbs-template :size x)) margins-size)))
    (make-chess-board-border :canvas (make-chess-board-canvas :squares (if (eq orientation :horizontal)
									 cbss (transpose cbss)))
			     :margin-squares cbs-margins)))
;;(cbb-new '(A B C D E F G H) (make-chess-board-square :size '(9 1) :color "brown") '(0 0) :horizontal)



(provide 'chess-board)

