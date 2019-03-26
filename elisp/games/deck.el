(defconst +deck-value+ (append (cons 'A (a-b 2 9)) '(T J Q K)))

(defconst +deck-color+
  (list (propertize "♠" 'font-lock-face '(:foreground "black"))
	(propertize "♥" 'font-lock-face '(:foreground "red"))
	(propertize "♦" 'font-lock-face '(:foreground "red"))
	(propertize "♣" 'font-lock-face '(:foreground "black"))))

(cl-defun deck-insert-card-1 (sv sc &optional (pos (point)))
  (if sv
    (insert (format "%S%s" sv sc))
    (insert (format " %s" sc))))
;;(deck-insert-card-1 nil "a")

(cl-defun deck-insert-card (n &optional (pos (point)))
  (destructuring-bind (v c) (cl-floor n 13)
    (deck-insert-card-1 (nth c +deck-value+) (nth v +deck-color+))))

(cl-defun deck-format-card-1 (sv sc &optional (pos (point)))
  (if sv
    (format "%S%s" sv sc)
    (format " %s" sc)))
;;(deck-insert-card-1 nil "a")

(cl-defun deck-format-card (n &optional (pos (point)))
  (if n
    (if (minusp n)
      (format " %s" (nth (- -1 n) +deck-color+))
      (destructuring-bind (v c) (cl-floor n 13)
	(format "%S%s" (nth c +deck-value+) (nth v +deck-color+))))
    "  "))
;;(mapcar #'deck-format-card '(0 -1 -2 -3 -4 nil))

(defun deck-shuffle ()
  (randomize (0-n 52)))
;;(deck-shuffle)

(defun deck-value (card)
  (mod card 13))

(defun deck-color (card)
  (/ card 13))

(defun deck-parse-card (string-or-symbol &optional split-p)
  (let ((scard (upcase (sstring string-or-symbol))))
    (let ((v (position (ssymbol (substring scard 0 1)) +deck-value+))
	  (c (position (ssymbol (substring scard 1 2)) '(S H D C))))
      (if split-p
	(list v c)
	(+ (* 13 c) v)))))
;;(deck-parse-card "ah" t)

(provide 'deck)
