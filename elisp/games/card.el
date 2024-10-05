(defconst +card-values+ (append (cons 'A (a-b 2 9)) '(T J Q K)))

(defconst +card-colors+
  (list (propertize "♠" 'font-lock-face '(:foreground "black" :height 3.0))
	(propertize "♥" 'font-lock-face '(:foreground "red" :height 3.0))
	(propertize "♦" 'font-lock-face '(:foreground "red" :height 3.0))
	(propertize "♣" 'font-lock-face '(:foreground "black" :height 3.0))))

(cl-defun card-format (n &optional (pos (point)))
  (if n
    (if (minusp n)
      (concat (propertize " " 'font-lock-face '(:height 3.0))
	      (nth (- -1 n) +card-colors+))
      (destructuring-bind (v c) (cl-floor n 13)
	(concat (propertize (format "%S" (nth c +card-values+))
			    'font-lock-face '(:height 3.0))
		(nth v +card-colors+))))
    (propertize "  " 'font-lock-face '(:height 3.0))))
;;(mapcar #'card-format '(0 -1 -2 -3 -4 nil))

(cl-defun card-value (n) (mod n 13))
(cl-defun card-color (n) (/ n 13))
(cl-defun card-red-p (n) (< 0 (card-color n) 3))
(cl-defun card-black-p (n) (not (card-red-p n)))

(cl-defun card-ace-p (n) (zerop (card-value n)))
(cl-defun card-king-p (n) (= (card-value n) 12))

(cl-defun card-parse-value (char)
  (case (upcase char)
    ((?B ?C ?D ?E ?F ?G ?H ?I) (- (upcase char) 65))
    (t (position (ssymbol (upcase (coerce (list char) 'string)))
		 +card-values+))))
;;(mapcar #'card-parse-value '(?a ?b ?c ?i ?2 ?3 ?8 ?9 ?t ?j ?q ?k))

(cl-defun card-parse (string-or-symbol &optional split-p)
  (let ((scard (upcase (sstring string-or-symbol))))
    (let ((v (card-parse-value (char scard 0)))
	  (c (position (ssymbol (substring scard 1 2)) '(S H D C))))
      (if split-p
	(list v c)
	(+ (* 13 c) v)))))
;;(card-parse "ah" t)


(provide 'card)
