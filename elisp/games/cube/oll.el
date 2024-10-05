;;; This module contains functions that convert a shorthand
;;; representation of an OLL algorithm to rubikcube tex code.
;;; 
;;; Faces are numbered 1 to 9 as in the figure below. Corners are
;;; either the symbol L or R, indicating that the yellow face is
;;; oriented left or right, respectively. Only relevant corners are
;;; listed, but always anti-clockwise.
;;;
;;;   D         C
;;;    |7‾‾8‾‾9| 
;;;    |4  5  6| 
;;;    |1__2__3| 
;;;   A         B
;;;
;;; An algorithm is represented as a string on the format "NAME:
;;; <UFACES> <OCORNERS>, where NAME is the name of the algorithm,
;;; <UFACES> is a string with numbers indicating the colored faces,
;;; and <OCORNERS> is a string indicating the corner orientation as
;;; described above.
;;;
;;; E.g. The "Gun" algorithm is represented as the string "Gun: 1456
;;; lll". This means that only the upper faces 1, 4, 5, and 6 are
;;; yellow, while all the relevant corners are oriented left. In this
;;; case the relevant corners are B, C, and D. A is not a relevant
;;; corner since the upper face 1 is yellow.
;;;
;;; Other examples: "Fish Salad: 23567 rl"
;;;
;;; In the functions below the following varibale names are used
;;; consistently.
;;; OLL:  The whole shorthand string ("Fish Salad: 23567 rl")
;;; UF:   The yellow upper faces ('(2 3 5 6 7))
;;; ROC:  The relevant corner orientations ('(r l))
;;; OC:   All corner orientations A, B, C, and D. The symbol U indicates up. 
;;;       ('(r u l u))
;;; UC:   Upper corners
;;; SF:   A triplet of X's and Y's denoting the side face colors on a side
;;; SIDE: The symbol b, r, t, or l, denoting the bottom, right, top, and
;;;;      left side, respectively.

(require 'cube-utils)

(defconst +oll-tex-direction+ '(6 7 8  3 4 5  0 1 2)
  "The order the upper faces are listed in the TeX module rubik.")

(cl-defun oll-expand-corners (uf roc)
  "Return OC"
  (assert (= (length (cl-set-difference '(1 3 9 7) uf))
	     (length roc))
	  nil "Malformed oll. UF: %S, ROC: %S" uf roc)
  (cl-loop for x in '(1 3 9 7)
	collect (if (member x uf) 'u (pop roc))))
;;(oll-expand-corners '(5 9) '(r r r))
;;(oll-expand-corners '(3 4 5 6 9) '(r l))

(cl-defun oll-uf-p (i uf)
  "Return not nil if upper face I is yellow given UF."
  (not-null (member i uf)))

(cl-defun oll-ufs (uf)
  "Map each upper face to nil if it is not yellow. Otherwise map it to t."
  (cl-loop for i from 1 to 9 collect (oll-uf-p i uf)))
;;(oll-ufs '(2 3 5 6 7))

(cl-defun mapnil (list value &optional true-value)
  (cl-loop for x in list collect (if x (or true-value x) value)))
;;(mapnil '(nil 1 nil 2) 'x 'y)

(cl-defun oll-uf-xy-tex (uf)
  "Convert UF to X's and Y's in the TeX direction"
  (mapnil (project (oll-ufs uf) +oll-tex-direction+) 'X 'Y))
;;(oll-uf-xy-tex '(2 3 5 6 7))

(cl-defun oll-swap-xy (color) (if (eql color 'X) 'Y 'X))

(cl-defun oll-side (lc mid rc)
  (list (if (eql lc 'r) 'Y 'X) (if mid 'X 'Y) (if (eql rc 'l) 'Y 'X)))
;;(oll-side 'l nil 'l)

(cl-defun oll-sides (uf roc)
  (let ((oc (oll-expand-corners uf roc))
	(xy (oll-ufs uf)))
    (list (oll-side (nth 0 oc) (nth 1 xy) (nth 1 oc))
	  (oll-side (nth 1 oc) (nth 5 xy) (nth 2 oc))
	  (oll-side (nth 2 oc) (nth 7 xy) (nth 3 oc))
	  (oll-side (nth 3 oc) (nth 3 xy) (nth 0 oc)))))
;;(oll-sides '(2 3 4 5 6 8 9) '(r l))

;;;; TeX output
(cl-defun oll{} (color)
  "Embrace color."
  (format "{%S}" color))
;;(oll{} 'X)

(cl-defun oll-face-tex (uf)
  (concat* (oll-uf-xy-tex uf) :pre "  \\RubikFaceUp" :key #'oll{}))
;;(oll-face-tex '(2 3 5 6 7))

(cl-defun oll-side-tex (side sf &aux (hidden '(X X X X X X)))
  (concat* (append sf hidden)
    :key #'oll{} :pre (format "\\RubikFace%s" side)))
;;(oll-side-tex "Back" '(X Y Y))

(cl-defun oll-sides-tex (uf roc)
  (concat* (cl-loop for side in '("Front" "Right" "Back" "Left")
		 for sf in (oll-sides uf roc)
		 collect (oll-side-tex side sf))
    :pre "  " :in "\n  "))
;;(oll-sides-tex '(5 9) '(r r r))

(cl-defun oll-parse (oll)
  (let ((soll (split-string oll nil t)))
    (list (mapcar #'string-to-integer (split-string (first soll) "" t))
	  (when (rest soll)
	    (mapcar #'ssymbol (split-string (second soll) "" t))))))
;;(oll-parse "13579")

(cl-defun oll-tex (oll &optional (cm 1) (ratio 0.3))
  (destructuring-bind (uf roc) (oll-parse oll)
    (format "\\ShowCube{%gcm}{%g}{\n%s\n%s\n  \\DrawRubikFaceUpSide\n}"
      cm ratio (oll-face-tex uf) (oll-sides-tex uf roc))))
;;(oll-tex "59 rrr")

(cl-defun oll-fliplr-if-G (alg anti)
  (if (string= anti "G")
    (cube-with-algorithm #'cube-fliplr alg)
    anti))

(cl-defun oll-table-row (x tree)
  (destructuring-bind (name oll alg anti rev) x
    (concat* (list (oll-tex oll)
		   name
		   (oll-fliplr-if-G anti alg)
		   (oll-fliplr-if-G alg anti)
		   rev)
      :in "\n & ")))
;;(oll-table-row)

(cl-defun parse-oll-csv (filename)
  (let ((res (cl-remove (list "")
	       (split-if #'empty-string-p
		 (remove-if (bind #'string-match "^#" 1)
		   (file-lines filename)))
	       :test #'equal)))
    (cons (car res) (mapcar #'rest (cdr res)))))
;;(mapcar #'length (parse-oll-csv "oll_algorithms.csv"))

(cl-defun oll-table (lines)
  (awhen (mapcar #'(lambda (x) (split-string x ";" nil " ")) lines)
    (concat* it
      :pre "\\begin{tabular}{c l l l l}\n% & Name & Algorithm & Anti & Reverse\\\\[0.3cm]\n"
      :key (bind #'oll-table-row it)
      :in "\n \\\\[0.7cm]\n\n"
      :suf "\n\\end{tabular}")))
;;(oll-table (third (parse-oll-csv "oll_algorithms.csv")))
;;(parse-csv-file "oll_algorithms.csv" ";" nil t " *")
;;(split-string " qwe ; ewq ; " ";" t " ")

(defconst +oll-home+ "~/projects/utils/elisp/games/cube")

(cl-defun oll-doc (&optional (filename (expand-file-name "oll_algorithms.csv" +oll-home+)))
  (concat* (parse-oll-csv filename)
    :pre "\\begin{document}\n"
    :key #'oll-table
    :in "\n\n"
    :suf "\n\\end{document}\n"))
;;(oll-doc)

(provide 'oll)
