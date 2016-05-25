(require 'gchord)
(require 'chord-skeleton)

(defconst test-chords
  (list 
   ;;first part
   "D" "Gm" "D" "Gm" "Gm" "D" "Gm" "D" 
   "Ab" "C#d7" "D" "Gm" 
   "D7" "Gm"
   ;; second part
   "Gm" "A" "Dm" "A" "Dm" "Dm" "Gm" "A" "Dm" 
   "Eb" "Ad7" "Bb" "Ed7" "F" "Ad7" 
   "BbA" "Gm" "Cm" "D" "Gm" "D"
   "Gm" "Cm" "G7" "Cm" "Gm" "A" "Dm" "A" "D"
   )
  )

;;(mapcar #'schord-from-string test-chords)
(defun gchords-from-strings (strings)
  (mapcar (bind #'gch-from-string 'schordx) strings))
;;(gchords-from-strings test-chords)

(defun gch-from-nchord (nchord)
  (let ((n (first nchord)))
    (gch-new (schordx-from-nchord nchord) (n-start-time n) (d-copy (n-duration n)))))
;;(gch-from-nchord (first (nchords (mvt-test))))

(defun* gchords-from-mvt (&optional (mvt (mvt-test)))
  "mvt -> segmented mvt -> nchords -> gchords"
  (mapcar #'gch-from-nchord (nchords (mb-segmentation (mvt-copy mvt)))))
;;(length (gchords-from-mvt))

;;; this should be moved to modulation
(defun gnode-dominantic-p (n &optional recursively)
  (and n (or (eq (second n) 'dominantic)
	     (and recursively 
		  (eq (second n) 'equal)
		  (or (gnode-dominantic-p (first (third n)) t)
		      (gnode-dominantic-p (second (third n)) t))))))

(defun gnode-subdominantic-p (n &optional recursively)
  (and n (or (find (second n) '(neapolitan subdominantic))
	     (and recursively
		  (eq (second n) 'equal)
		  (or (gnode-subdominantic-p (first (third n)) t)
		      (gnode-subdominantic-p (second (third n)) t))))))

(defun gnode-relation (n1 n2)
  "If a valid relation exists between N1 and N2 this is returned.
Otherwise nil is returned"
  (let ((relation (gch-relation (first n1) (first n2))))
    (case relation ;;implement acase?
      ((equal dominantic) 
       relation)
      ((neapolitan subdominantic)
       (when (gnode-dominantic-p n2)
	 relation)))))

(defun gtree-stack-state (stack)
  (if (< (length stack) 2)
    'stack-too-short
    (gnode-relation (second stack) (first stack))))

(defun gtree-reduce-stack (stack)
  "Reduces stack as much as possible applying unary and binary
rules"
  (let ((state (gtree-stack-state stack)))
    ;; unary
    (when (find state '(seventh))
      (let ((new-stack-element (list (gtree-remove-seventh (first (first stack)))
				     state
				     (list (second stack)))))
	(pop* stack 2)
	(push new-stack-element stack)
	(setq stack (gtree-reduce-stack stack))))
    ;; binary
    (when (find state '(equal dominantic neapolitan subdominantic))
      (let ((new-stack-element (list (first (first stack))
				     state
				     (list (second stack) (first stack)))))
	(pop* stack 2)
	(push new-stack-element stack)
	(setq stack (gtree-reduce-stack stack))))
    stack))

(defun gtree-build (stack set-chords)
  "Starts with an empty STACK and a list of SET-CHORDS.
Successively feeds stack with new chords until SET-CHORDS is
empty and STACK can't be reduced anymore"
  (while set-chords
    (push (pop set-chords) stack)
    (setq stack (gtree-reduce-stack stack)))
  (nreverse stack))

(defun gtree (gchords)
  ""
  (gtree-build '() (mapcar (bind #'list 'leaf) gchords)))
;;(gtree (gchords-from-mvt (mvt-test)))

(defun gtree-from-chord-strings (chord-strings)
  (gtree (gchords-from-strings chord-strings)))
;;(gtree-from-chord-strings test-chords)

(lexical-let ((tree nil))
  (defun* gtree-test (&key source start end)
    (when (or source start end (not tree))
      (let ((gchords (if (eq source :strings)
		       (gchords-from-strings test-chords)
		       (gchords-from-mvt))))
	(setq tree (gtree (subseq gchords (or start 0) end)))))
    tree))
;;(prin1 (gtree-test :start 8 :end 10))

;; the next two methods should be simplified
(defun gnode-to-dot (gnode identifier)
  (let ((name (gch-to-string (first gnode) 'english-chord))
	(color (cond ((gnode-dominantic-p gnode) 'yellow)
		     ((gnode-subdominantic-p gnode) 'green)
		     ((gnode-subdominantic-p gnode t) 'greenyellow)
		     ((gnode-dominantic-p gnode t) 'lightyellow)
		     (t (message "%S" (second gnode)) ""))))
    (dot-node identifier name color)))

(defun gtree-to-dot-1 (gnode node-identifier)
  "Converts sub tree starting at GNODE to a list of .dot
statements"
  (loop with children = (third gnode)
	for child in children
	for char from ?A
	for child-identifier = (format "%s%c" node-identifier char)
	for child-name = (gch-to-string (first child) 'english-chord)
	for child-definition-statement = (format "%s [label=\"%s\"];" child-identifier child-name)
	for node-to-child-statement = (format "%s -> %s" node-identifier child-identifier)
	collect (gnode-to-dot child child-identifier)
	collect node-to-child-statement
	append (gtree-to-dot-1 child child-identifier)))

(defun gtree-to-dot-0 (gtree)
  "Converts GTREE to a list of .dot statements"
  (loop for child in gtree
	for char from ?A
	for child-identifier = (format "n%d" char)
	for child-name = (gch-to-string (first child) 'english-chord)
	for child-definition-statement = (format "%s [label=\"%s\"];" child-identifier child-name)
	collect (gnode-to-dot child child-identifier)
	append (gtree-to-dot-1 child child-identifier)))

(defun gtree-to-dot (gtree)
  "Converts GTREE to a list of .dot statements"
  (concat* (gtree-to-dot-0 gtree) :pre "digraph g {\n" :in "\n" :suf "\n}"))
;;(gtree-to-dot (gtree-test))
;;(gtree-tree-to-dot '(((7 10 2) dominant (((2 6 9 0) leaf) ((7 10 2) leaf)))))
;;(string-to-file (gtree-tree-to-dot-string '(((7 10 2) dominant (((2 6 9 0) leaf) ((7 10 2) leaf))))) "c:/Documents and Settings/matsb/My Documents/data/dot/graph3.gv")

(require 'dot)
(defun* gtree-view (tree &optional filename)
  (dot-view (gtree-to-dot tree) filename))
;;(gtree-view (gtree-test :start 8 :end 11))
;;(gtree-view '(((7 10 2) dominant (((2 6 9 0) leaf) ((7 10 2) leaf)))))

;;;; Conversion to chromes (pitch classes)
(defun gtree-schordx-to-chordx-1 (gnode parent-chordx)
  "Primitive first version. Only root of parent chord is used.
Maybe it is enough, but to deduce a reference key, not only a
reference chrome, would be more general."
  (let* ((gchord (first gnode)) 
	 (chordx (schordx-to-chordx (gch-chord gchord) (chordx-root parent-chordx))))
    (list (gch-copy gchord :chord chordx)
	  (second gnode) ;;keep the functional relation
	  (mapcar (bind #'gtree-schordx-to-chordx-1 chordx) (third gnode)))))

(defun* gtree-schordx-to-chordx (gtree &optional (reference-chrome-chordx (chordx-from-string "C")))
  (loop with parent-chord = reference-chrome-chordx
	for n in gtree
	for chrome-n = (gtree-schordx-to-chordx-1 n reference-chrome-chordx)
	for parent-chord = (first chrome-n)
	collect chrome-n))
;;(gtree-schordx-to-chordx (gtree-test))
;;(gtree-view (gtree-schordx-to-chordx (gtree-test)))

(defun gtree-is-leaf-p (gtree-node)
  (eq (second gtree-node) 'leaf))

(defun gtree-leaves-1 (tree)
  (if (gtree-is-leaf-p tree)
    (list (first tree))
    (append (gtree-leaves-1 (first (third tree)))
	    (gtree-leaves-1 (second (third tree))))))

(defun gtree-leaves (tree)
  (loop for n in tree
	append (gtree-leaves-1 n)))
;;(gtree-leaves (gtree-test))


(defun schordx-adjust-dim7 (x y)
  "X and Y are schordxs"
  (let* ((target-root (spc-transpose (schordx-root y) -1))
	(d (/ (mod (- (schordx-root x) target-root) 12) 3))
	(res (schordx-copy x)))
    (setf (schordx-root res) target-root)
    (setf (schordx-inversion res) 
	  (mod (+ (schordx-inversion res) d) 4))
    res))
;;(schordx-adjust-dim7 '((3 6 9) 1 0) `((3 6 9) 0 ,(1+ 6)))

(defun schordx-adjust-aug (x y)
  "X and Y are schordxs"
  (let* ((target-root (spc-transpose (schordx-root y) -5))
	(d (/ (mod (- (schordx-root x) target-root) 12) 4))
	(res (schordx-copy x)))
    (setf (schordx-root res) target-root)
    (setf (schordx-inversion res) 
	  (mod (+ (schordx-inversion res) d) 3))
    res))
;;(schordx-adjust-aug '((4 8) 1 0) `((4 8) 0 ,(1+ 4)))

(defun gchords-adjust-dim7-aug (gchords)
  "Assumes gchords are schordx based"
 (loop for gg in (pairs gchords)
       for gch1 = (first gg)
       for gch2 = (second gg)
       for schordx1 = (gch-chord gch1)
       for schordx2 = (gch-chord gch2)
       for ct = (schordx-type schordx1)
       if (eq ct 'diminished-seventh)
       do (setf (gch-chord gch1) (schordx-adjust-dim7 schordx1 schordx2))
       if (eq ct 'augmented-triad)
       do (setf (gch-chord gch1) (schordx-adjust-aug schordx1 schordx2)))
 gchords)
;;(gchords-adjust-dim7-aug gchords-spc)

(defun* gchord-at (start-time gchords)
  (aif (find start-time gchords :key #'gch-start-time :from-end t :test #'>=)
    (when (> (gch-end-time it) start-time)
      it)))
;;(gchord-at .9 (gtree-leaves (gtree-test)))

(defun key-deduce (gchords)
  (let* ((chordxs (mapcar #'gch-chord gchords))
	(groups (distribute chordxs #'(lambda (x y)
					(and (equal (chordx-chosk x) (chordx-chosk y))
					     (equal (chordx-root x) (chordx-root y))))))
	(best (first (sort* groups #'> :key #'length)))
	(mode (if (eq (first (chordx-chosk best)) 'm3)
		'minor 'major))
	(root (chordx-root best)))
    (k-new root mode)))
;;(key-deduce gchords)

(defun n-modify (n gchords)
  "Assume gchords are of type 'chordx"
  (let* ((gchord (gchord-at (n-start-time n) gchords))
	 (chordx (gch-chord gchord))
	 (chord (chord-from-chordx chordx))
	 (chrome (find (chrome-to-spitch (n-chrome n)) chord :key #'chrome-to-spitch)))
    (when chrome 
      (setf (n-chrome n) chrome))
    n))
;;(n-modify (new))

(defun mvt-modify (mvt gchords)
  "Assume gchords are of type 'chordx"
  (let ((mvt* (mvt-copy mvt))
	(key (key-deduce gchords)))
    (mapc (bind #'n-modify gchords) (notes mvt*))
    (mvt-set-key mvt* key)
    mvt*))
;;(mvt-modify mvt (gtree-leaves (gtree-schordx-to-chordx (gtree-test))))

;;(mb-segmentation (mvt-copy (mvt-test)))

(provide 'gtree)
