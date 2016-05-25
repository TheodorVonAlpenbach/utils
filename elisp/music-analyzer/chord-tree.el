;;(require 'spitch)
;;(require 'chrome-set-pitch-class)
(require 'mu-general)
(require 'gchord)
(require 'segmentation)

(defun dch-valid-schord (schord)
  (awhen (schosk-info-chord-type (schordx-to-schosk-info (schord-to-schordx schord)))
    (case it
      ((major-ninth-suspension minor-ninth-suspension suspended-triad) nil)
      (t t))))
;;(qwe '(0 1 7))

(defun filter-schords (schords)
  (copy-if #'dch-valid-schord schords))
;;(filter-schords '((0 1 7) (0 3 7)))

(defun* psegment (from &optional (length 1))
  (let* ((to (+ from length))
	(mvt (mvt-copy (mvt-submovement (mvt-test) from to)))
	(seg (segmentation mvt 'dense))
	(schords (schords-filter (schords seg)))
	(stree (dch-tree-from-schords schords))
	(tree (dch-sc-to-chrome stree)))
    ;;(movement-to-lilypond seg :title (format "Dense, bars %d--%d" from to))
    (dch-tree-view tree)
    tree))
;;(psegment 1 15)
;;(movement-to-lilypond (mvt-copy (mvt-submovement (mvt-test) 1 2)) :title "qwe")

(defvar dch-modulation-span '(:maximum :minium)
  "maximum: the whole modulation greyzone should be marked with both keys
minium: only SD should be marked")

(defun dch-node-dominantic-p (n &optional recursively)
  (and n (or (eq (second n) 'dominantic)
	     (and recursively 
		  (eq (second n) 'equal)
		  (or (dch-node-dominantic-p (first (third n)) t)
		      (dch-node-dominantic-p (second (third n)) t))))))

(defun dch-node-subdominantic-p (n &optional recursively)
  (and n (or (find (second n) '(neapolitan subdominantic))
	     (and recursively
		  (eq (second n) 'equal)
		  (or (dch-node-subdominantic-p (first (third n)) t)
		      (dch-node-subdominantic-p (second (third n)) t))))))

(defun scs-relation (sc1 sc2)
   (cond 
   ((equal (sc-root-position sc1) (sc-root-position sc2)) 'equal)
   ((sc-dominantic-relation-p sc1 sc2) 'dominantic)
   ((sc-neapolitan-relation-p sc1 sc2) 'neapolitan)
   ((sc-subdominantic-relation-p sc1 sc2) 'subdominantic)))

(defun node-relation (n1 n2)
  (let ((relation (schord-relation (first n1) (first n2))))
    (case relation ;;implement acase?
      ((equal dominantic) relation)
      ((neapolitan subdominantic) (and (dch-node-dominantic-p n2) relation)))))

(defun dch-stack-state (stack)
  (if (< (length stack) 2)
    'stack-too-short
    (node-relation (second stack) (first stack))))

(defun schosk-remove-seventh (schosk)
  (copy-list (butlast schosk)))
;;(schosk-remove-seventh '(4 7 10))

(defun schordx-remove-seventh (schordx)
  (let* ((res (schordx-copy schordx))
	 (new-schosk (schosk-remove-seventh (schordx-schosk schordx))))
    (setf (schordx-schosk res) new-schosk)
    res))
;;(schordx-remove-seventh (schordx-new '(4 7 10) 1 2))

(defun schord-remove-seventh (schord)
  (schord-from-schordx (schordx-remove-seventh (schord-to-schordx schord))))
;;(schordx-remove-seventh (schordx-new '(1 4 7 10)))

(defun dch-reduce-stack (stack)
  "Reduces stack as much as possible applying unary and binary
rules"
  (let ((state (dch-stack-state stack)))
    ;; unary
    (when (find state '(seventh))
      (let ((new-stack-element (list (dch-remove-seventh (first (first stack)))
				     state
				     (list (second stack)))))
	(pop* stack 2)
	(push new-stack-element stack)
	(setq stack (dch-reduce-stack stack))))
    ;; binary
    (when (find state '(equal dominantic neapolitan subdominantic))
      (let ((new-stack-element (list (first (first stack))
				     state
				     (list (second stack) (first stack)))))
	(pop* stack 2)
	(push new-stack-element stack)
	(setq stack (dch-reduce-stack stack))))
    stack))

(defun dch-make-tree (stack set-chords)
  "Starts with an empty STACK and a list of SET-CHORDS.
Successively feeds stack with new chords until SET-CHORDS is
empty and STACK can't be reduced anymore"
  (while set-chords
    (push (pop set-chords) stack)
    (setq stack (dch-reduce-stack stack)))
  (nreverse stack))

(defun dch-tree-from-schords (schords)
  (dch-make-tree '() (mapcar (bind #'list 'leaf) schords)))

(defun dch-tree-from-chord-strings (chord-strings)
  (dch-tree-from-schords (mapcar #'schord-from-string test-chords)))
;;(dch-tree-from-chord-strings test-chords)

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

(lexical-let ((tree nil))
  (defun dch-test-tree (&optional reset)
    (when (or reset (not tree))
      (setq tree 
	    (dch-make-tree '() (mapcar (compose (bind #'list 'leaf) #'schord-from-string)
				       test-chords))))
      tree))
;;(prin1 (dch-test-tree))

(defun dot-node-base (identifier properties)
  "Creates a dot node definition string. PROPERTIES is an alist
where each element defines a dot node property \(name . value)"
  (format "%s [%s];" identifier
	  (concat* properties :in "," :key #'(lambda (p) 
					       (format "%s=\"%s\"" (car p) (cdr p))))))
;;(dot-node-base 'n '((label . "nodeName") (fillcolor . yellow) (style . filled)))

(defun* dot-node (identifier &optional name color (style "filled"))
  (dot-node-base identifier (list (cons 'label name)
				  (cons 'fillcolor color)
				  (cons 'style style))))
;;(dot-node 'n "nodeName" 'yellow)

(defun dch-dot-node-definition (node identifier)
  (let ((name (gchord-to-string (first node) 'english-chord))
	(color (cond ((dch-node-dominantic-p node) 'yellow)
		     ((dch-node-subdominantic-p node) 'green)
		     ((dch-node-subdominantic-p node t) 'greenyellow)
		     ((dch-node-dominantic-p node t) 'lightyellow)
		     (t (message "%S" (second node)) ""))))
    (dot-node identifier name color)))

(defun dch-tree-to-dot-statements-1 (tree node-identifier)
  "Converts tree to a list of .dot statements"
  (loop with children = (third tree)
	for child in children
	for char from ?A
	for child-identifier = (format "%s%c" node-identifier char)
	for child-name = (gchord-to-string (first child) 'english-chord)
	for child-definition-statement = (format "%s [label=\"%s\"];" child-identifier child-name)
	for node-to-child-statement = (format "%s -> %s" node-identifier child-identifier)
;;	collect child-definition-statement
	collect (dch-dot-node-definition child child-identifier)
	collect node-to-child-statement
	append (dch-tree-to-dot-statements-1 child child-identifier)))

(defun dch-tree-to-dot-statements (tree)
  "TODO: This method is now ridicously similar to its *-1 version."
  (loop for child in tree
	for char from ?A
	for child-identifier = (format "n%d" char)
	for child-name = (gchord-to-string (first child) 'english-chord)
	for child-definition-statement = (format "%s [label=\"%s\"];" child-identifier child-name)
;;	collect child-definition-statement
	collect (dch-dot-node-definition child child-identifier)
	append (dch-tree-to-dot-statements-1 child child-identifier)))

(defun dch-tree-to-dot-string (tree)
  (concat* (dch-tree-to-dot-statements tree) :pre "digraph g {\n" :in "\n" :suf "\n}"))
;;(dch-tree-to-dot-string '(((7 10 2) dominant (((2 6 9 0) leaf) ((7 10 2) leaf)))))
;;(string-to-file (dch-tree-to-dot-string '(((7 10 2) dominant (((2 6 9 0) leaf) ((7 10 2) leaf))))) "c:/Documents and Settings/matsb/My Documents/data/dot/graph3.gv")

(defun dch-tree-view (tree)
  (dot-view (dch-tree-to-dot-string tree)))
;;(dch-tree-view '(((7 10 2) dominant (((2 6 9 0) leaf) ((7 10 2) leaf)))))
;;(dch-tree-view (dch-test-tree))
;;(dch-tree-to-dot-string (dch-test-tree))

;;;; dot utils
(defconst *dot-dir* (concat *local-data-dir* "dot/"))

(defun dot-tmp-path (dot-string)
  (concat temporary-file-directory (md5 dot-string)))

(defun dot-file-to-pgn (path)
  (let ((res (call-process "dot" nil "*qwe*" nil path "-Tpng" "-O")))
    (if (zerop res)
      (concat path ".png")
      (error "Couldn't compile .dot file %s. See *qwe* for reason." path))))
;;(dot-file-to-pgn "c:/Users/Theodor/Documents/data/dot/test.dot")

(defun* dot-to-png (dot-string &optional (tmp-path (dot-tmp-path dot-string)))
  "Returns path to generated PNG file"
  (string-to-file dot-string tmp-path)
  (let ((res (call-process "dot" nil "*qwe*" nil tmp-path "-Tpng" "-O")))
    (if (zerop res)
      (concat tmp-path ".png")
      (error "Couldn't compile .dot file %s. See *qwe* for reason." tmp-path))))
;;(dot-to-png "")

(defun png-view (filename)
  (browse-url filename))
;;(png-view "c:/Users/Theodor/Documents/data/dot/test.dot.png")
(defun pdf-view (filename)
  (browse-url filename))

(defun* dot-view (dot-string &optional (tmp-path (dot-tmp-path dot-string)))
  (png-view (dot-to-png dot-string tmp-path)))
;;(dot-view (file-to-string (concat *local-data-dir* "dot/test.dot")))

;;;; Conversion to chromes (pitch classes)
(defun dch-sc-to-chrome-node (sc-node parent-chord)
  "Primitive first version. Only root of parent chord is used.
Maybe it is enough, but to deduce a reference key, not only a
reference chrome, would be more general."
  (let ((chord (schord-to-chord (first sc-node) (chord-root parent-chord))))
    (list chord
	  (second sc-node) ;;keep the functional relation
	  (mapcar (bind #'dch-sc-to-chrome-node chord) (third sc-node)))))

(defun* dch-sc-to-chrome (tree &optional (reference-chrome-chord (chord-from-string "C")))
  (loop with parent-chord = reference-chrome-chord
	for n in tree
	for chrome-n = (dch-sc-to-chrome-node n reference-chrome-chord)
	for parent-chord = (first chrome-n)
	collect chrome-n))
;;(dch-sc-to-chrome (dch-test-tree))
;;(dch-tree-view (dch-sc-to-chrome (dch-test-tree)))

(defun dch-is-leaf-p (dch-node)
  (eq (second dch-node) 'leaf))

(defun dch-tree-extract-leaves (tree)
  (if (dch-is-leaf-p tree)
    (list (first tree))
    (append (dch-tree-extract-leaves (first (third tree)))
	    (dch-tree-extract-leaves (second (third tree))))))
;;(dch-tree-extract-leaves (first (dch-test-tree)))

(defun* dch-chords (tree &optional (reference-chrome-chord (chord-from-string "C")))
  (let ((tree* (dch-sc-to-chrome tree reference-chrome-chord)))
    (apply #'conc (mapcar (bind #'chords-to-string 'english-chord) (mapcar #'dch-tree-extract-leaves tree*)))))
;;(dch-chords (dch-test-tree))

;;; 1. When tree is finished. The next task is to define tonal
;;; segments.

;;; 2. Then, to each set-chord, a tonality is applied

;;; 3. Then, each set-chord is transformed into a chrome-chord based on
;;; given tonality

;;; 4. Also modulations are provided this way, see dch-modulation-span
;;; for how to express modulation spans. Eg. C=0 F G C G Am Em C D G=9
;;; (tonalities ((C 0 8) (G 7 10))) ;;left span (former modulation covers intermediate notes; this is default)
;;; (tonalities ((C 0 3) (G 4 10))) ;;right span (latter modulation covers intermediate notes)
;;; (tonalities ((C 0 8) (G 4 10))) ;;maximum span (both modulations cover intermediate notes)

(provide 'deduce-chromes)
