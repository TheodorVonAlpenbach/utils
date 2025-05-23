;;(require 'spitch)
;;(require 'chrome-set-pitch-class)
(require 'mu-general)
(require 'gchord)
(require 'segmentation)

(setq dch-doc "
 Goal: parse a Bach choral from MIDI and try to find the correct chrome values from set-pitch-classes

 1* find segmentation on schords of the movement (handle passing notes etc)
   * must somehow keep track the original durations
 2o from the chord segment list, construct a tonality tree (i.e. a hierachical segmentation on tonality)
 3o convert all schords in tree to chords. Since all intermediate chords should be either pure major or
    minor, the local tonality keys are trivially identified.
 4o print the resulting tree using dot
 5  how to give an ordinary Riemann analysis from tree? Once key is established the rest is simple:
    A. Only leaf nodes of the tree is by tree definition segmented chords, and only those are given 
    a Riemann function according to the tonality of their respective parent node. 
    B. Traverse tree and identify overlapping tonicizations. Use rules overlapping rules 
    (max, min-left, min-right) to settle overlapping function output 
    But how to establish key?
    Approach 1
    All strictly dominant nodes inherit tonality from closest clause node (subdominatal)
    Approach 2 
    Same as 1, but also considers
    a. Big dominant subtree (must define /big/)
    b. Abrupt chrome key changes
    c. Picardic tonicizations
    d. Chuch modes

 1 is difficult, but we start with a /dense segmentation/ (ref. Rohrmeier), 
   just to have something to show. We probably just discard non-chords at first.
   Later we could the /harmonic segmentation/ described by Rohrmeier
 
all D->T relations. Those could
 be T->SD but we may apply secondary rules to accomodate this

Example (from 2-6):
 (D g D g g D g D As cis0 D g D g)
stack:
 D
 D g
 (g* 
  (D g))
 (g* 
  (D g)) D g
 (g* 
  (D g)) (g* (D g))
 (g* 
  (g* (D g)) (g* (D g))) etc 

Example choral:
http://www.jsbchorales.net/down/pdf/000206.pdf
Example dot file:
c:/Documents and Settings/matsb/My Documents/data/dot/graph1.gv
")

;;;; Segmentation
;;(setf sm (mvt-submovement (mvt-test) 1 4))

(cl-defun dch-valid-schord (schord)
  (awhen (schosk-info-chord-type (schordx-to-schosk-info (schord-to-schordx schord)))
    (cl-case it
      ((major-ninth-suspension minor-ninth-suspension suspended-triad) nil)
      (t t))))
;;(qwe '(0 1 7))

(cl-defun filter-schords (schords)
  (copy-if #'dch-valid-schord schords))
;;(filter-schords '((0 1 7) (0 3 7)))

(cl-defun psegment (from &optional (length 1))
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

(cl-defun dch-node-dominantic-p (n &optional recursively)
  (and n (or (eq (second n) 'dominantic)
	     (and recursively 
		  (eq (second n) 'equal)
		  (or (dch-node-dominantic-p (first (third n)) t)
		      (dch-node-dominantic-p (second (third n)) t))))))

(cl-defun dch-node-subdominantic-p (n &optional recursively)
  (and n (or (cl-find (second n) '(neapolitan subdominantic))
	     (and recursively
		  (eq (second n) 'equal)
		  (or (dch-node-subdominantic-p (first (third n)) t)
		      (dch-node-subdominantic-p (second (third n)) t))))))

(cl-defun scs-relation (sc1 sc2)
   (cond 
   ((equal (sc-root-position sc1) (sc-root-position sc2)) 'equal)
   ((sc-dominantic-relation-p sc1 sc2) 'dominantic)
   ((sc-neapolitan-relation-p sc1 sc2) 'neapolitan)
   ((sc-subdominantic-relation-p sc1 sc2) 'subdominantic)))

(cl-defun node-relation (n1 n2)
  (let ((relation (schord-relation (first n1) (first n2))))
    (cl-case relation ;;implement acase?
      ((equal dominantic) relation)
      ((neapolitan subdominantic) (and (dch-node-dominantic-p n2) relation)))))

(cl-defun dch-stack-state (stack)
  (if (< (length stack) 2)
    'stack-too-short
    (node-relation (second stack) (first stack))))

(cl-defun schosk-remove-seventh (schosk)
  (copy-list (butlast schosk)))
;;(schosk-remove-seventh '(4 7 10))

(cl-defun schordx-remove-seventh (schordx)
  (let* ((res (schordx-copy schordx))
	 (new-schosk (schosk-remove-seventh (schordx-schosk schordx))))
    (setf (schordx-schosk res) new-schosk)
    res))
;;(schordx-remove-seventh (schordx-new '(4 7 10) 1 2))

(cl-defun schord-remove-seventh (schord)
  (schord-from-schordx (schordx-remove-seventh (schord-to-schordx schord))))
;;(schordx-remove-seventh (schordx-new '(1 4 7 10)))

(cl-defun dch-reduce-stack (stack)
  "Reduces stack as much as possible applying unary and binary
rules"
  (let ((state (dch-stack-state stack)))
    ;; unary
    (when (cl-find state '(seventh))
      (let ((new-stack-element (list (dch-remove-seventh (first (first stack)))
				     state
				     (list (second stack)))))
	(pop* stack 2)
	(push new-stack-element stack)
	(setq stack (dch-reduce-stack stack))))
    ;; binary
    (when (cl-find state '(equal dominantic neapolitan subdominantic))
      (let ((new-stack-element (list (first (first stack))
				     state
				     (list (second stack) (first stack)))))
	(pop* stack 2)
	(push new-stack-element stack)
	(setq stack (dch-reduce-stack stack))))
    stack))

(cl-defun dch-make-tree (stack set-chords)
  "Starts with an empty STACK and a list of SET-CHORDS.
Successively feeds stack with new chords until SET-CHORDS is
empty and STACK can't be reduced anymore"
  (while set-chords
    (push (pop set-chords) stack)
    (setq stack (dch-reduce-stack stack)))
  (nreverse stack))

(cl-defun dch-tree-from-schords (schords)
  (dch-make-tree '() (mapcar (bind #'list 'leaf) schords)))

(cl-defun dch-tree-from-chord-strings (chord-strings)
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

(let ((tree nil))
  (cl-defun dch-test-tree (&optional reset)
    (when (or reset (not tree))
      (setq tree 
	    (dch-make-tree '() (mapcar (compose (bind #'list 'leaf) #'schord-from-string)
				       test-chords))))
      tree))
;;(prin1 (dch-test-tree))

(cl-defun dot-node-base (identifier properties)
  "Creates a dot node definition string. PROPERTIES is an alist
where each element defines a dot node property \(name . value)"
  (format "%s [%s];" identifier
	  (concat* properties :in "," :key #'(lambda (p) 
					       (format "%s=\"%s\"" (car p) (cdr p))))))
;;(dot-node-base 'n '((label . "nodeName") (fillcolor . yellow) (style . filled)))

(cl-defun dot-node (identifier &optional name color (style "filled"))
  (dot-node-base identifier (list (cons 'label name)
				  (cons 'fillcolor color)
				  (cons 'style style))))
;;(dot-node 'n "nodeName" 'yellow)

(cl-defun dch-dot-node-definition (node identifier)
  (let ((name (gchord-to-string (first node) 'english-chord))
	(color (cond ((dch-node-dominantic-p node) 'yellow)
		     ((dch-node-subdominantic-p node) 'green)
		     ((dch-node-subdominantic-p node t) 'greenyellow)
		     ((dch-node-dominantic-p node t) 'lightyellow)
		     (t (message "%S" (second node)) ""))))
    (dot-node identifier name color)))

(cl-defun dch-tree-to-dot-statements-1 (tree node-identifier)
  "Converts tree to a list of .dot statements"
  (cl-loop with children = (third tree)
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

(cl-defun dch-tree-to-dot-statements (tree)
  "TODO: This method is now ridicously similar to its *-1 version."
  (cl-loop for child in tree
	for char from ?A
	for child-identifier = (format "n%d" char)
	for child-name = (gchord-to-string (first child) 'english-chord)
	for child-definition-statement = (format "%s [label=\"%s\"];" child-identifier child-name)
;;	collect child-definition-statement
	collect (dch-dot-node-definition child child-identifier)
	append (dch-tree-to-dot-statements-1 child child-identifier)))

(cl-defun dch-tree-to-dot-string (tree)
  (concat* (dch-tree-to-dot-statements tree) :pre "digraph g {\n" :in "\n" :suf "\n}"))
;;(dch-tree-to-dot-string '(((7 10 2) dominant (((2 6 9 0) leaf) ((7 10 2) leaf)))))
;;(string-to-file (dch-tree-to-dot-string '(((7 10 2) dominant (((2 6 9 0) leaf) ((7 10 2) leaf))))) "c:/Documents and Settings/matsb/My Documents/data/dot/graph3.gv")

(cl-defun dch-tree-view (tree)
  (dot-view (dch-tree-to-dot-string tree)))
;;(dch-tree-view '(((7 10 2) dominant (((2 6 9 0) leaf) ((7 10 2) leaf)))))
;;(dch-tree-view (dch-test-tree))
;;(dch-tree-to-dot-string (dch-test-tree))

;;;; dot utils
;;;; Conversion to chromes (pitch classes)
(cl-defun dch-sc-to-chrome-node (sc-node parent-chord)
  "Primitive first version. Only root of parent chord is used.
Maybe it is enough, but to deduce a reference key, not only a
reference chrome, would be more general."
  (let ((chord (schord-to-chord (first sc-node) (chord-root parent-chord))))
    (list chord
	  (second sc-node) ;;keep the functional relation
	  (mapcar (bind #'dch-sc-to-chrome-node chord) (third sc-node)))))

(cl-defun dch-sc-to-chrome (tree &optional (reference-chrome-chord (chord-from-string "C")))
  (cl-loop with parent-chord = reference-chrome-chord
	for n in tree
	for chrome-n = (dch-sc-to-chrome-node n reference-chrome-chord)
	for parent-chord = (first chrome-n)
	collect chrome-n))
;;(dch-sc-to-chrome (dch-test-tree))
;;(dch-tree-view (dch-sc-to-chrome (dch-test-tree)))

(cl-defun dch-is-leaf-p (dch-node)
  (eq (second dch-node) 'leaf))

(cl-defun dch-tree-extract-leaves (tree)
  (if (dch-is-leaf-p tree)
    (list (first tree))
    (append (dch-tree-extract-leaves (first (third tree)))
	    (dch-tree-extract-leaves (second (third tree))))))
;;(dch-tree-extract-leaves (first (dch-test-tree)))

(cl-defun dch-chords (tree &optional (reference-chrome-chord (chord-from-string "C")))
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
