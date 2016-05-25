;; Bedre graffremstilling
;; treffe igjen 2013-04-29 16:00
;; Hamburg: 12. mai, 

;;TODO
;; * in dch use schordxs instead of schords
;; * fix D7 -> D
;; * fix root of dims
;; * D73, A75, Am7-5, DA-7
;; dch is too big:
;; gchord-tree.el or something
;; dch.el uses tree
;; funtional-analysis.el bruker tree
;; dot.el

;;;; Demo 3 (2013-04-08)
(require 'gtree)
(require 'dot)

;; The bach setting
;(pdf-view (concat *mb-lisp-dir* "midi/data/000206b_.pdf"))

;; Naive midi extraction

;(setq demo-mvt (mvt-copy (mvt-test t)))
;(movement-to-lilypond demo-mvt :title "Midi extraction" :file (concat *local-data-dir* "ly/midi.ly") :start t :view nil)

;(setq segmented-mvt (mb-segmentation (mvt-copy demo-mvt)))
;(movement-to-lilypond segmented-mvt :title "My segmentation" :file (concat *local-data-dir* "ly/mb-segmentation.ly") :start t :view nil)

;; todo: where comes gtree-test from?
;(setq gtree-spc (gtree-test :start 0))

;(setf gchords-spc (gtree-leaves gtree-spc))
;(setf gchords-spc (gchords-adjust-dim7-aug (gtree-leaves gtree-spc)))
;(gtree-view gtree-spc)

;(setq gtree-pc (gtree-schordx-to-chordx (gtree-test)))
;(gtree-view gtree-pc)

;(setq gchords (gtree-leaves gtree-pc))

;(setf final-mvt (mvt-modify demo-mvt gchords))

;;(movement-to-lilypond final-mvt :title "Final result" :file (concat *local-data-dir* "ly/final.ly") :start t :view nil)

;;(movement-to-lilypond (mvt-submovement final-mvt 2 4) :title "Difficult voice" :file (concat *local-data-dir* "ly/difficult.ly") :start t :view nil)

(defun chrome-from-spc (spc &optional pcb)
  (if pcb
    (let* ((pc (chrome-new pcb 0))
	   (pc-spc (chrome-to-spitch pc)))
      (setf (chrome-accidentals pc) (- spc pc-spc))
      pc)
    (spc-to-chrome spc)))
;;(chrome-from-spc 8 5)

(defun chrome-sans-accidentals (chrome)
  (chrome-new (chrome-base chrome)))
;;(chrome-sans-accidentals (chrome-new 4 1))

(defun* notes-deduce-pitches (notes &optional (key (k-from-string "G minor")))
  (let ((scale-pcs (flatten (k-scale key))))

    ;; first simple matching with key
    (loop for n in notes
	  for chrome = (n-chrome n)
	  for spc = (chrome-to-spitch chrome)
	  for x = (find spc scale-pcs :key #'chrome-to-spitch)
	  if x do (setf (n-chrome n) x))

    ;; chromatic pairs
    (loop for 2n in (pairs notes)
	  for n1 = (first 2n)
	  for n2 = (second 2n)
	  for pc1 = (n-chrome n1)
	  for pc2 = (n-chrome n2)
	  for spc1 = (chrome-to-spitch pc1)
	  for spc2 = (chrome-to-spitch pc2)
	  for x1 = (find spc1 scale-pcs :key #'chrome-to-spitch)
	  for x2 = (find spc2 scale-pcs :key #'chrome-to-spitch)
	  for pcb1 = (chrome-base pc1)
	  for pcb2 = (chrome-base pc2)

	  ;;upwards
	  if (= (mod (- spc2 spc1) 12)
		1)
	  do (if (and x2 (not x1))
	       (setf (n-chrome n1) (chrome-from-spc spc1 (mod (1- pcb2) 7)))
	       (if (and x1 (not x2))
		 (setf (n-chrome n2) (chrome-from-spc spc2 (mod (1+ pcb1) 7)))))

	  ;;downwards
	  if (= (mod (- spc1 spc2) 12) 
		1)
	  do (if (and x2 (not x1))
	       (setf (n-chrome n1) (chrome-from-spc spc1 (mod (1+ pcb2) 7)))
	       (if (and x1 (not x2))
		 (setf (n-chrome n2) (chrome-from-spc spc2 (mod (1- pcb1) 7)))))
	  
	  )
   notes))
;;(notes-to-string (notes-deduce-pitches (v-notes (v-test 3))))
;;(notes-to-string (notes-deduce-pitches (list (n-new (p-new (chrome-new 5 -1))) (n-new (p-new (chrome-new 4 0))))))

(defun* mvt-deduce-pitches (mvt &optional (key (k-from-string "G minor")))
  (let ((mvt-new (mvt-copy mvt)))
    (mvt-set-key mvt-new key)
    (loop for v in (vg-voices (mvt-voice-group mvt-new))
	  do (notes-deduce-pitches (v-notes v) key))
    mvt-new))
;;(mvt-deduce-pitches (mvt-test t))
;;(movement-to-lilypond (mvt-deduce-pitches (mvt-test t)) :title "My simple algorithm" :file (concat *local-data-dir* "ly/mbsimple.ly") :start t :view nil)

;;;; Demo 2 (2013-04-08)
(require 'deduce-chromes)

;;;; Demo 2 (2013-04-08)
(require 'deduce-chromes)
(require 'dot)

;; The bach setting
(pdf-view (concat *mb-lisp-dir* "midi/data/000206b_.pdf"))

;; chords extracted manually:
(prin1 test-chords)

;; The set pitch class version of test-chords:
(prin1 (mapcar #'schord-from-string test-chords))

;; From this we build a tonicization tree:
(setf stree (dch-tree-from-chord-strings test-chords))

;; The string representation:
(prin1 stree)

;; ... and the graph representation (note that it is difficult to see
;; the difference between the two difference green colors):
(dch-tree-view stree)

;; From simple heuristics we imply chromes
(setf tree (dch-sc-to-chrome stree))

;; Again, the string representation of the tree:
(prin1 tree)

;; The graph:
(dch-tree-view tree)

;; Now, we have tried to establish chrome bases for the chords, so may
;; can compare with the original chord series:
(setf chord-strs (dch-chords stree))
(prin1 chord-strs)

;; Segmentaion demo
(setq mvt1 (mvt-submovement (mvt-test 1) 2 3)) ;; Bar 2 shows the differences
(setf (mvt-voice-groups mvt1)
      (append 
       (mvt-voice-groups mvt1)
       (loop for segmentation-type in '(total dense metric harmonic)
	     for m = (mvt-copy mvt1)
	     for s = (segmentation m segmentation-type)
	     for vg = (mvt-voice-group s)
	     collect vg)))
(movement-to-lilypond mvt1 :title "Segmentations" :file "c:/Documents and Settings/matsb/My Documents/data/ly/segmentations.ly")

;; First try
(setq mvt2 (segmentation (mvt-copy (mvt-test)) 'dense))
(setq tree3 (dch-tree-from-schords (filter-schords (schords mvt2))))
(dch-tree-view tree3)
(setq tree4 (dch-sc-to-chrome tree3))
(dch-tree-view tree4)

;;;; Demo 1 (2013-03-11)
;; pick corresponding midi file
(setq demo-chorale-name (concat *mb-lisp-dir* "midi/data/000206b_"))
(setq midi-file (concat demo-chorale-name ".mid"))

;; show raw midi
(progn (find-file midi-file)
       (hexl-mode))

;; parse midi to internal lisp format
(setq mf (read-midi-file midi-file))

;; show internal lisp format
(prin1 mf)

;; show movement definition (open movement.el)

;; convert internal midi to movement format
(setq mvt (midi-file-to-movement midi-file)))

;;show movement result
(prin1 mvt)

;;show print version
(mvt-to-string mvt)

;;conversion to LilyPond
(setq lp-file (concat demo-chorale-name ".ly"))
(setq title (format "MIDI to LilyPond %s" (iso-date-and-time :with-seconds t)))
(movement-to-lilypond mvt title lp-file)
(find-file lp-file)

(setq demo-result-pdf-file (concat demo-chorale-name ".pdf"))
(browse-url demo-result-pdf-file)

;; comment:
;; ais -> bes
;; dis -> dis
;; punctuation in the tenor near the end
;; repetition

;; todo:
;; dis -> ees
;;(gtree-view gtree-spc (concat *local-data-dir* "dot/test.dot"))
