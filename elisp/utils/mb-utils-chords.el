(require 'mb-utils-div)

;; begin true file here
(defconst chordtypes
  '((major (4 7))
    (minor (3 7))
    (diminished (3 6))
    (augmented (4 8))))

(defun transpose-note (note n)
  (mod (+ note n) 12))
;;(transpose-note 2 -2)

(defun transpose-notes (note-list n)
  (mapcar (lambda (x) (mod (+ x n) 12)) note-list))
;;(transpose-notes '(0 2 4 5 7 9 11) 1)

(defun chord-skeleton (chord)
  (rest (transpose-notes chord (- (first chord)))))
;;(chord-skeleton '(4 7 12))

(defun* ninvert-chord-skeleton (cs &optional (n 1))
  "Destructive"
  (chord-skeleton (nrotate-list (push 0 cs) n)))
;;(ninvert-chord-skeleton '(4 7) 2)

(defun* invert-chord-skeleton (cs &optional (n 1))
  (ninvert-chord-skeleton (copy-list cs) n))
;;(invert-chord-skeleton major-root 2)
;;(setq major-root '(4 7))

(defun cs-chordtype (cs)
  "CS is chord-skeleton"
  (loop for i to (length cs) 
	for chordtype = (find (invert-chord-skeleton cs (- i)) chordtypes :key #'second :test #'equal)
	if chordtype return (list chordtype i)))
;;(cs-chordtype '(6 9))

(defun chordtype (chord)
  (cs-chordtype (chord-skeleton chord)))
;;(chordtype '(2 5 9))

(defun chord-function-step (chord key)
  (nth (mod (- (second (chordtype chord))) (length chord)) chord))
;;(chord-function-step '(5 9 2) 0)
