(require 'movement)

(cl-defun mvt-create (x)
  (if (movement-p x) x (mvt-new (vgs-create x))))

(cl-defun vgs-create (x)
  (if (vgs-p x) x))

(cl-defun voice-groups (x &optional preserve-tree)
  "Extracs voice groups from X (a `movement' or list of
`voice-group'). PRESERVE-TREE is currently present only for
conformity with other general extraction methods like `voices'
and `notes'"
  (if (vgs-p x) x (mvt-voice-groups x)))
;;(voice-groups sm nil)

(cl-defun voice-group (x &optional preserve-tree)
  (if (voice-group-p x)
    x 
    (if preserve-tree
      (first (voice-groups x preserve-tree))
      (apply #'conc (voice-groups x preserve-tree)))))
;;(voice-group sm)

(cl-defun voices (x &optional preserve-tree)
  (if (voices-p x) 
    x
    (aif (voice-groups x preserve-tree)
      (if preserve-tree
	(mapcar #'vg-voices it)
	(apply #'conc (mapcar #'vg-voices it)))
      (when (voice-group-p x)
	(vg-voices x)))))
;;(voices sm)

(cl-defun voice (x)
  (if (voice-p x)
    x 
    (first (voices x))))
;;(voice sm)

(cl-defun notes (x &optional preserve-tree)
  "Returns a list of all notes in X. For meaning of
PRESERVE-TREE, see `voices'"
  (if (notes-p x) 
    x
    (aif (voices x preserve-tree)
      (if preserve-tree
	(maptree #'v-notes it)
	(flatten (maptree #'v-notes it)))
      (when (voice-p x)
	(v-notes x)))))
;;(notes (mvt-test) t)

(cl-defun note (x)
  (if (note-p x) x (n-new (pitch x))))
;;(note (p-new))

(cl-defun durations-p (x)
  (and (listp x) (every #'duration-p x)))

(cl-defun durations (x &optional preserve-tree)
  "Returns a list of all durations in X, For meaning of
PRESERVE-TREE, see `voices'"
  (or (maptree #'n-duration (notes x preserve-tree))
      (if (durations-p x) x)))
;;(durations (mvt-test))

(cl-defun duration (x &optional preserve-tree)
  "Returns a list of all durations in X, For meaning of
PRESERVE-TREE, see `voices'"
  (cond ((duration-p x) x)  
	((numberp x) (d-new x))))
;;(duration 3)

(cl-defun dvalues (x &optional preserve-tree)
  "Returns a list of all durations in X, For meaning of
PRESERVE-TREE, see `voices'"
  (maptree #'d-value (durations x preserve-tree)))
;;(dvalues (mvt-test))

(cl-defun dvalue (x &optional preserve-tree)
  "Returns a list of all durations in X, For meaning of
PRESERVE-TREE, see `voices'"
  (cond ((duration-p x) x)  
	((numberp x) (d-new x))))
;;(duration 3)

(cl-defun pitches (x &optional preserve-tree)
  "Returns a list of all notes in X. For meaning of
PRESERVE-TREE, see `voices'"
  (or (maptree #'n-pitch (notes x preserve-tree))
      (if (pitches-p x) x)))
;;(pitches (mvt-test))

(cl-defun pitch (x)
  (cond ((pitch-p x) x)  
	((chrome-p x) (p-new x))))
;;(pitch (chrome-new))


;;chords
(cl-defun nchords (x)
  (transpose (notes (voices x nil) t)))
;;(length (nchords (mvt-test)))

(cl-defun pchords (x)
  (maptree #'n-pitch (nchords x) :levels 2))
;;(length (pchords (mvt-test)))

(cl-defun chords (x)
  (maptree #'p-chrome (pchords x) :levels 2))
;;(length (chords (mvt-test)))

(cl-defun schords (x)
  (mapcar #'schord-from-chord (chords x)))
;;(length (schords (mvt-test)))


;;methods
(cl-defun upbeat (x bar &optional (with-upbeat t))
  (v-upbeat (voice x)))

(cl-defun bar-position (x bar &optional (with-upbeat t))
  (v-bar-position (voice x) bar with-upbeat))


(provide 'mu-general)
