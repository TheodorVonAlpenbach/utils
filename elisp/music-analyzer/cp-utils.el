(defun note-pair-to-pitch-pair (np)
  (mapcar #'n-pitch np))

(defun pitch-pair-to-chrome-pair (pp)
  (mapcar #'p-chrome pp))

(defun note-pair-to-chrome-pair (np)
  (mapcar #'n-chrome np))

(defun voices-to-chords (voices)
  (transpose (mapcar #'v-notes voices)))

(defun cp-chords (cp)
  (voices-to-chords (cp-voices cp)))

(defun 2voices-to-note-pairs (voice1 voice2)
  (loop for n1 in (v-notes voice1)
	for n2 in (v-notes voice2)
	collect (list n1 n2)))

(defun 2voices-to-pitch-pairs (voice1 voice2)
  (mapcar #'note-pair-to-pitch-pair (2voices-to-note-pairs voice1 voice2)))

(defun 2voices-to-vertical-intervals (voice1 voice2)
  (loop for pp in (2voices-to-pitch-pairs voice1 voice2)
	collect (apply #'i-new pp) ))

(defun voice-to-horizontal-intervals (voice)
  (loop for 2p in (pairs (mapcar #'n-pitch (v-notes voice)))
	collect (apply #'i-new 2p)))

(defun voice-to-pitches (voice)
  (mapcar #'n-pitch (v-notes voice)))

(defun voice-to-chromees (voice)
  (mapcar #'p-chrome (voice-to-pitches voice)))

(defun voice-pairs (voices)
  (relations voices))

(provide 'cp-utils)