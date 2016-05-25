(defun midi-chrome-from-byte (pitch-byte) (mod pitch-byte 12))
;;(midi-chrome-from-byte 100) ==> 4
(defun midi-octave-byte (pitch-byte) (1- (/ pitch-byte 12)))
;;(midi-octave-byte 100) ==> 7

(defun midi-split-pitch-byte (pitch-byte)
  "Splits PITCH-BYTE into its pitch class and octave parts: (PITCH-CLASS OCTAVE),
where PITCH-CLASS is in [0 11] and OCTAVE in [-1 9]."
  (list (midi-chrome-from-byte pitch-byte)
	(midi-octave-byte pitch-byte)))
;;(midi-split-pitch-byte 100) => (4 7) i.e. E 7

(defun midi-split-pitch-string (pitch-string)
  "Splits PITCH-STRING into its pitch class and octave parts: (PITCH-CLASS OCTAVE).
where PITCH-CLASS is an element in constant MIDI-PITCH-CLASSES and OCTAVE in [-1 9]."
  (split-string-at-pos pitch-string
     (if (string-match "#" pitch-string) 2 1)))
;;(midi-split-pitch-string "C#1") ==> ("C#" "1")

(defun midi-chrome-string (chrome-int)
  "Converts the PITCH-CLASS-INT to the corresponding pitch class string."
  (nth chrome-int midi-chromees))
;;(midi-chrome-string 1) ==> "C#"

(defun midi-chrome-byte (chrome-string)
  "Converts the PITCH-CLASS-INT to the corresponding pitch class string."
  (position chrome-string midi-chromees :test #'string-equal))
;;(midi-chrome-byte "C#") ==> 1

(defun midi-pitch-string (pitch-byte)
  (let ((spb (midi-split-pitch-byte pitch-byte)))
    (format "%s%d" (midi-chrome-string (first spb)) (second spb))))
;;(midi-pitch-string 100) ==> "E7"

(defun midi-pitch-byte-from-parts (chrome-byte octave-byte)
  "Joins a split pitch-byte to pitch-byte.
Almost to an inverse of midi-split-pitch-byte."
  (+ chrome-byte (* 12 (1+ octave-byte))))
;;(midi-pitch-byte-from-parts 4 7) ==> 100

(defun midi-pitch-byte (pitch-string)
  "Converts PITCH-STRING to a pitch byte"
  (let ((sps (midi-split-pitch-string pitch-string)))
    (midi-pitch-byte-from-parts (midi-chrome-byte (first sps))
				(string-to-int (second sps)))))
;;(midi-pitch-byte "E7") ==> 100

;;midi-key conversions
(defun midi-key-to-chrome (key mode)
  (nth (+ key 7) (tmap-0-1 mode midi-key-chromees)))
;;(midi-key-to-chrome -1 'major) ==> "F"
;;(midi-key-to-chrome -1 'minor) ==> "F"

(defun midi-chrome-to-key (chrome mode)
  (- (position chrome (tmap-0-1 mode midi-key-chromees) :test #'string-equal) 7))
;;(midi-chrome-to-key "F" 'minor) ==> -4

(defun midi-mode (mode-byte)
  (if (zerop mode-byte)
    'major 'minor))
;;(midi-mode 1) ==> minor

(defun midi-mode-byte (mode)
  (if (eq mode 'major) 
    0 1))
;;(midi-mode-byte 'minor) ==> 1

(provide 'midi-pitch)
