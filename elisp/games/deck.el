(cl-defun deck-shuffle ()
  (randomize (0-n 52)))
;;(deck-shuffle)

(defface solitaire
       '((t :height 3.0))
       "Basic face for solitaire buffers."
       :group 'basic-faces)
;;(face-attribute 'solitaire :height)

(provide 'deck)
