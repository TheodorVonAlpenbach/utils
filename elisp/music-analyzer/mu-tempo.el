(defstruct (mu-tempo (:type list) :named (:conc-name mu-tempo-))
  (tempo-signature);allegro etc should one in a list of such
  (metronome)) ;optional

(provide 'mu-tempo)
