(defconst scale-types
  '((major (0 2 4 5 7 9 11))
    (minor (0 2 3 5 7 (8 9) (10 11))) ;;include all standard minor alteration notes
    (minor-natural (0 2 3 5 7 8 10))
    (minor-harmonic (0 2 3 5 7 8 11))
    (minor-melodic (0 2 3 5 7 9 11))))

(defun scale-type (mode)
  (if (symbolp mode)
    (find mode scale-types :key #'first)
    (find mode scale-types :test #'string-equal :key (compose #'symbol-name #'first))))
;;(scale-type 'minor)
;;(scale-type "major")

(provide '4-scales)