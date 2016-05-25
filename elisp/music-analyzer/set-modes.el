(defconst set-scales
  '((major (0 2 4 5 7 9 11))
    (minor (0 2 3 5 7 (8 9) (10 11))) ;;include all standard minor alteration notes
    (minor-natural (0 2 3 5 7 8 10))
    (minor-harmonic (0 2 3 5 7 8 11))
    (minor-melodic (0 2 3 5 7 9 11))))

(defun set-scale (mode)
  (if (symbolp mode)
    (find mode set-scales :key #'first)
    (find mode set-scales :test #'string-equal :key (compose #'symbol-name #'first))))
;;(set-scale 'minor)

