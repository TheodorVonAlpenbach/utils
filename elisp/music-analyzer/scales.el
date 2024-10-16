(require 'chrome)

(cl-defun define-scale (name chromees &optional print-style)
  (list name (if (stringp chromees) 
	       (pcs-from-string chromees print-style)
	       chromees)))
;;(define-scale "Super Locrian" "C Db Eb Fb Gb Ab B")

(defconst scale-types
  (cl-loop for x in '(("Major" "c d e f g a b")
		   ("Melodic Minor" "c d ees f g a b")
		   ("Super Locrian" "c des ees fes ges aes bes")
		   ("Neapolitan Minor" "c des ees f g aes b")
		   ("Harmonic Minor" "c d ees f g aes b")
		   ("Neapolitan Major" "c des ees f g a b")
		   ("Neapolitan Minor" "c des ees f g aes b")
		   ("Oriental" "c des e f ges a bes")
		   ("Double Harmonic" "c des e f g aes b")
		   ("Enigmatic" "c des e fis gis ais b")
		   ("Hungarian Minor" "c d ees fis g aes b")
		   ("Major Locrian" "c d e f ges aes bes")
		   ("Lydian Minor" "c d e fis g aes bes")
		   ("Overtone" "c d e fis g a bes")
		   ("Leading Whole-tone" "c d e fis gis ais b")
		   ("Hungarian Major" "c dis e fis g a bes")
		   ("Eight-tone Spanish" "c des ees e f ges aes bes")
		   ("Symmetrical" "c des ees e fis g a bes")
		   ("Diatonic Whole-tone" "c d e g a")
		   ("Pelog" "c des ees g aes")
		   ("Hirajoshi" "c d ees g aes")
		   ("Kumoi" "c d ees g a")
		   ("Six-tone Symmetrical" "c des e f gis a")
		   ("Prometheus" "c d e fis a bes")
		   ("Prometheus Neapolitan" "c des e fis a bes")
		   ("Whole-tone" "c d e fis gis ais"))
	collect (define-scale (first x) (second x) 'lilypond)))

(cl-defun get-scale-type (name) (tmap-0-1 name scale-types :test #'equal))

(cl-defun scale-skeleton (scale-type)
  (cl-loop for x in (pairs (append (mapcar #'spc-to-chrome scale-type) '(12)))
	collect (- (second x) (first x))))
;;(scale-skeleton (get-scale-type "Melodic Minor"))

(cl-defun scale-skeleton-base (scale-skeleton)
  (let ((modal-variants (cl-loop for n below (length scale-skeleton)
			      collect (rotate-list scale-skeleton n))))
    (nminimum modal-variants #'list>)))
;;(scale-skeleton-base (scale-skeleton (get-scale-type "Super Locrian")))

(prin1 (cl-loop for x in scale-types
      collect (scale-skeleton-base (scale-skeleton (second x)))))


(2 2 1 2 2 2 1) 
(2 3)

(2 1 2 2 2 2 1)
(1 4)

((2 2 2 1 2 2 1)
 (3 2)
 (2 2 2 2 1 2 1)
 (4 1)
 (2 2 2 2 2 1 1)
 (5 0)

 (3 1 1 2 2 2 1)
 (3 1 2 1 2 2 1)
 (2 2 2 2 2 1 1)
 (3 1 1 2 2 2 1)
 (3 1 2 1 3 1 1)
 (3 1 2 1 3 1 1)
 (3 2 2 2 1 1 1)
 (3 1 2 1 3 1 1)
 (2 2 2 2 2 1 1)
 (2 2 2 2 2 1 1)
 (2 2 2 2 1 2 1)
 (2 2 2 2 2 1 1)
 (3 1 2 1 2 1 2)
 (2 2 2 1 2 1 1 1)
 (2 1 2 1 2 1 2 1)
 (3 2 3 2 2)
 (4 1 4 1 2)
 (4 2 1 4 1)
 (4 2 3 2 1)
 (3 1 3 1 3 1)
 (3 1 2 2 2 2)
 (3 2 3 1 2 1)
 (2 2 2 2 2 2)
 )


