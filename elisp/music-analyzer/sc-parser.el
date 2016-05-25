;;;; See http://en.wikipedia.org/wiki/Cadence_(music) for terms

(defconst scs-test
  (mapcar #'sc-from-chord-symbol '(C F C Am F G C G C Em Am D7 G))
  "Chords are taken from 'St. Anne' (Our God, Our Help in Ages Past) by Isaac Watts")

(defun scr-root (sc1 sc2)
  (- (sc-root sc2) (sc-root sc1)))
;;(scr-root (first scs-test) (second scs-test))

(defun scr (sc1 sc2)
  (list (scr-root sc1 sc2) (sc-typename sc1) (sc-typename sc2)))
;;(scr (first scs-test) (second scs-test))

(defun scr-reduce-stack (stack)
  (let ((sc2 (pop stack))
	(sc1 (pop stack)))
    (case (sc-relation sc1 sc2)
      (SDT (push (list SDT (sc-base-pc sc2 sc1) sc2) stack))
      (DT (push (list DT (sc-base-pc sc2 sc1) sc2) stack))
      (TD (push (list DT (sc-base-pc sc2 sc1) sc2) stack))
      (t (push* sc1 sc2)))))

(defun scr-parse (scs)
  (let ((stack (pop-list scs 2)))
    (list stack scs)))
;;(scr-parse scs-test)


(provide 'sc-parser)




