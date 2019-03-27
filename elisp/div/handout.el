;;;; Scratch for klondike

(defun ho-make-hand (list)
  "HAND is a pair (VISIBLE HIDDEN), where VISIBLE and HIDDEN are
lists of integers (cards) . Initially VISIBLE is empty. With
ho-pop-hidden, you pop an integer from HIDDEN and push it on
VISIBLE if HIDDEN is not empty; if it is empty we pop in sequence
every integer from VISIBLE, except the last, and push it to
HIDDEN.

No errors are signaled in this design."
  (list nil list))

(defalias 'ho-visible #'first)
(defalias 'ho-hidden #'second)

(defun ho-pop-visible (hand)
  "Return the first element in VISIBLE and remove it."
  (pop (ho-visible hand)))

(defun ho-pop-hidden (hand)
  "Return the first element in HIDDEN and move it to the start of VISIBLE."
  (aif (pop (ho-hidden hand))
    (car (push it (ho-visible hand)))
    (setf (ho-hidden hand) (nreverse (ho-visible hand))
	  (ho-visible hand) nil)
    (when (ho-hidden hand)
      (ho-pop-hidden hand))))

(provide 'handout)
