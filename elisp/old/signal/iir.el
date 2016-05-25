(require 'elliptic-filter)

(defun dirac-impulse (n)
  (cons 1 (make-list n 0)))
;;(dirac-impulse 10)

(defun convolution (x y)
  "This is actually only a quasi-convolution since (A) all
numbers are real, not complex and (B) vectors X and Y have only
non-zero indexes."
  (loop with Lx = (length x)
	 with Ly = (length y)
	 for n below (1- (+ Lx Ly)) collect
	 (loop for k from (max 0 (- n Ly -1)) below (min Lx (1+ n))
	       sum (* (elt x k) (elt y (- n k))))))

(defun convolution-term (c x n)
  "This is actually only a quasi-convolution since (A) all
numbers are real, not complex and (B) vectors X and Y have only
non-zero indexes."
  (loop for i from (max 0 (- n (1- (length x)))) to (min (1- (length c)) n)
	sum (* (elt c i) (elt x (- n i)))))
;;(convolution-term '(1 1) '(1 1 1 1) -1)

(defun iir-filter (x a b)
  (let ((y (make-vector (length x) 0))
	(x (coerce x 'vector))
	(progress-reporter (make-progress-reporter "Calculating IIR..." 0  (length x))))
    (loop for n below (length x)
	  for i from 0
	  for yn = (- (convolution-term b x n)
		      (convolution-term a y n))
	  do (progress-reporter-update progress-reporter i)
	  do (setf (elt y n) yn))
    (progress-reporter-done progress-reporter)
    (coerce y 'list)))
;;(elliptic-filter :order 9 :Ap .5 :wp 430 :ws 450 :normalize-frequencies nil :T 1.0)

(defun qwe (wav)
  (let* ((filter (elliptic-filter :order 9 :Ap .5 :wp .04 :ws .041 :normalize-frequencies t :T (/ 1.0 44100)))
	 (new-wav (wave-copy wav))
	 (new-channels (loop for c in (wave-channels wav) 
			     collect (apply #'iir-filter c filter))))
    (setf-wave-channels new-wav new-channels)
    new-wav))
;;(write-wave-file (qwe (wave-copy ragtime-init)) "/cygdrive/c/Users/eier/Documents/MATLAB/lyder/new-ragtime.wav")

(defun ewq (amps wav)
  (let ((samples (loop for a in amps collect (list (cons :sample (round a))))))
    (setf (wave-samples wav) samples)
    wav))
;;(setf flute3 (ewq output (copy-tree flute)))
;;(write-wave-file flute3 "/cygdrive/c/Users/eier/Documents/MATLAB/lyder/flute3.wav")
;;(mb-gnuplot output :type :points)
;;(mb-gnuplot (wave-channel flute 0) :type :points)
;;(mb-gnuplot (wave-channel flute 0) :type :points)

