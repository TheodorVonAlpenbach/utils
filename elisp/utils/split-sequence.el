(cl-defun split-sequence (delimiter sequence &key (start 0) (end (length sequence)) (from-end nil)
				    (cl-count nil) (remove-empty-subseqs nil)
				    (test #'eql) (test-not nil) (key #'identity))
  "Return a list of subsequences in seq delimited by delimiter.
If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (cond
    ((and (not from-end) (null test-not))
     (split-from-start (lambda (sequence start)
			 (position delimiter sequence :start start :key key :test test))
		       sequence start end count remove-empty-subseqs))
    ((and (not from-end) test-not)
     (split-from-start (lambda (sequence start)
			 (position delimiter sequence :start start :key key :test-not test-not))
		       sequence start end count remove-empty-subseqs))
    ((and from-end (null test-not))
     (split-from-end (lambda (sequence end)
		       (position delimiter sequence :end end :from-end t :key key :test test))
		     sequence start end count remove-empty-subseqs))
    ((and from-end test-not)
     (split-from-end (lambda (sequence end)
		       (position delimiter sequence :end end :from-end t :key key :test-not test-not))
		     sequence start end count remove-empty-subseqs))))
;;(split-sequence ?  "The quick brown fox jumps over the lazy dog")

(cl-defun split-sequence-if (predicate sequence &key (start 0) (end (length sequence)) (from-end nil)
                            (cl-count nil) (remove-empty-subseqs nil) (key #'identity))
    "Return a list of subsequences in seq delimited by items satisfying
predicate.
If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
    (if from-end
        (split-from-end (lambda (sequence end)
                          (position-if predicate sequence :end end :from-end t :key key))
                        sequence start end count remove-empty-subseqs)
        (split-from-start (lambda (sequence start)
                            (position-if predicate sequence :start start :key key))
                          sequence start end count remove-empty-subseqs)))
;;(split-sequence-if #'oddp (0-n 10) :remove-empty-subseqs t)

(cl-defun split-sequence-if-not (predicate sequence &key (cl-count nil) (remove-empty-subseqs nil)
					   (from-end nil) (start 0) (end (length sequence)) (key #'identity))
    "Return a list of subsequences in seq delimited by items satisfying
\(CL:COMPLEMENT predicate).
If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF-NOT.  In particular,
the behaviour of :from-end is possibly different from other versions
of this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
    (if from-end
        (split-from-end (lambda (sequence end)
                          (position-if-not predicate sequence :end end :from-end t :key key))
                        sequence start end count remove-empty-subseqs)
        (split-from-start (lambda (sequence start)
                            (position-if-not predicate sequence :start start :key key))
                          sequence start end count remove-empty-subseqs)))

(cl-defun split-from-end (position-fn sequence start end count remove-empty-subseqs)
  (loop
     for right = end then left
     for left = (max (or (funcall position-fn sequence right) -1)
                       (1- start))
     unless (and (= right (1+ left))
                  remove-empty-subseqs) ; empty subseq we don't want
     if (and count (>= nr-elts count))
     ;; We can't take any more. Return now.
       return (values (nreverse subseqs) right)
     else
       collect (subseq sequence (1+ left) right) into subseqs
       and sum 1 into nr-elts
     until (< left start)
   finally (return (values (nreverse subseqs) (1+ left)))))

(cl-defun split-from-start (position-fn sequence start end count remove-empty-subseqs)
  (let ((length (length sequence)))
    (cl-loop for left = start then (+ right 1)
	  for right = (min (or (funcall position-fn sequence left) length)
			   end)
	  unless (and (= right left)
		      remove-empty-subseqs) ; empty subseq we don't want
	  if (and count (>= nr-elts count))
	  ;; We can't take any more. Return now.
   	    return (values subseqs left)
	  else
	    collect (subseq sequence left right) into subseqs
	    and sum 1 into nr-elts
	  until (>= right end)
     finally (return (values subseqs right)))))
