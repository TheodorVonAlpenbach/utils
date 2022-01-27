(defparameter *default-progress*
  (list :frequency 0.01 :format " ~a% "
	:key #'(lambda (progress step)
		 (round (* 100 (/ (- step (getf progress :a)) (getf progress :n)))))
	:sub-frequency 0.001 :sub-format "." :sub-key nil))

(defun create-progress (a b n
			frequency format key
			sub-frequency sub-format sub-key
			interval-i interval-sub-i next-i next-sub-i)
  (list :a a :b b :n n
	:frequency frequency :format format :key key
	:sub-frequency sub-frequency :sub-format sub-format :sub-key sub-key
	:interval-i interval-i :interval-sub-i interval-sub-i
	:next-i next-i :next-sub-i next-sub-i))
;;(create-progress 0 10 10 0.1 "a" #'identity nil nil nil 1 nil 1 nil)

(defparameter *progress*
  (list :a 0 :b 0 :n 0
	:frequency 0.01 :format " ~a% " :key #'identity
	:sub-frequency 0.001 :sub-format "." :sub-key nil
	:interval-i 0 :interval-sub-i
	:next-i 0 :next-sub-i))

(defun get-progress-frequency (progress) (getf progress :frequency))
(defun get-progress-format (progress) (getf progress :format))
(defun get-progress-key (progress) (getf progress :key))
(defun get-progress-sub-frequency (progress) (getf progress :sub-frequency))
(defun get-progress-sub-format (progress) (getf progress :sub-format))
(defun get-progress-sub-key (progress) (getf progress :sub-key))

(defun init-progress (a b &key
			    (frequency (get-progress-frequency *default-progress*))
			    (format (get-progress-format *default-progress*))
			    (key (get-progress-key *default-progress*))
			    (sub-frequency (get-progress-sub-frequency *default-progress*))
			    (sub-format (get-progress-sub-format *default-progress*))
			    (sub-key (get-progress-sub-key *default-progress*)))
  (let* ((n (- b a))
	 (interval-i (round (* frequency n)))
	 (interval-sub-i (and sub-frequency (round (* sub-frequency n))))
	 (next-i (+ a interval-i))
	 (next-sub-i (and interval-sub-i (+ a interval-sub-i))))
    (create-progress a b n frequency format key sub-frequency sub-format sub-key
		     interval-i interval-sub-i next-i next-sub-i)))
;;(setf *progress* (init-progress 0 10000))

(defun progress-handle (progress step)
  (let ((next-i (getf progress :next-i))
	(next-sub-i (getf progress :next-sub-i))
	(key (getf progress :key))
	(sub-key (getf progress :sub-key)))
    (when (and next-sub-i (> step next-sub-i))
      (princ (format nil (getf progress :sub-format)
		     (and sub-key (funcall sub-key progress step))))
      (setf (getf progress :next-sub-i) (+ next-sub-i (getf progress :interval-sub-i)))
      (when (and next-i (> step next-i))
	(princ (format nil (getf progress :format)
		       (and key (funcall key progress step))))
	(setf (getf progress :next-i) (+ next-i (getf progress :interval-i)))))))

(provide 'progress)
