(defun tf-space-width (font)
  font)

(defun tf-length (word font)
  (* (length word)
     font))

(defun tf-wl (word font)
  (list (tf-length word font) word))

(defun tf-insert-newlines (s font)
  (let* ((words (split-string s))
	 (wls (mapcar (bind #'tf-wl font) words)))))