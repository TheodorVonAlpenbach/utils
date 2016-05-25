(defconst +qddbug-on+ nil)
(defmacro qddebug (form)
  `(when ,+qddbug-on+
     (with-buffer "*debugger*"
       (goto-char (point-max))
       (insert (format "%S = %S\n" ',form ,form)))))
;;(qddebug (+ 2 2))

(defun mb-key-chord-input-method (first-char)
  "Input method controlled by key bindings with the prefix `key-chord'."
  (qddebug first-char)
  (qddebug key-chord-last-unmatched)
  (qddebug (key-chord-lookup-key (vector 'key-chord first-char)))
  (if (and (not (eq first-char key-chord-last-unmatched))
	   (key-chord-lookup-key (vector 'key-chord first-char)))
    (let ((delay (if (key-chord-lookup-key (vector 'key-chord first-char first-char))
		   key-chord-one-key-delay
		   ;; else
		   key-chord-two-keys-delay)))
      (qddebug (key-chord-lookup-key (vector 'key-chord first-char)))
      (qddebug delay)
      (if (if executing-kbd-macro
	    (not (memq first-char key-chord-in-last-kbd-macro))
	    (sit-for delay 0 'no-redisplay))
	(progn
	  (setq key-chord-last-unmatched nil)
	  (list first-char))
	;; else input-pending-p
	(let* ((input-method-function nil)
	       (next-char (read-event))
	       (res (vector 'key-chord first-char next-char)))
	  (qddebug next-char)
	  (qddebug res)
	  (if (key-chord-lookup-key res)
	    (progn
	      (setq key-chord-defining-kbd-macro
		    (cons first-char key-chord-defining-kbd-macro))
	      (list 'key-chord first-char next-char))
	    ;; else put back next-char and return first-char
	    (setq unread-command-events (cons next-char unread-command-events))
	    (if (eq first-char next-char)
	      (setq key-chord-last-unmatched first-char))
	    (list first-char)))))
    ;; else no key-chord keymap
    (setq key-chord-last-unmatched first-char)
    (list first-char)))
