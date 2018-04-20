(require 'octave-breakpoint)

;;;; old, deprecated shortcuts, 
(defun deprecate-message (fn new-key)
  (lexical-let ((lfn fn) (lnew-key new-key))
    (lambda ()
      (interactive)
      (message "This key for %S is deprecated! Use \"%s\" instead." lfn lnew-key))))

(define-key octave-mode-map [(f5)] (deprecate-message #'octave-run "gh d r"))
(define-key octave-mode-map [(ctrl f5)] (deprecate-message #'octave-quit-debug "gh d q"))
(define-key octave-mode-map [(shift f5)] (deprecate-message #'octave-where "gh d w"))
(define-key octave-mode-map [(f9)] (deprecate-message #'octave-toggle-breakpoint "gh b t"))
(define-key octave-mode-map [(ctrl f9)] (deprecate-message #'octave-clear-all-breakpoints
							   "gh b d a"))
(define-key octave-mode-map [(meta f9)] (deprecate-message #'octave-list-all-breakpoints "gh b l"))
(define-key octave-mode-map [(f10)] (deprecate-message #'octave-step "gh d SPACE"))
(define-key octave-mode-map [(ctrl f10)] (deprecate-message #'octave-step-in "gh d d"))
(define-key octave-mode-map [(shift f10)] (deprecate-message #'octave-step-out "gh d u"))
(define-key octave-mode-map " " #'(lambda () (interactive) (self-insert-command 1)))
(define-key octave-mode-map "b" #'(lambda () (interactive) (self-insert-command 1)))

(defun octave-debug-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "r" #'octave-run)
    (define-key map "w" #'octave-where)
    (define-key map "s" #'octave-status)
    (define-key map " " #'octave-step)
    (define-key map "q" #'octave-quit-debug)
    (define-key map "d" #'octave-step-in)
    (define-key map "u" #'octave-step-out)
    map))

(defvar *mb-octave-debug-marker* nil
  "This is created with \(make-marker\) when debugging starts.")
;;(nilf *mb-octave-debug-marker*)

(cl-defun mb-octave-set-debug-marker (line &optional (buffer (current-buffer)))
  (when (null *mb-octave-debug-marker*)
    (setf *mb-octave-debug-marker* (make-marker))
    (setf overlay-arrow-position *mb-octave-debug-marker*))
  (set-marker *mb-octave-debug-marker* (and line (bol* :linum line)) buffer))

(defun scope-region (scope)
  (case scope
    ((nil :line) (line-region))
    ((t :buffer) (buffer-region))
    (otherwise
     (assert (integerp scope) t
	     "SCOPE must be either nil, t, :line, :buffer, or a positive integer.")
     (line-region scope))))
;;(scope-region :qwe)


(defun mb-octave-location (&optional pos)
  "Returns (FUNCTION-NAME LINUM), where FUNCTION-NAME is the
function at point and LINUM is the current line number."
  (let ((fn (substring-no-properties (add-log-current-defun)))
	(linum (line-number-at-pos pos)))
    (list fn linum)))

(defun octave-debug-p ()
  "Returns not nil if octave is in debugging mode."
  (octave-send-string "dbstack")
  (awhen (car inferior-octave-output-list)
    (not (empty-string-p it))))

(defun octave-where ()
  "Return the line and buffer of the stop line in the current debug session."
  (interactive)
  (octave-send-string "dbwhere" t)
  (let ((tokens  (split-string (car inferior-octave-output-list))))
    (list (string-to-number (sixth tokens))
	  (third tokens))))

(defun octave-status ()
  "Return the line and buffer of the stop line in the current debug session."
  (interactive)
  (octave-send-string "dbstatus" t))

(defun mb-octave-parse-ans (line)
  "Handles only single line answers"
  (and (stringp line)
       (string-match* "ans = \\(.\\)" line :num 1)))
;;(mapcar #'mb-octave-parse-ans (list nil "ans = 0"))


(defun mb-octave-debug-mode-p ()
  (octave-send-string "isdebugmode")
  (awhen (first inferior-octave-output-list)
    (not (equal (mb-octave-parse-ans it) "0"))))

(defun octave-exit-debug-mode ()
  "Not implemented Should make current buffer writeable, remove
step line markings, etc"
  (mb-octave-set-debug-marker nil)
  ;; remove this later
  (octave-send-string "" t))


(defun octave-enter-debug-mode ()
  "Not implemented Should make current buffer read-only, amplify breakpoints, etc.
  markings, etc"
  (octave-update-all-dbstop))

(defun octave-debug-start ()
 "Start debugging with comment statement after the defun at point."
  (save-excursion
    (octave-update-dbstops-buffer (current-buffer))
    (end-of-defun)
    (octave-eval-current-line)))

(defun octave-debug-refresh-display ()
  (if (octave-debug-p)
    (destructuring-bind (line fn)
	(octave-where)
      (when (or (find-tag fn)
		(save-excursion
		  (bol :linum line)
		  (string= (octave-defun-name) fn)))
	(mb-octave-set-debug-marker line)
	(goto-line line)
	(eol)))
    (octave-exit-debug-mode)))

(defun octave-resume ()
  (octave-send-string "dbcont" t)
  (octave-debug-refresh-display))


;;; UI
(defun octave-step ()
  (interactive)
  (if (mb-octave-debug-mode-p)
    (octave-send-string "dbstep" t)
    (octave-with-temp-dbstop (bod*)
     (octave-debug-start))) 
  (octave-debug-refresh-display))

(defun octave-step-in ()
  (interactive)
  (if (mb-octave-debug-mode-p)
    (octave-send-string "dbstep in" t)
    (octave-with-temp-dbstop
     (bod*)
     (octave-debug-start)))  
  (octave-debug-refresh-display))

(defun octave-step-out ()
  "Not documented"
  (interactive)
  (octave-send-string "dbstep out" t)
  (octave-debug-refresh-display))

(defun octave-quit-debug ()
  (interactive)
  (when (mb-octave-debug-mode-p)
    (octave-send-string "dbquit" t))
  (octave-debug-refresh-display))

(defun octave-run ()
  (interactive)
  (if (mb-octave-debug-mode-p)
    (octave-resume)
    (octave-debug-start)
    (octave-debug-refresh-display)))

(provide 'octave-debug-mode)
