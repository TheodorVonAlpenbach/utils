(require 'octave-breakpoint)

;;;; old, deprecated shortcuts, 
(cl-defun deprecate-message (fn new-key)
  (let ((lfn fn) (lnew-key new-key))
    (lambda ()
      (interactive)
      (message "This key for %S is deprecated! Use \"%s\" instead." lfn lnew-key))))

(cl-defun octave-debug-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "r" #'octave-run)
    (define-key map "l" #'octave-run-last)
    (define-key map "c" #'octave-run-to-cursor)
    (define-key map "w" #'octave-where)
    (define-key map "S" #'octave-status)
    (define-key map "s" #'octave-step)
    (define-key map " " #'octave-step)
    (define-key map "q" #'octave-quit-debug)
    (define-key map "d" #'octave-step-in)
    (define-key map "u" #'octave-step-out)
    map))

(defvar octave-debug-mode-map
  (let ((map (copy-keymap evil-normal-state-map)))
    (evil-define-key 'normal map "s" (octave-debug-map))
    (evil-define-key 'normal map "i" (octave-breakpoint-map))
    map))
;;(nilf octave-debug-mode-map)

(define-minor-mode octave-debug-mode
  "Mode for Emacs Lisp buffers while in debugging Octave.

In addition to all Emacs Lisp commands (except those that modify the
buffer) there are local and global key bindings to several debug
specific commands.  E.g. `octave-step' is bound to \\[octave-step]
in the debug buffer and \\<global-map>\\[octave-step] in any buffer.

The edebug buffer commands:
\\{octave-debug-mode-map}

Global commands prefixed by `global-octave-debug-prefix':
\\{global-octave-debug-map}

Options:
`octave-debug-setup-hook'"
  :lighter " *Debugging*"
  :keymap octave-debug-mode-map
  ;; If the user kills the buffer in which Octave debug is currently
  ;; active, exit to top level, because the edebug command loop can't
  ;; usefully continue running in such a case.
  ;;
  ;; (if (not octave-debug-mode)
  ;;   (progn
  ;;     (setq old-evil-normal-state-map evil-normal-state-map)
  ;;     (message "qweqweqwe!"))
  ;;   (progn
  ;;     (setq old-evil-normal-state-map evil-normal-state-map)))
  )

;; (define-key octave-mode-map " " #'(lambda () (interactive) (self-insert-command 1)))
;; (define-key octave-mode-map "b" #'(lambda () (interactive) (self-insert-command 1)))


(defvar *mb-octave-debug-marker* nil
  "This is created with \(make-marker\) when debugging starts.")
;;(nilf *mb-octave-debug-marker*)

(cl-defun mb-octave-set-debug-marker (line &optional (buffer (current-buffer)))
  (when (null *mb-octave-debug-marker*)
    (setf *mb-octave-debug-marker* (make-marker))
    (setf overlay-arrow-position *mb-octave-debug-marker*))
  (set-marker *mb-octave-debug-marker* (and line (bol* :linum line)) buffer))

(cl-defun scope-region (scope)
  (cl-case scope
    ((nil :line) (line-region))
    ((t :buffer) (buffer-region))
    (otherwise
     (cl-assert (integerp scope) t
	     "SCOPE must be either nil, t, :line, :buffer, or a positive integer.")
     (line-region scope))))
;;(scope-region :qwe)


(cl-defun mb-octave-location (&optional pos)
  "Returns (FUNCTION-NAME LINUM), where FUNCTION-NAME is the
function at point and LINUM is the current line number.

TODO: rename and move this defun elsewhere. It is not Octave
specific."
  (save-excursion
    (when pos (goto-char pos))
    (list (substring-no-properties (add-log-current-defun))
	  (line-number-at-pos))))

(cl-defun octave-debug-p ()
  "Returns not nil if octave is in debugging mode."
  (octave-send-string "dbstack")
  (awhen (car inferior-octave-output-list)
    (not (empty-string-p it))))

(cl-defun octave-where ()
  "Return the line and buffer of the stop line in the current debug session."
  (interactive)
  (octave-send-string "dbwhere" t)
  (let ((tokens  (split-string (car inferior-octave-output-list))))
    (list (string-to-number (sixth tokens))
	  (third tokens))))

(cl-defun octave-status ()
  "Return the line and buffer of the stop line in the current debug session."
  (interactive)
  (octave-send-string "dbstatus" t))

(cl-defun mb-octave-parse-ans (line)
  "Handles only single line answers"
  (and (stringp line)
       (string-match* "ans = \\(.\\)" line :num 1)))
;;(mapcar #'mb-octave-parse-ans (list nil "ans = 0"))


(cl-defun mb-octave-debug-mode-p ()
  (octave-send-string "isdebugmode")
  (awhen (first inferior-octave-output-list)
    (not (equal (mb-octave-parse-ans it) "0"))))

(cl-defun octave-exit-debug-mode ()
  "Not implemented Should make current buffer writeable, remove
step line markings, etc"
  (mb-octave-set-debug-marker nil)
  ;; remove this later
  (octave-send-string "" t)
  (octave-buffer-breakpoints)
  (with-buffers (octave-buffers)
    (octave-debug-mode -1)))

(cl-defun octave-enter-debug-mode ()
  "Not implemented Should make current buffer read-only, amplify breakpoints, etc.
  markings, etc"
  (octave-update-all-dbstop))

(cl-defun octave-debug-start ()
  "Start debugging with comment statement after the defun at point."
  (octave-debug-mode 1)
  (save-excursion
    (octave-update-dbstops-buffer (current-buffer))
    (octave-eval-defun-test)))

(cl-defun octave-debug-refresh-display ()
  (if (octave-debug-p)
    (cl-destructuring-bind (line fn)
	(octave-where)
      (when (or (save-excursion
		  (bol :linum line)
		  (string= (octave-defun-name) fn))
		(condition-case nil
		    (find-tag fn)
		  (error nil)))
	(octave-debug-mode 1)
	(mb-octave-set-debug-marker line)
	(goto-line line)
	(eol)))
    (octave-exit-debug-mode)))

(cl-defun octave-resume ()
  (octave-send-string "dbcont" t)
  (octave-debug-refresh-display))


;;; UI
(cl-defun octave-step ()
  (interactive)
  (if (mb-octave-debug-mode-p)
    (octave-send-string "dbstep" t)
    (octave-with-temp-dbstop (bod*)
     (octave-debug-start))) 
  (octave-debug-refresh-display))

(cl-defun octave-step-in ()
  (interactive)
  (if (mb-octave-debug-mode-p)
    (octave-send-string "dbstep in" t)
    (octave-with-temp-dbstop
     (bod*)
     (octave-debug-start)))  
  (octave-debug-refresh-display))

(cl-defun octave-step-out ()
  "Not documented"
  (interactive)
  (octave-send-string "dbstep out" t)
  (octave-debug-refresh-display))

(cl-defun octave-quit-debug ()
  (interactive)
  (when (mb-octave-debug-mode-p)
    (octave-send-string "dbquit" t))
  (octave-debug-refresh-display))

(cl-defun octave-run ()
  (interactive)
  (if (mb-octave-debug-mode-p)
    (octave-resume)
    (octave-debug-start)
    (octave-debug-refresh-display)))

(cl-defun octave-run-last ()
  "Re-eval last defun test expression."
  (interactive)
  (octave-update-all-dbstop)
  (octave-send-string *octave-last-debug-expression* t)
  (octave-debug-refresh-display))

(cl-defun octave-run-to-cursor ()
  (interactive)
  (octave-with-temp-dbstop (point)
   (octave-run)))

(provide 'octave-debug-mode)
