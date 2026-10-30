;;; octave-debug-mode.el --- Octave debugger controls -*- lexical-binding: t; -*-

;;; Code:

(require 'octave-breakpoint)
(require 'subr-x)
(require 'xref)

(defun octave-debug-map ()
  "Return the keymap for Octave debugger commands."
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
  (let ((map (make-sparse-keymap)))
    (define-key map "s" (octave-debug-map))
    (define-key map "i" (octave-breakpoint-map))
    map)
  "Keymap active while `octave-debug-mode' is enabled.")

(define-minor-mode octave-debug-mode
  "Provide Octave debugger commands in an Octave source buffer."
  :lighter " *Debugging*"
  :keymap octave-debug-mode-map)

(defvaralias '*mb-octave-debug-marker* 'mb-octave-debug-marker)

(defvar mb-octave-debug-marker (make-marker)
  "Marker identifying the current Octave debugger line.")

(defun mb-octave-set-debug-marker (line &optional buffer)
  "Display the debugger marker at LINE in BUFFER.
Clear the marker when LINE is nil.  BUFFER defaults to the
current buffer."
  (unless (markerp mb-octave-debug-marker)
    (setq mb-octave-debug-marker (make-marker)))
  (when-let ((old-buffer (marker-buffer mb-octave-debug-marker)))
    (with-current-buffer old-buffer
      (when (eq overlay-arrow-position mb-octave-debug-marker)
        (setq-local overlay-arrow-position nil))))
  (if (null line)
      (set-marker mb-octave-debug-marker nil)
    (let ((target (or buffer (current-buffer))))
      (with-current-buffer target
        (save-excursion
          (octave--goto-line line)
          (set-marker mb-octave-debug-marker (point) target))
        (setq-local overlay-arrow-position mb-octave-debug-marker))))
  mb-octave-debug-marker)

(defun octave-debug-p ()
  "Return non-nil when the inferior Octave process is debugging."
  (not (string-empty-p
        (string-trim (or (octave-send-string "dbstack") "")))))

(defun octave-debug--parse-where (output)
  "Parse an Octave dbwhere OUTPUT string as (LINE FUNCTION)."
  (if (string-match
       "\\bstopped in \\([^[:space:]]+\\) at line \\([0-9]+\\)\\b"
       output)
      (list (string-to-number (match-string 2 output))
            (match-string 1 output))
    (error "Unexpected Octave dbwhere output: %s" output)))

(defun octave-where ()
  "Return the current Octave debugger location as (LINE FUNCTION)."
  (interactive)
  (octave-debug--parse-where (octave-send-string "dbwhere" t)))

(defun octave-status ()
  "Display Octave's current breakpoint status."
  (interactive)
  (octave-send-string "dbstatus" t))

(defun mb-octave-parse-ans (line)
  "Return the value from a single-line Octave `ans' response LINE."
  (when (and (stringp line)
             (string-match
              "\\`[[:space:]]*ans[[:space:]]*=[[:space:]]*\\(.+?\\)[[:space:]]*\\'"
              line))
    (match-string 1 line)))

(defalias 'mb-octave-debug-mode-p #'octave-debug-p)

(defun octave-exit-debug-mode ()
  "Clear debugger display state in all Octave buffers."
  (mb-octave-set-debug-marker nil)
  (dolist (buffer (octave--mode-buffers))
    (with-current-buffer buffer
      (octave-debug-mode -1))))

(defun octave-enter-debug-mode ()
  "Synchronize marked breakpoints with Octave."
  (octave-update-all-dbstops))

(defun octave-debug-start ()
  "Start debugging with the test expression at point."
  (octave-debug-mode 1)
  (octave-update-dbstops-buffer (current-buffer))
  (octave-eval-defun-test))

(defun octave-debug--current-function-p (function)
  "Return non-nil when point is in Octave FUNCTION."
  (and (derived-mode-p 'octave-mode)
       (condition-case nil
           (string= (car (octave--function-location)) function)
         (user-error nil))))

(defun octave-debug--find-function (function)
  "Move to Octave FUNCTION when it is not at point."
  (unless (octave-debug--current-function-p function)
    (xref-find-definitions function)
    (unless (octave-debug--current-function-p function)
      (user-error "Could not locate Octave function %s" function))))

(defun octave-debug-refresh-display ()
  "Refresh the current Octave debugger source location."
  (if (not (octave-debug-p))
      (octave-exit-debug-mode)
    (pcase-let ((`(,line ,function) (octave-where)))
      (octave-debug--find-function function)
      (octave-debug-mode 1)
      (mb-octave-set-debug-marker line)
      (octave--goto-line line)
      (end-of-line))))

(defun octave-resume ()
  "Resume the current Octave debug session."
  (octave-send-string "dbcont" t)
  (octave-debug-refresh-display))

(defun octave-debug--defun-start ()
  "Return the beginning position of the current Octave function."
  (save-excursion
    (octave-beginning-of-defun)
    (point)))

(defun octave-step ()
  "Step over the next Octave statement."
  (interactive)
  (if (octave-debug-p)
      (octave-send-string "dbstep" t)
    (octave-with-temp-dbstop (octave-debug--defun-start)
      (octave-debug-start)))
  (octave-debug-refresh-display))

(defun octave-step-in ()
  "Step into the next Octave function call."
  (interactive)
  (if (octave-debug-p)
      (octave-send-string "dbstep in" t)
    (octave-with-temp-dbstop (octave-debug--defun-start)
      (octave-debug-start)))
  (octave-debug-refresh-display))

(defun octave-step-out ()
  "Continue execution until the current Octave function returns."
  (interactive)
  (octave-send-string "dbstep out" t)
  (octave-debug-refresh-display))

(defun octave-quit-debug ()
  "Quit the current Octave debug session."
  (interactive)
  (when (octave-debug-p)
    (octave-send-string "dbquit" t))
  (octave-debug-refresh-display))

(defun octave-run ()
  "Resume debugging, or start a debug session at point."
  (interactive)
  (if (octave-debug-p)
      (octave-resume)
    (octave-debug-start)
    (octave-debug-refresh-display)))

(defun octave-run-last ()
  "Re-evaluate the most recent Octave debug expression."
  (interactive)
  (unless (and (stringp octave-last-debug-expression)
               (not (string-empty-p octave-last-debug-expression)))
    (user-error "No previous Octave debug expression"))
  (octave-update-all-dbstops)
  (octave-send-string octave-last-debug-expression t)
  (octave-debug-refresh-display))

(defun octave-run-to-cursor ()
  "Run Octave to the current source position."
  (interactive)
  (octave-with-temp-dbstop (point)
    (octave-run)))

(provide 'octave-debug-mode)

;;; octave-debug-mode.el ends here
