;;;; Definitions

;;; The names of the functions below are chosen based the following
;;; definitions:

;; BUFFER BREAKPOINT: a line in an Octave file marked as a breakpoint.
;; The mark is a special comment appended to the breakpoint line.
;; However, it is not shown as such, but converted to a fringe symbol
;; when minor-mode `octave-debug' is in effect.

;; DBSTOP: a pair (FUNC LINUM) which Octave sets and unsets with
;; `dbstop' and `dbclear', respectively. FUNC is an Octave function
;; name, and LINUM is the line number in the FILE where FUNC is
;; defined. When Octave is in debug mode, and the execution has
;; reached LINUM in FILE, the execution halts (until it is resumed
;; with `dbcont').

;; BREAKPOINT: a line in an Octave file that is both a BUFFER
;; BREAKPOINT and a DBSTOP.

(defun octave-breakpoint-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "t" #'octave-toggle-breakpoint)
    (define-key map "l" #'octave-list-all-breakpoints)
    (define-key map "d" (octave-delete-breakpoint-map))
    map))

(defun octave-delete-breakpoint-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "r" #'octave-delete-breakpoints-region)
    (define-key map "l" #'octave-delete-breakpoint-line)
    (define-key map "d" #'octave-delete-defun-breakpoints)
    (define-key map "b" #'octave-delete-buffer-breakpoints)
    (define-key map "a" #'octave-delete-all-breakpoints)
    map))

(defface octave-breakpoint-indicator
    '((((min-colors 88) (class color))
       :foreground "yellow1" :background "red1" :width condensed)
      (((class color))
       :foreground "yellow" :background "red" :width condensed)
      (t :inverse-video t))
  "Face used by Octave mode for marking breakpoint lines in the fringe."
  :group 'octave-mode)

(defconst +octave-breakpoint-string+ " #BREAKPOINT")
(defconst +octave-breakpoint-regexp+ (concat +octave-breakpoint-string+ "$"))
(defconst +octave-breakpoint-display+
  '(left-fringe hollow-rectangle octave-breakpoint-indicator))

;;; Buffer breakpoints
(defun octave-buffer-breakpoint-p (&optional pos)
  "Return not `nil' if the current buffer line is a buffer breakpoint."
  (save-excursion
    (bol :point pos)
    (re-search-forward +octave-breakpoint-regexp+ (line-end-position) t)))

(defun octave-set-buffer-breakpoint (&optional line)
  "The breakpoint mark at LINE.
The default LINE is the current line."
  (save-excursion
    (if line (eol :linum line) (eol))
    (unless (octave-buffer-breakpoint-p)
      (insert +octave-breakpoint-string+)
      (put-text-property
	  (- (line-end-position) (length +octave-breakpoint-string+))
	  (line-end-position) 'display
	  +octave-breakpoint-display+))))
;;(octave-unset-buffer-breakpoint)

(defun octave-unset-buffer-breakpoint (&optional scope)
  "Delete buffer breakpoint at line and return T.
If scope is a number, then delete breakpoint at that line. If
SCOPE is :BUFFER then delete every breakpoint mark in the current
buffer. If SCOPE is a string, then act on the buffer with name
scope. If scope is a buffer object act on that buffer. If scope
is :all then delete every breakpoint mark in every buffer in
Octave mode. A scope value of :line or nil is equivalent with not
providing it.

If no breakpoint is deleted by calling this function, typically
because there are no breakpoints in SCOPE, then the function
returns `nil'.

Currently only the scope values nil, number, and :buffer are
implemented."
  (save-excursion
    (save-restriction
      (apply #'narrow-to-region (scope-region scope))
      (goto-char (point-min))
      (while (re-search-forward +octave-breakpoint-regexp+ nil t)
	(replace-match "")
	(bol)))))

(defun octave-region-breakpoints (beg end &optional buffer)
  "Return the buffer breakpoints BUFFER's region (BEG END).
The result is organized as a list (DEFUN-BREAKPOINT1
DEFUN-BREAKPOINT2 ...), where each element is a pair (DEFUN
LINUM-OR-LINUMS), where LINUM-OR-LINUMS is a buffer breakpoint
line number, or a list of such numbers, within the Octave
function DEFUN."
  (with-buffer buffer
    (goto-char beg)
    (loop while (re-search-forward +octave-breakpoint-regexp+ end t)
	  collect (list (octave-defun-name) (line-number-at-pos)))))

(defun octave-buffer-breakpoints (&optional buffer)
  "Return the buffer breakpoints in BUFFER.
The result is a list of the same format as the result from
`octave-region-breakpoints'."
  (octave-region-breakpoints (point-min) (point-max) buffer))

(defun octave-all-buffer-breakpoints (&optional buffer)
  "Return the buffer breakpoints in all active Octave mode buffers.
The result is organized as the list described in
`octave-update-dbstops'."
  (loop for x being the buffers
	if (octave-mode-p x)
	collect (list x (octave-buffer-breakpoints x))))


;;; dbstops
(defun octave-dbstop-p (&optional pos)
  "Return not `nil' if the current buffer line is a dbstop in Octave."
  (let ((bl (octave-dbstop-list)))
    (destructuring-bind (fn line) (mb-octave-location pos)
      (awhen (find fn bl :test #'string= :key #'first)
	(find line (second it))))))

(defun octave-dbstop-list-1 (oline)
  "Extract function name and dbstop lines in string OLINE.
This is a helper function for `octave-dbstop-list'."
  (destructuring-bind (fn lines)
      (string-match*
	  "breakpoint in \\([^ ]+\\) at lines? \\(.*\\)\\."
	oline :num '(1 2))
    (list fn (mapcar #'string-to-number (split-string lines ", " t)))))
;;(mapcar #'string-to-number (split-string "1, 2" ", " t))

(defun octave-dbstop-list (&optional fn)
  (octave-send-string (if fn (format "dbstatus %s" fn) "dbstatus"))
  (mapcar #'octave-dbstop-list-1 inferior-octave-output-list))

(defun octave-set-dbstops-1 (fn lines)
  "Compose string for `octave-set-dbstops'."
  (awhen (mapcar #'number-to-string (listify lines))
    (concat* (if fn (cons fn it) it) :pre "dbstop " :in " ")))
;;(octave-set-dbstops-1 "fn" 1)

(defun octave-set-dbstops (defun-name lines &optional show-output-p)
  "Set Octave breakpoints for function with DEFUN-NAME.
If defun-name is nil then skip discard function name argument,
which you typically will do in debug mode. If optional argument
show-output-p is not nil, then print Octave response in REPL."
  (awhen (octave-set-dbstops-1 defun-name lines)
    (octave-send-string it show-output-p)))

(defun octave-update-dbstops (buffer-breakpoints)
  "Convert all BUFFER-BREAKPOINTS to Octave dbstops.
The input argument is a list with elements on the form \(BUFFER
DEFUN-BREAKPOINTS\), where each element in DEFUN-BREAKPOINTS is a
list of the same format as the result from
`octave-region-breakpoints'."
  (loop for (buffer fn-breakpoints) in buffer-breakpoints
	do (octave-source-buffer t buffer)
	do (loop for (fn lines) in fn-breakpoints
		 do (octave-set-dbstops fn lines t))))

(defun octave-update-dbstops-buffer (&optional buffer)
  "Convert all buffer breakpoints in BUFFER to Octave dbstops.
A side effect is that the BUFFER file will be source-d in
Octave."
  (awhen (octave-buffer-breakpoints buffer)
    (octave-update-dbstops (list (list buffer it)))))

(defun octave-update-all-dbstop ()
  "Convert all buffer breakpoints in all buffers to Octave dbstops.
A side effect is that the corresponding octave files will be
source-d in Octave."
  (octave-update-dbstops (octave-all-buffer-breakpoints)))


;;; Breakpoints
(defun octave-breakpoint-p (&optional pos)
  "Return not `nil' if the current buffer line is an Octave breakpoint."
  (let ((dbstop-p (octave-dbstop-p pos))
	(buffer-breakpoint-p (octave-buffer-breakpoint-p pos)))
    (unless (eql dbstop-p buffer-breakpoint-p)
      (message "Buffer breakpoint and dbstop settings differ!"))
    (or dbstop-p buffer-breakpoint-p)))

(defun octave-set-breakpoint (&optional pos)
  "Set breakpoint at the line covering the point POS.
The function adds a dbstop in Octave, and marks the corresponding
buffer line."
  (destructuring-bind (fn line) (mb-octave-location pos)
    (octave-set-dbstops (unless (octave-debug-p) fn) line t)
    (octave-set-buffer-breakpoint line))
  (first inferior-octave-output-list))
;;(octave-set-breakpoint)

(defun octave-unset-breakpoint (&optional pos)
  "Unset any breakpoint at the line covering point POS.
Implementaion note. This mechanism could be more effective if a
DEFUN-BREAKPOINT was the triple (DEFUN LINUM POS). Then all of
this info could be passed to this function and the costy call to
mb-octave-location would be superfluous."
  (destructuring-bind (fn line) (mb-octave-location pos)
    (octave-send-string (if (octave-debug-p)
			  (format "dbclear %d" line)
			  (format "dbclear %s %d" fn line))
			t)
    (octave-unset-buffer-breakpoint line))
  inferior-octave-output-list)
;;(octave-unset-breakpoint)

(defun octave-unset-breakpoints-region (beg end &optional buffer)
  "Unset every breakpoint in the region (beg end) in BUFFER.
See implementation note in `octave-unset-breakpoint'."
  (with-buffer buffer
    (loop for (fn line) in (octave-region-breakpoints beg end buffer)
	  do (octave-unset-breakpoint (bol* :linum line)))))

(defmacro octave-with-temp-dbstop (pos &rest body)
  "Execute BODY with a temparary dbstop set at POS.
Note that POS is a buffer position, not at line number. The macro
sets a dbstop at the line covering POS, executes body and unsets
the breakpoint. If there is a already breakpoint at POS, it only
executes BODY."
  `(if (octave-breakpoint-p ,pos)
     (progn ,@body)
     (progn
       (octave-set-breakpoint ,pos)
       ,@body
       (octave-unset-breakpoint ,pos))))
(def-edebug-spec octave-with-temp-dbstop t)


;;; UI
(defun octave-toggle-breakpoint ()
  (interactive)
  (if (octave-breakpoint-p)
    (octave-unset-breakpoint)
    (octave-set-breakpoint)))

(defun octave-list-all-breakpoints ()
  (interactive)
  (octave-send-string "dbstatus" t))

(defun octave-delete-breakpoint-line ()
  (interactive)
  (octave-unset-breakpoint))

(defun octave-delete-region-breakpoints (beg end)
  (interactive "r")
  (octave-unset-breakpoints-region beg end))

(defun octave-delete-defun-breakpoints ()
  (interactive)
  (apply #'octave-delete-region-breakpoints (defun-region)))

(defun octave-delete-buffer-breakpoints ()
  (interactive)
  (octave-delete-region-breakpoints (point-min) (point-max)))

(defun octave-delete-all-breakpoints ()
  (interactive)
  (loop for (buffer fn-breakpoints) in (octave-all-buffer-breakpoints)
	do (with-buffer buffer
	     (octave-delete-buffer-breakpoints)))
  ;; just to be sure
  (octave-send-string "dbclear all"))

(provide 'octave-breakpoint)