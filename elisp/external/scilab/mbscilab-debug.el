(require 'mb-utils-buffer)
(require 'mbscilab-syntax)
(require 'mb-lists)

(defconst +scilab-breakpoint-line-color+ "red")
(defconst +scilab-step-line-color+ "green")
(defvar *scilab-breakpoints* nil
  "List of breakpoints. Each breakpoint is a pair \(function-name
  global-linum\).")
;;(nilf *scilab-breakpoints*)

;;; some utils
(cl-defun add-if (predicate number &optional (addend 1))
  (if predicate (+ number addend) number))
;;(add-if t 10)

(defun scilab-vector (list)
  (concat* list :pre "[" :in ", " :suf "]" :key #'sstring))
;;(scilab-vector (a-b 1 10))

;;; Breakpoint handling
;;; Note that this should not involve 'soft' breakpoints.
;;; To avoid confusion, soft breakpoints should be tagged with 'bpt'
;;; Show breakpoint: only visual, format buffer so the breakpoint is easily seen
;;; Add breakpoint: adds a new breakpoint to the system (*scilab-breakpoints*, Scilab), and shows it
;;; Activate breakpoint: adds a breakpoint already existing in *scilab-breakpoints* to Scilab and show it
;;; Delete breakpoint: removes a breakpoint from the system and hides it
;;; Deactivate breakpoint: removes a breakpoint from SciLab (but not from *scilab-breakpoints*) and hides it

;;; bpt handling, simple UI to turn 'SciLab breakpoint's == 'bpt's on and off
(defun scilab-toggle-fn-bpts (toggle function-name relative-linums)
  "Toggles bpt at the RELATIVE-LINUMS in function FUNCTION-NAME.
RELATIVE-LINUMS could be either a list of integer or a single integer."
  (message "toggle bpt: %S" (list toggle function-name relative-linums))
  (condition-case var
      (mbscilab-eval-raw
       (format "%s('%s', %s)"
	 (case toggle
	   (:on "setbpt")
	   (:off "delbpt")
	   (t (error "In scilab-toggle-fn-bpts: Illegal toggle value")))
	 function-name (scilab-vector (remove 0 (mapcar #'1- relative-linums)))))
    (error (message "%s" (second var)))))

(defun scilab-toggle-bpt (toggle bpt)
  "Toggles bpt at line corresponding to soft breakpoint BPT."
  (scilab-toggle-fn-bpts toggle (first bpt) (list (second bpt))))

(defun scilab-unset-all-bpts ()
  "Deletes all soft breakpoints in every loaded function. Obsolete?"
  (mbscilab-eval-raw "delbpt()"))
;;(scilab-unset-all-bpts)

;;; breakpoint visibility handling
(cl-defun toggle-line-color (toggle linum color &optional (buffer (current-buffer)))
  "LINUM is global buffer line number"
  (message "toggle color line: %S" (list toggle linum color))
   ;; (with-output-to-temp-buffer "backtrace-output"
   ;;   (let ((var 1))
   ;;     (save-excursion
   ;; 	 (setq var (eval '(progn
   ;; 			   (1+ var)
   ;; 			   (print (format "toggle color line: %S" (list toggle linum color)))
   ;; 			   (list 'testing (backtrace))))))))
  (with-buffer buffer
    (destructuring-bind (start end) (line-as-region linum)
      (funcall (case toggle
		 (:on #'add-text-properties)
		 (:off #'remove-text-properties)
		 (t (error "In toggle-line-color: Illegal toggle value")))
	       start (1+ end) `(font-lock-face (:background ,color))))
    (not-modified)))

(cl-defun scilab-hide-all-breakpoints (&optional (beg (point-min)) (end (point-max)))
  "Unformats all breakpoints in current buffer"
  (remove-text-properties beg end '(font-lock-face (:background "red")))
  (not-modified))
;;(scilab-hide-all-breakpoints)

(cl-defun scilab-toggle-breakpoint-visibility (toggle breakpoint &optional buffer)
  "Formats, or unformats, a breakpoint at current line.
If BUFFER is specified the operation is restricted to this buffer."
  (condition-case var
      (destructuring-bind (function-name relative-line) breakpoint
	(with-buffer (or buffer (first (scilab-function-buffer function-name)))
	  (toggle-line-color
	   toggle
	   (scilab-global-linum relative-line (scilab-fn-info-from-name function-name))
	   +scilab-breakpoint-line-color+)))
    (error (message "Warning! no buffer process active."))))

(cl-defun scilab-show-all-breakpoints ()
  "Reformats all breakpoints in all buffers"
  (loop for b in *scilab-breakpoints* do (scilab-toggle-breakpoint-visibility :on b)))
;;(scilab-show-all-breakpoints t)

;;; breakpoint activation
(defun scilab-activate-breakpoint (breakpoint)
  "Activates the existing BREAKPOINT at the current line"
  (when (not (member breakpoint *scilab-breakpoints*))
    (error "Breakpoint %S is not an existing breakpoint" breakpoint))
  (scilab-toggle-bpt :on breakpoint)
  (scilab-toggle-breakpoint-visibility :on breakpoint))

(defun scilab-activate-all-breakpoints ()
  (loop for b in *scilab-breakpoints* do (scilab-activate-breakpoint b)))

(defun scilab-deactivate-breakpoint (breakpoint)
  (scilab-toggle-bpt :off breakpoint)
  (scilab-toggle-breakpoint-visibility :off breakpoint))

(defun scilab-deactivate-all-breakpoints ()
  (loop for b in *scilab-breakpoints* do (scilab-deactivate-breakpoint b)))

(defun scilab-refresh-all-breakpoints ()
  (interactive)
  (scilab-deactivate-all-breakpoints)
  (scilab-activate-all-breakpoints))

;;; breakpoint handling
(defun scilab-create-breakpoint ()
  "Creates a breakpoint for the current line.
See `*scilab-breakpoints*' for breakpoint format."
  (mapprop (scilab-fn-info) '(:name :current-line)))

(defun scilab-breakpoint-exists-p (breakpoint)
  (find breakpoint *scilab-breakpoints* :test #'equal))
;;(scilab-breakpoint-exists-p "mbfct" 2)

(defun scilab-add-breakpoint (breakpoint)
  "Adds a BREAKPOINT at the current line"
  (when (member breakpoint *scilab-breakpoints*)
    (message "Warning! %S is already added to system"))
  (push-unique breakpoint *scilab-breakpoints* #'equal)
  (scilab-activate-breakpoint breakpoint))

(defun scilab-delete-breakpoint (breakpoint)
  "Deletes the BREAKPOINT from the current line"
  (if (not (member breakpoint *scilab-breakpoints*))
    (message "Warning! %S does not exist in system")
    (scilab-deactivate-breakpoint breakpoint)
    (draw breakpoint *scilab-breakpoints* :test #'equal)))

(defun scilab-toggle-breakpoint ()
  "Toggles breakpoint at the current line"
  (interactive)
  (let ((breakpoint (scilab-create-breakpoint)))
    (if (member breakpoint *scilab-breakpoints*)
      (scilab-delete-breakpoint breakpoint)
      (scilab-add-breakpoint breakpoint))))

(defun scilab-delete-all-breakpoints ()
  (interactive)
  (loop for b in *scilab-breakpoints*
	do (scilab-delete-breakpoint b)))


;;; env
(cl-defun scilab-num-lines-fn (&optional (fn-info (scilab-fn-info)))
  (destructuring-bind (first last) (getf fn-info :lines)
    (1+ (- last first))))

(cl-defun relative-linums (global-linums &optional (base 1))
  "Convert GLOBAL-LINUMS to relative linums"
  (mapcar (bind #'- (- (first global-linums) base)) global-linums))
;;(relative-linums (a-b 7 10))

(cl-defun scilab-fn-linums (relative-to &optional (fn-info (scilab-fn-info)))
  (case relative-to
    ((:buffer :file :global) (apply #'a-b (getf fn-info :lines)))
    ((:function t) (relative-linums (scilab-fn-linums :file fn-info)))))

(defun scilab-code-line-p (global-linum buffer)
  "Returns t is line GLOBAL-LINUM in current buffer is a code line"
  (with-buffer buffer
    (goto-line global-linum)
    (scilab-ltype-code)))

(cl-defun scilab-bpt-linums (relative-to &optional (fn-info (scilab-fn-info)))
  "Returns relative linums corresponding to all relevant bpts in current function"
  (case relative-to
    ((:buffer :file) (copy-if (bind #'scilab-code-line-p (getf fn-info :buffer))
		       (scilab-fn-linums :file fn-info)))
    ((:function t) (relative-linums (scilab-bpt-linums :file fn-info)))))
;;(scilab-bpt-linums :buffer ())

(defun fringe-helper-convert (&rest strings)
  "Convert STRINGS into a vector usable for `define-fringe-bitmap'.
Each string in STRINGS represents a line of the fringe bitmap.
Periods (.) are background-colored pixel; Xs are foreground-colored. The
fringe bitmap always is aligned to the right. If the fringe has half
width, only the left 4 pixels of an 8 pixel bitmap will be shown.
For example, the following code defines a diagonal line.
\(fringe-helper-convert
\"XX......\"
\"..XX....\"
\"....XX..\"
\"......XX\"\)"
  (unless (cdr strings)
    ;; only one string, probably with newlines
    (setq strings (split-string (car strings) "\n")))
  (apply 'vector
	 (mapcar
	     (lambda (str)
	       (let ((num 0))
		 (dolist (c (string-to-list str))
		   (setq num (+ (* num 2) (if (eq c ?.) 0 1))))
		 num))
	   strings)))

(define-fringe-bitmap 'dot (fringe-helper-convert
  "........"
  "........"
  " ......."
  "...XX..."
  "..XXXX.."
  ".XXXXXX."
  ".XXXXXX."
  "..XXXX.."
  "...XX..."
  "........"
  "........"
  "........") nil nil 'center)

(defconst +scilab-soft-breakpoint-glyph+
  (propertize (char-to-string ?\uE000)
	      'display
	      '(left-fringe dot font-lock-keyword-face)))

(defun scilab-toggle-visibility-soft-breakpoints (toggle linums buffer)
  "Toggles the soft breakpoint marks in the left fringe of code buffer.
Note: Important to specify BUFFER, since at this point, current buffer could be *scilab*."
  (with-buffer buffer
    (case toggle
      (:on
       (loop for l in linums
	     do (goto-line l)
	     do (let ((overlay (make-overlay (point) (point))))
		  (overlay-put overlay 'before-string +scilab-soft-breakpoint-glyph+))))
      ((:off t)
       (destructuring-bind (beg end) (line-as-region linums buffer)
	 (remove-overlays beg end 'before-string +scilab-soft-breakpoint-glyph+))))))

(defun scilab-toggle-soft-breakpoints (toggle fn-info)
  (let ((linums (scilab-bpt-linums :file fn-info)))
    (scilab-toggle-fn-bpts toggle (getf fn-info :name) (relative-linums linums))
    (scilab-toggle-visibility-soft-breakpoints toggle linums (getf fn-info :buffer))))

(cl-defun scilab-instrument-function (toggle &optional function-name)
  "Instruments current function from current `point' and onwards.
TODO: check if "
  (interactive)
  (let* ((fn-info (if function-name
		    (scilab-fn-info-from-name function-name)
		    (scilab-fn-info))))
    (scilab-toggle-soft-breakpoints toggle fn-info)
    (loop for b in *scilab-breakpoints* do (scilab-activate-breakpoint b))))

(defun scilab-next-statement-line (current-line)
  "Returns the line number of the next statement after CURRENT-LINE.
The line numbers are local to the current function."
  (let ((lines (scilab-defun-lines)))
    (and (< current-line (length lines))
	 (awhen (position-if #'scilab-statement-p lines :start current-line)
	   (1+ it)))))
;;(scilab-next-statement-line 3)

(defun scilab-parse-value (string)
  (if (= (char string 0) ?!)
    (string-trim string "[!\n\t ]*")
    (string-to-number string)))
;;(mapcar #'scilab-parse-value '("!qwe    !" "1.2" "456"))

(defun scilab-parse-answer-group (group)
  (let* ((lines (cl-remove "" (mapcar #'string-trim group) :test #'string=))
	 (symbol (first (read-from-string (first lines))))
	 (values (mapcar #'scilab-parse-value (rest lines))))
    (list symbol (if (stringp (first values))
		   (loop for x in values for i from 1 if (oddp i) collect x)
		   values))))
;;(scilab-parse-answer-group "   ")

(defun scilab-parse-answer (string)
  "Returns the current stack at debug stop point"
  (lexical-let ((re "[A-Za-z0-9]+  ="))
    (let ((groups (rest (cut-if #'(lambda (x) (string-match* re x)) (string-to-lines string) t))))
      (mapcar #'scilab-parse-answer-group groups))))
;;(transpose (mapcar #'second (scilab-parse-answer qwe)))

(defun scilab-debug-stack ()
  (awhen (mapcar #'second (scilab-parse-answer (mbscilab-eval-raw "[x,y]=where()" t)))
    (transpose it)))
;;(scilab-debug-stack)

(defun scilab-debug-current-env () (second (scilab-debug-stack)))

(cl-defun scilab-comint-move-step (&optional (point (point)))
  "Sets soft breakpoint at next breakpoint place in scilab and deletes current soft breakpoint
Note that scilab actually evaluates the statement at a current scilab line"
  (when (scilab-last-line-p fn-info point)
      (scilab-unset-all-bpts)))

(cl-defun scilab-buffer (function)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward function nil t 1)
    (backward-paragraph 1)
    (let ((beg (point))
	  (end (re-search-forward ",[[:digit:]]+$")))
      (list beg end))))
;;(scilab-buffer "scilab_error")

(cl-defun scilab-unmark-step-line (&optional (de (scilab-debug-current-env)))
  (toggle-line-color :off
		     (scilab-global-linum (second de) (scilab-fn-info))
		     +scilab-step-line-color+
		     (first (scilab-function-buffer (first de))))
  (scilab-show-all-breakpoints))

(defun scilab-mark-step-line (function-name relative-linum)
  (let* ((fn-info (scilab-fn-info-from-name function-name))
	 (global-linum (scilab-global-linum relative-linum fn-info))
	 (buffer (getf fn-info :buffer)))
    (with-buffer* buffer
      ;; At this point during execution, current buffer is the
      ;; comint-buffer, so switching buffer here confuses Emacs
      ;; seriously:
      ;; (switch-to-buffer (getf fn-info :buffer))
      ;; Maybe the solutions is to set a call back, or something
      (toggle-line-color :on global-linum +scilab-step-line-color+)
      (switch-to-buffer buffer)
      (goto-line global-linum))))
;;(scilab-mark-step-line :on nil)

(defun scilab-function-name (name-or-env)
  (if (stringp name-or-env)
    name-or-env
    (getf (or name-or-env (scilab-fn-info))
	  :name)))

(defun scilab-continue ()
  "Continue should never care about break points"
  (awhen (scilab-debug-current-env)
    (scilab-unmark-step-line it)
    (mbscilab-eval-raw "resume")))

(defun scilab-resume ()
  "By now, this method (or `scilab-continue') has become superfluous"
  (interactive)
  ;; delete every soft breakpoint in current function
  (scilab-instrument-function :off (first (scilab-debug-current-env)))
  (scilab-continue))

(defun scilab-resume-to-cursor ()
  (interactive)
  (let ((breakpoint (scilab-create-breakpoint)))
    (if (scilab-breakpoint-exists-p breakpoint)
      (scilab-resume)
      ;;else
      (scilab-add-breakpoint breakpoint)
      ;; Note that we have to specify the current debug function (FN)
      ;; Otherwise we will de-instrument the funtion at (point)
      ;; which would be bad if (point)'s function and FN are not the same.
      (scilab-instrument-function :off (first (scilab-debug-current-env)))
      (scilab-resume)
      (scilab-delete-breakpoint breakpoint))))

(defun scilab-step-outof ()
  "TODO"
  (interactive)
  (scilab-instrument-function :off))

(cl-defun scilab-step-over (&optional (fn-info (scilab-fn-info)))
  "F10"
  (interactive)
  (when (scilab-last-line-p fn-info)
    (scilab-instrument-function :off))
  (scilab-continue))

(cl-defun scilab-step-into (&optional (function-name (scilab-function-at-line)))
  (interactive)
  (when function-name
    (scilab-instrument-function :on function-name))
  (scilab-continue))

(cl-defun scilab-abort ()
  (interactive)
  (let ((de (scilab-debug-current-env)))
    (if (not de)
      (warn "Debug session is not active.")
      ;;else
      (remove-overlays)
      (scilab-unmark-step-line de)
      (scilab-unset-all-bpts)
      (mbscilab-eval-raw "abort"))))


(defun scilab-debug-status (&optional string)
  "This is a comint filter function. The purpose is to look for debug status output from Scilab.
If Scilab reports 'Stop after row 6 in function GenerateFFIAOG'
it will mark line 6 in GenerateFFIAOG as a step line."
  (message "scilab-debug-status: got string: %s" string)
  (awhen (and string
	      (string-match* 
	       "Stop after row[[:space:]]+\\([[:digit:]]+\\) in function \\([^.]+\\)\\."
	       string :num '(1 2)))
    (destructuring-bind (srlinum fn) it
      (message "Got string, parsing to %S" it)
      (scilab-instrument-function :on fn) ;;TODO: skip this if the function already has been instrumented
      (scilab-mark-step-line fn (1+ (string-to-number srlinum))))))


(provide 'mbscilab-debug)
