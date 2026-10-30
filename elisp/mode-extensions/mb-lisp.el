;;; mb-lisp.el --- Common Lisp REPL driver and Lisp mode extension -*- lexical-binding: t; -*-

;;; Commentary:

;; Drives an inferior CLISP process over comint: expressions are sent to the
;; REPL, the resulting output batches are collected by a comint filter, parsed
;; back into Elisp values, and reported to the user.  Also provides
;; `mb-lisp-mode', a Lisp mode extension, and `mb-lisp-repl-mode', a read-only
;; mode for interacting with a REPL break loop.
;;
;; Loading this file has no global effect.  Call `mb-lisp-setup' to install the
;; inferior Lisp hook, the `forward-sentence' advice and the mode key bindings.

;;; TODO
;; Extract last output, i.e. the string between the last two prompts in ilb
;; Parse that output.  Different cases
;; * single sexp, ok message
;; * values, ok concat and message
;; * break loop: display ilb in split window and offer a minor mode or something
;;               but it is important to have a quit key, e.g. 'q' like in slime
;;               so at this point a minor mode with some new key defs should be
;;               provided

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'inf-lisp)
(require 'lisp-mode)
(require 'thingatpt)
(require 'derived)
(require 'mb-clhs) ; for (mb-common-lisp-hyperspec)
(require 'mb-anaphora)
(require 'mb-utils-buffer)
(require 'mb-utils-div)
(require 'mb-utils-io)
(require 'mb-utils-math)
(require 'mb-utils-strings)
(require 'mb-utils-time)

(declare-function slime-p "mb-evil")
(declare-function org-table-convert-region "org-table")
(declare-function LilyPond-compile-file "lilypond-mode")
(defvar LilyPond-master-file)
(defvar mb-evil-eval-map)

(defvar *repl-output* nil
  "This is set to last output value from REPL. It is temporarily
  set to nil (and also initially) immediately before new input is
  sent to REPL. In this way, this parameter can be used to check
  the job status of the REPL.")

(defvar *repl-discard-output* nil
  "If true, the result is not printed to REPL")

(defvar *repl-discard-message* nil
  "If true, the result is not printed to in the echo area")

(defvar *repl-comint-output-list* nil
  "This variable is used to collect the output packages
  originating from one single command.")

(defvar *repl-error* nil
  "This variable is used to collect the output packages
  originating from one single command.")

(defvar *repl-last-output-time* nil
  "The time of the last output batch from comint Same format
   as (current-time) When all batches have been received, it must
   be set to NIL")

(defvar *repl-max-waiting-time* (* 1000 15)
  "Maximum allowed waiting time in milliseconds for the first
  output batch from comint after entering a top-level form
  Default is 15 seconds")

(defvar *repl-max-waiting-time-between-outputs* 1000
  "Maximum allowed waiting time in milliseconds between output
  batches from comint.")

(defun repl-abort ()
  "Repeatedly send Abort until the REPL prompt is back at top level.
A true hack..."
  (interactive)
  (while (not (string-match-exact "^\\[[0-9,]*\\]>\\s-*" (line-string)))
    (execute-kbd-macro (kbd "a b o r t RET"))
    (sleep-for 0.1)))

(defun symbol-regexp (symbol)
  "Return a regexp matching SYMBOL as a whole Lisp symbol."
  (format "\\_<%s\\_>" symbol))

(defun search-symbol (symbol)
  "Move point past the next occurrence of SYMBOL.
Return non-nil when SYMBOL was found."
  (re-search-forward (symbol-regexp symbol) nil t 1))

(defun search-form (first-symbol)
  "Move point to the first form (FIRST-SYMBOL ...) in the current buffer.
Return non-nil when such a form was found.  Commented forms are not
recognised; a reader walking every sexp in the buffer would be needed
for that."
  (let ((first-symbol-regexp
         (format "\\s(\\s-*%s" (symbol-regexp first-symbol))))
    (when (re-search-forward first-symbol-regexp nil t 1)
      (backward-up-list 1)
      t)))
;;(search-form 'defun)

(defun buffer-package-name ()
  "Return the package name in use in the current Lisp buffer.
Defaults to COMMON-LISP-USER when no IN-PACKAGE form precedes point."
  (save-excursion
    (if (re-search-backward "^\\s-*(in-package :\\(\\b.*\\b\\))" nil t)
      (match-string-no-properties 1)
      "COMMON-LISP-USER")))
;;(buffer-package-name)

(defun ilb-last-output (max-point)
  "Return the last inferior Lisp buffer output after MAX-POINT.
Waits until the inferior Lisp buffer has grown past MAX-POINT."
  (with-buffer* inferior-lisp-buffer
    ;; wait for output
    (while (= max-point (point-max))
      (sleep-for 0.01))
    (goto-char (point-max))
    (let ((end (re-search-backward "^\\[[0-9]+\\]>" nil t)))
      (when end
        (forward-sexp 1)
        (forward-char 1)
        (prog1 (buffer-substring-no-properties (point) end)
          (goto-char (point-max)))))))

(defun ilb-parse (output)
  "Join the semicolon-separated values in OUTPUT into one string."
  (let ((values (split-string (string-trim output) ";\n")))
    (concat* values :in "; ")))

(defun repl-wait-for-output (&optional max-milliseconds)
  "Wait for REPL output, at most MAX-MILLISECONDS.
MAX-MILLISECONDS defaults to `*repl-max-waiting-time*'.  The deadline is
extended by `*repl-max-waiting-time-between-outputs*' after each batch.
Return `*repl-output*'."
  (let ((max-waiting-time (or max-milliseconds *repl-max-waiting-time*)))
    (cl-loop for i from 0
	     for waiting-time = (expt 2 i)
	     for total-waiting-time = 0 then (+ total-waiting-time waiting-time)
	     while (and (null *repl-output*)
			(not *repl-error*)
			(< total-waiting-time max-waiting-time))
	     do (sleep-for (/ waiting-time 1000.0))
	     if *repl-last-output-time*
	     do (setf max-waiting-time
		      (+ (diff-current-time-msec *repl-last-output-time*
                                                 (current-time))
			 *repl-max-waiting-time-between-outputs*))))
  *repl-output*)
;;(repl-wait-for-output)

(defalias 'repl-wait-for-ouput #'repl-wait-for-output
  "Compatibility alias for the misspelled original name.")

(defun lisp-eval-expression-1 (expression-string &optional waiting-for-result)
  "Send EXPRESSION-STRING to the inferior Lisp process and return the result.
When WAITING-FOR-RESULT is a number, wait at most that many milliseconds;
when it is otherwise non-nil, wait at most 2000 milliseconds.

This should only be used for small commands requiring a single comint
result package, with a fairly short processing time."
  (setf *repl-output* nil)
  (setf *repl-last-output-time* nil)
  (comint-simple-send (inferior-lisp-proc) expression-string)
  (if waiting-for-result
    (repl-wait-for-output
     (if (numberp waiting-for-result) waiting-for-result 2000))
    *repl-output*))
;;(lisp-eval-expression-1 "(let ((res (cl-loop for i below 5000 collect i))) (last res))" t)
;;(lisp-eval-expression-1 "1" t)
;;(comint-simple-send (inferior-lisp-proc) "1")

(defun repl-current-package-name ()
  "Return the name of the package currently in use in the REPL."
  (let ((*repl-discard-message* t)
	(*repl-discard-output* t))
    (cl-first (lisp-eval-expression-1 "(package-name *package*)" nil))))
;;(repl-current-package-name)

(defun repl-set-package (package-name)
  "Switch the REPL to PACKAGE-NAME, a symbol or string."
  (let* ((*repl-discard-message* t)
	 (*repl-discard-output* nil)
	 (arg (format "(in-package %s)"
		(upcase (if (symbolp package-name)
			  (symbol-name package-name)
			  package-name)))))
    (cl-first (lisp-eval-expression-1 arg t))))
;;(repl-set-package ":utils")

(defun mb-lisp-change-in-package (prefix)
  "Switch the REPL to the package of the current buffer.
With PREFIX, prompt for the package name instead."
  (interactive "P")
  (let ((package (buffer-package-name))
	(repl-package (repl-current-package-name)))
    (repl-set-package
     (if prefix
       (read-from-minibuffer "Package name: ")
       (if (or (not repl-package)
	       (string= (upcase package) repl-package))
	 :cl-user package)))))

(defmacro repl-with-buffer-package (&rest body)
  "Evaluate BODY with the REPL switched to the current buffer's package.
The previous REPL package is restored afterwards."
  (declare (indent 0) (debug t))
  (let ((current (make-symbol "current"))
        (wanted (make-symbol "wanted")))
    `(let ((,current (repl-current-package-name))
           (,wanted (buffer-package-name)))
       (if (cl-equalp ,current ,wanted)
         (progn ,@body)
         (unwind-protect
             (progn (repl-set-package ,wanted) ,@body)
           (repl-set-package ,current))))))

(defun lisp-eval-print-result (result)
  "Report RESULT, a list of REPL values, to the user.
Large vectors are rendered as an Org table instead."
  (if (and (vectorp (cl-first result))
	   (> (mvec-size (cl-first result)) 20))
    (mvec->table (cl-first result))
    (message "==> %s"
	     (concat* result
	       :in "; "
	       :key (lambda (x) (if (stringp x)
				  (format "\"%s\"" x)
				  (sstring x)))))))

(defun lisp-eval-expression (expression-string)
  "Send EXPRESSION-STRING to the inferior Lisp process and report the result.
The REPL is temporarily switched to the package of the current buffer.
Return the first returned value."
  (setf *repl-error* nil)
  (setf *repl-comint-output-list* nil)
  (repl-with-buffer-package
    (let ((result (lisp-eval-expression-1 expression-string t)))
      (lisp-eval-print-result result)
      (when *repl-error* (mb-lisp-repl-mode))
      (cl-first result))))

(defun multi-substitute (map sequence &rest plist)
  "Substitute all OLD elements in MAP with its corresponding NEW value.
MAP is an alist with elements (NEW . OLD). The method calls
repeatedly `cl-substitute' and all keywords in `cl-substitute'
are allowed.
 
   Note: in this version be careful when an element is both a NEW
   and an OLD value."
  (if map
    (apply #'multi-substitute (cl-rest map)
	   (apply #'cl-substitute
		  (car (cl-first map))
		  (cdr (cl-first map))
		  sequence plist)
	   plist)
    sequence))
;;(multi-substitute '((?a . ?b) (?n . ?c)) "abc")

(defun string-to-valid-pathname (string)
  "Replace characters in STRING that are invalid in a pathname.
Note that the CARs of the substitution map are the resulting characters."
  (multi-substitute '((?_ . ? )
		      (?! . ?\\)
		      (?! . ?/)
		      (?- . ?:)
		      (?\L . ?\()
		      (?\R . ?\))
		      (?P . ?+)
		      (?G . ?<)
		      (?L . ?>)
		      (?Q . ?\")
		      (?H . ?#))
  (cl-remove ?' string)))
;;(string-to-valid-pathname "qwe '()+")

(defun mb-lisp-eval-1 (expression-string prefix)
  "Evaluate EXPRESSION-STRING in the REPL, dispatching on PREFIX.
EXPRESSION-STRING must be a string ready to send to the REPL, so any
embedded strings have to be quoted.  PREFIX 0 renders the result as a
music PDF, 1 prints it in ANSI format, 2 opens a gnuplot target file,
and any other prefix inserts the result at point."
  (if prefix
    (cl-case prefix
      ;; yamal: view music PDF on the fly
      (0 (let ((res (lisp-eval-expression
		     (format "(print-music %s :lilypond-file)"
		       expression-string))))
	   (if res
	     (pdf-music
	      res (format "yamal-%s-"
		    (string-to-valid-pathname expression-string))))))
      ;; yamal: print music expression in ANSI format
      (1 (lisp-eval-expression
	  (format "(print-music %s :ANSI)" expression-string)))
      ;; gnuplot
      (2 (let ((res (lisp-eval-expression expression-string)))
	   (awhen (cl-getf res :TARGET) ;case sensitive!
	     (if (string= (file-name-extension it) "pdf")
	       (find-file it)
	       (message "Target file is not viewable")))))
      (t (insert (format "%S" (lisp-eval-expression expression-string)))))
    (lisp-eval-expression expression-string)))

(defun mb-previous-sexp ()
  "Return the sexp immediately to the left of point, as a string.
Unlike `sexp-at-point' this does not use `read-from-string', so it
tolerates Common Lisp syntax such as #(1 2 3) that the Elisp reader
rejects."
  (let ((end (point)))
    (save-excursion
      (backward-sexp 1)
      (string-trim (buffer-substring-no-properties (point) end)))))
;;(mb-previous-sexp)

(defun mb-lisp-eval-sexp (prefix)
  "Evaluate the sexp before point in the REPL, dispatching on PREFIX."
  (interactive "P")
  (mb-lisp-eval-1 (mb-previous-sexp) prefix))

(defun mb-lisp-eval-defun (prefix)
  "Evaluate the defun at point in the REPL, dispatching on PREFIX."
  (interactive "P")
  (let ((defun-string (thing-at-point 'defun)))
    (unless defun-string
      (user-error "Point is not inside a defun"))
    (mb-lisp-eval-1 (substring-no-properties defun-string) prefix)))

(defun mb-lisp-eval-region (beg end)
  "Evaluate the region between BEG and END in the REPL.
Any IN-PACKAGE form in the region is stripped."
  (interactive "r")
  (mb-lisp-eval-1
   (replace-regexp-in-string "(in-package :[^)]*)" ""
                             (buffer-substring-no-properties beg end))
   nil))

(defun mb-lisp-eval-buffer ()
  "Evaluate the whole buffer in the REPL."
  (interactive)
  (mb-lisp-eval-region (point-min) (point-max)))

(defun mb-lisp-load-buffer (prefix)
  "Ask the REPL to LOAD the file of the current buffer, dispatching on PREFIX."
  (interactive "P")
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (mb-lisp-eval-1 (format "(load \"%s\")" file) prefix)))

(defun inferior-lisp-program-with-lispinit ()
  "Return a CLISP command line that preloads the personal memory image."
  (format "clisp -q -norc -M %s" (file-truename "~/lispinit.mem")))

(define-derived-mode mb-lisp-mode lisp-mode "MB-Lisp"
  "An extension of Lisp mode.

\\{mb-lisp-mode-map}"
  (setq-local paragraph-start "\"\\|\f\\|[ \t]*$"))

(defun mblisp-show-process-buffer ()
  "Display the inferior Lisp buffer, starting the process when needed."
  (interactive)
  (aif (lisp-process-buffer)
    (display-buffer it '(display-buffer-use-some-window))
    (inferior-lisp inferior-lisp-program)))

(defun lisp-process-buffer ()
  "Return the inferior Lisp buffer, or nil when there is none."
  (if (boundp 'inferior-lisp-buffer)
    inferior-lisp-buffer))
;;(lisp-process-buffer)

(defun mb-inferior-lisp-set-locals ()
  "Install the personal comint output filters in the inferior Lisp buffer."
  (define-key inferior-lisp-mode-map [(f12)] #'repl-abort)
  (setq-local comint-output-filter-functions
	      '(mb-lisp-scroll-to-bottom
		shell-strip-ctrl-m
		mb-lisp-on-output)))
(defvar mb-lisp-mode-bindings
  '(("C-x C-e" . mb-lisp-eval-sexp)
    ("C-c e"   . mb-lisp-eval-buffer)
    ("C-c C-e" . mb-lisp-load-buffer)
    ("C-c r"   . mb-lisp-eval-buffer)
    ("C-M-x"   . mb-lisp-eval-defun)
    ("C-c z"   . mb-lisp-change-in-package)
    ("C-h f"   . mb-common-lisp-hyperspec)
    ("C-;"     . comment-region))
  "Alist of key descriptions and commands for `mb-lisp-mode-map'.")

(defun mb-lisp-setup ()
  "Install the personal Lisp integration.
Binds the keys in `mb-lisp-mode-bindings', arranges for the comint
output filters to be installed in inferior Lisp buffers, and restricts
`forward-sentence' to Lisp documentation strings.  Safe to call more
than once."
  (pcase-dolist (`(,key . ,command) mb-lisp-mode-bindings)
    (define-key mb-lisp-mode-map (kbd key) command))
  (add-hook (derived-mode-hook-name 'inferior-lisp-mode)
            #'mb-inferior-lisp-set-locals)
  (advice-add 'forward-sentence :around #'restrict-to-lisp-doc)
  (when (boundp 'mb-evil-eval-map)
    (define-key mb-evil-eval-map "p" #'slime-plot-last-result)))

(defun mb-lisp-teardown ()
  "Undo the global effects of `mb-lisp-setup'."
  (remove-hook (derived-mode-hook-name 'inferior-lisp-mode)
               #'mb-inferior-lisp-set-locals)
  (advice-remove 'forward-sentence #'restrict-to-lisp-doc))

(defun tree->mvec (tree)
  "Convert TREE, a nested list, into a nested vector."
  (if (atom tree) tree (cl-map 'vector #'tree->mvec tree)))
;;(tree->mvec '((1 2)))

(defun mvec-dimensions (mvec)
  "Return the dimensions of the nested vector MVEC as a list."
  (if (vectorp mvec)
    (if (zerop (length mvec))
      0
      (cons (length mvec) (mvec-dimensions (elt mvec 0))))))
;;(mvec-dimensions (tree->mvec '(((1 2) (1 2)) ((1 2) (1 2)) ((1 2) (1 2)))))

(defun tree-dimensions (tree)
  "Return the dimensions of the nested list TREE as a list."
  (when (consp tree)
    (cons (length tree) (tree-dimensions (cl-first tree)))))
;;(tree-dimensions '(((1 2) (1 2)) ((1 2) (1 2)) ((1 2) (1 2))))

(defun mvec->tree (x)
  "Convert the nested vector X into a nested list."
  (if (vectorp x) (cl-map 'list #'mvec->tree x) x))
;;(mvec->tree (tree->mvec '((1 2))))

(defun mvec->string (mvec)
  "Render the nested vector MVEC as space-separated CSV text."
  (let ((tree (mvec->tree mvec)))
    (csv-string (if (= (length (tree-dimensions tree)) 1)
		  (transpose (list tree)) tree)
		" ")))
;;(mvec->string (tree->mvec '((1 2) (3 4))))
;;(mvec->string (tree->mvec (a-b 0 10)))

(defun mvec->table (mvec &optional buffer)
  "Display the nested vector MVEC as an Org table in BUFFER."
  (let ((target (or buffer (get-buffer-create "*mvec*"))))
    (with-buffer* target
      (org-mode)
      (insert (mvec->string mvec))
      (org-table-convert-region (point-min) (point-max)))
    (switch-to-buffer target)))
;;(mvec->table (tree->mvec '((1 2) (3 4))))

(defun mvec-size (mvec)
  "Return the number of atoms in the nested vector MVEC."
  (if (vectorp mvec)
    (sum (cl-map 'list #'mvec-size mvec)) 1))
;;(mvec-size [[1 2 3] [1 2 3]])

(defun eread (string)
  "Read the Common Lisp expression in STRING as an Elisp object.
Handles CL print syntax that the Elisp reader rejects:

  #(1 2 3)              --> [1 2 3]
  #2A((1 2) (3 4))      --> ((1 2) (1 2)) ;i.e. a tree

Unreadable objects and pathnames are converted to strings:

  #P/home/MBe/.emacs.d/ --> \"/home/MBe/.emacs.d\"
  #<PACKAGE EGINA>      --> \"#<PACKAGE EGINA>\"

Return nil when STRING is not a string, is empty, or cannot be read.

TODO: atomic expressions are handled correctly, but they fail as
elements of a list.  For instance #P/home/MBe/.emacs.d/ works, while
\(#P/home/MBe/.emacs.d/) does not.  A proper recursive reader over the
buffer region would be needed to fix this."
  (when (stringp string)
    ;; Vector syntax is rewritten before matching, so #( never reaches the
    ;; clauses below; it is read back as a (vector ...) list.
    (let ((s (replace-regexp-in-string "#(" "(vector " string)))
      (acond
        ;;array
        ((string-match* "#.A(" s)
         (tree->mvec (read (substring s (1- (length it))))))
        ((string-match* "#.A(\\(.*\\))" s :num 1)
         (tree->mvec (read (format "(%s)" it))))
        ;;object
        ((string-match* "#<\\(.*\\)>" s :num 1)
         (format "(%s)" it))
        ;;path
        ((string-match* "#P\"\\(.*\\)\"" s :num 1)
         (format "(%s)" it))
        ((string-match* "\\([[:digit:].]+\\)L0" s :num 1)
         (read it))
        (t ;; `acond' binds `it' in every clause; the fallback has no use for it
         (ignore it)
         (unless (empty-string-p s)
	   (condition-case nil
	       (read s)
	     (error nil))))))))
;;(mapcar #'eread '("asdf#<PACKAGE YAMAL-MODEL>" nil))

(defun mb-lisp-process-output ()
  "Parse the collected REPL output batches into Elisp objects.
Sets `*repl-output*' once a complete prompt has been seen.  Multiple
values are returned as a list, cf. `cl-values'."
  (let ((string (concat* (reverse *repl-comint-output-list*))))
    (when (string-match "^[A-Z0-9-.]*\\[[0-9,]+\\]> " string)
      (setf *repl-comint-output-list* nil)
      (setf *repl-output*
            (mapcar #'eread
                    (split-string (cl-substitute 10 13 string) " ;\n\n"))))))

(defun mb-lisp-scroll-to-bottom (_string)
  "Scroll every window showing the current buffer to its end.
_STRING is the comint output and is ignored."
  (dolist (w (get-buffer-window-list (current-buffer) nil t))
    (set-window-point w (point-max))))

(defconst +repl-error-regexp+
  (regexp-opt '("*** - "
		"** - Continuable Error"))
  "Regexp matching the start of a CLISP error report.")

(defvar *repl-window-list* nil
  "The window list recorded when `mb-lisp-repl-mode' was entered.
If the Lisp process window is not among them, it is deleted when
leaving `mb-lisp-repl-mode'.")

(defvar-local mb-lisp-repl-mode-p nil
  "Non-nil in buffers currently in `mb-lisp-repl-mode'.")

(defun repl-error-p (string)
  "Return non-nil when STRING reports a REPL error."
  (string-match +repl-error-regexp+ string))

(defun mb-lisp-on-output (string)
  "Collect and parse the comint output STRING from the REPL.
Called by comint whenever the process emits output.  STRING is passed
through unchanged unless `*repl-discard-output*' is non-nil, in which
case the empty string is returned."
  (push string *repl-comint-output-list*)
  (mb-lisp-process-output)
  (when (repl-error-p string)
    (setf *repl-error* t))
  (setf *repl-last-output-time* (current-time))
  (if *repl-discard-output*
    "" string))

(defun repl-send (command)
  "Send COMMAND, a string, as one line to the inferior Lisp process."
  (comint-simple-send (inferior-lisp-proc) (format "%s\n" command)))

(defun repl-read-expression (from-minibuffer-p)
  "Send an expression to the REPL, qualified by the buffer package.
With FROM-MINIBUFFER-P, prompt for the expression; otherwise use the
sexp before point."
  (let ((expression (if from-minibuffer-p
                      (read-from-minibuffer "Eval: ")
                      (mb-previous-sexp)))
	(package-name (buffer-package-name)))
    (repl-send (if package-name
                 (format "%s::%s" package-name expression)
                 expression))))

(defmacro repl-define-simple-key-map (map command-string repl-command)
  "Bind COMMAND-STRING in MAP to sending REPL-COMMAND to the REPL."
  `(define-key ,map ,command-string
     (lambda ()
       (interactive)
       (repl-send ,repl-command))))

(defmacro repl-define-simple-key-maps (map char-sequence)
  "Bind each character C of CHAR-SEQUENCE in MAP to sending :C."
  `(progn ,@(mapcar (lambda (x)
		      `(repl-define-simple-key-map
			,map (string ,x) (format ":%c" ,x)))
		    char-sequence)))

(defmacro repl-define-number-key-maps (map number-sequence)
  "Bind each character C of NUMBER-SEQUENCE in MAP to sending :RC."
  `(progn ,@(mapcar (lambda (x)
		      `(repl-define-simple-key-map
			,map (string ,x) (format ":R%c" ,x)))
		    number-sequence)))

(defun exit-mb-lisp-repl-mode (&optional send-abort-message-p)
  "Leave `mb-lisp-repl-mode' and return to `mb-lisp-mode'.
With SEND-ABORT-MESSAGE-P, also send an abort to the REPL."
  (interactive)
  (mb-lisp-mode)
  (read-only-mode -1)
  (when send-abort-message-p
    (repl-send "abort")))

(defun mb-lisp-repl-quit ()
  "Leave `mb-lisp-repl-mode' without aborting the REPL."
  (interactive)
  (exit-mb-lisp-repl-mode nil))

(defun mb-lisp-repl-abort-and-quit ()
  "Abort the REPL and leave `mb-lisp-repl-mode'."
  (interactive)
  (exit-mb-lisp-repl-mode t))

(defun mb-lisp-repl-eval-from-minibuffer ()
  "Prompt for an expression and send it to the REPL."
  (interactive)
  (repl-read-expression t))

(defun mb-lisp-repl-eval-previous-sexp ()
  "Send the sexp before point to the REPL."
  (interactive)
  (repl-read-expression nil))

(defvaralias '+mb-lisp-repl-mode-map+ 'mb-lisp-repl-mode-map
  "Compatibility alias for `mb-lisp-repl-mode-map'.")

(defvar mb-lisp-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "Q" #'mb-lisp-repl-quit)
    (define-key map "q" #'mb-lisp-repl-abort-and-quit)
    (define-key map "E" #'mb-lisp-repl-eval-from-minibuffer)
    (define-key map "S" #'mb-lisp-repl-eval-previous-sexp)
    (repl-define-simple-key-map map "c" "continue")
    (repl-define-simple-key-maps map "wudtbe")
    (repl-define-number-key-maps map "123456789")
    map)
  "Keymap for read-only interaction with the REPL break loop.")

(define-derived-mode mb-lisp-repl-mode fundamental-mode "MB-Lisp REPL"
  "Control the Common Lisp REPL from the current buffer.

\\{mb-lisp-repl-mode-map}"
  (setf *repl-window-list* (window-list))
  (read-only-mode 1)
  (setq mb-lisp-repl-mode-p t)
  (when (and (boundp 'inferior-lisp-buffer) inferior-lisp-buffer)
    (display-buffer inferior-lisp-buffer)))

;;(require 'lilypond-mode)
(defun pdf-music (string &optional prefix)
  "Convert the LilyPond source STRING to a PDF file and open it.
The file name is generated under the temporary directory.  PREFIX, by
default \"yamal\", tags these temporary files."
  (interactive "sLilyPond source: ")
  (unless (fboundp 'LilyPond-compile-file)
    (user-error "LilyPond mode is not available"))
  (let* ((temp-file (make-temp-file (or prefix "yamal")))
	 (temp-ly-file (format "%s.ly" temp-file))
         (temp-pdf-file (format "%s.pdf" temp-file))
         finish)
    (with-temp-file temp-ly-file
      (insert string))
    (setq finish
          (lambda (_buffer status)
            (remove-hook 'compilation-finish-functions finish)
            (when (string-match "finished" status)
              (find-file temp-pdf-file)
              (other-window 1)
              (bury-buffer)
              (other-window 1))))
    (add-hook 'compilation-finish-functions finish)
    (let ((LilyPond-master-file temp-ly-file))
      (LilyPond-compile-file
       (format "lilypond %s" (shell-quote-argument temp-ly-file))
       "*LilyPond-compile*"))))

(defun lisp-doc-region ()
  "Return the region (BEG END) of the docstring of the defun at point.
Return nil when the defun has no documentation string.  Note that this
does not work for the docstrings of defclass, defconstant and friends."
  (save-excursion
    (beginning-of-defun-raw 1)
    (forward-char 1)
    ;; Protect the forward-sexp ops in case the defun is malformed.
    (condition-case nil
	;; Believe me, (forward-sexp 3) followed by (sexp-at-point) is not
	;; guaranteed to return the doc string; these two forms are.
	(progn
	  (forward-sexp 4)
	  (forward-sexp -1)
	  (let ((doc (sexp-at-point)))
	    (when (stringp doc)
	      (let ((beg (point))
		    (end (scan-sexps (point) 1)))
		(list beg end)))))
      (error nil))))
;;(lisp-doc-region)

(defun restrict-to-lisp-doc (orig-fun &rest args)
  "Call ORIG-FUN with ARGS narrowed to the docstring of the defun at point.
Used as :around advice for `forward-sentence' so that sentence motion
stays inside a Lisp documentation string.  When point is not in a Lisp
buffer, or the defun has no docstring, ORIG-FUN is called unchanged."
  (let ((region (and (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                     (lisp-doc-region))))
    (if region
      (cl-destructuring-bind (beg end) region
        (save-restriction
	  (narrow-to-region (1+ beg) (1- end))
	  (apply orig-fun args)))
      (apply orig-fun args))))

;;; slime/evil additions
(defun slime-plot-last-result ()
  "Open the most recent PDF path mentioned in the *Messages* buffer."
  (interactive)
  (when (and (fboundp 'slime-p) (slime-p))
    (with-current-buffer "*Messages*"
      (eob)
      (bol :offset -1)
      (awhen (string-match* "[^ \"]*pdf" (line-string))
	(find-file-other-window it)))))

(provide 'mb-lisp)

;;; mb-lisp.el ends here
