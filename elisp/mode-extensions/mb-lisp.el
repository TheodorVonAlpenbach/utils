;;;; TODO
;;;; Extract last output, i.e. the string between the last two prompts in ilb
;;;; Parse that output. Different cases
;;;; * single sexp, ok message
;;;; * values, ok concat and message
;;;; * break loop: display ilb in split window and offer a minor mode or something
;;;;               but it is important to have a quit key, e.g. 'q' like in slime
;;;;               so at this point a minor mode with some new key defs should be provided
(require 'mb-clhs) ; for (common-lisp-hyperspec)

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
  "Repeats Abort until the prompt is top-level.
A true hack..."
  (interactive)
  (while (not (regexp-equal "^\\[[0-9,]*\\]>\\s-*" 
			    (current-line-as-string)))
    (execute-kbd-macro "abort\n")
    (sleep-for 0 100))
;;  (execute-kbd-macro "(in-package :cl-user)\n")
  )

(defun symbol-regexp (symbol)
  (format "\\_<%s\\_>" symbol))

(defun search-symbol (symbol)
  "SEXP is an atom or a list. Currently only supports atoms, i.e. symbols."
  (re-search-forward (symbol-regexp (symbol)) nil t 1))

(cl-defun search-form (first-symbol &optional (skip-commented-forms t))
  "Moves point to the first form (FIRST-SYMBOL ...) in the
current buffer. Cant't handle comments. Should do. Instead make a
recursive function transversing through all sexps in buffer. "
  (let ((first-symbol-regexp (format "\\s(\\s-*%s" (symbol-regexp first-symbol))))
    (when (re-search-forward first-symbol-regexp nil t 1)
      (progn (backward-up-list 1) t))))
;;(search-form 'defun)

(defun buffer-package-name ()
  "Returns the package name in use in the current lisp buffer.
If no IN-PACKAGE declaration is found, NIL is returned."
  (save-excursion
    (if (re-search-backward "^\\s-*(in-package :\\(\\b.*\\b\\))" nil t)
      (match-string-no-properties 1)
      "COMMON-LISP-USER")))
;;(buffer-package-name)

(defun ilb-last-output (max-point)
  "Returns the last ilb output. Assumes current-buffer is the
  inferior lisp buffer."
  (with-buffer* inferior-lisp-buffer
    ;; wait for output
    (while (= max-point (point-max))
      (sleep-for 0 10))
    (goto-char (point-max))
    (let* ((b (re-search-backward "^\\[\\([0-9]+\\)\\]>" nil t))
	   ())
      (re-search-backward "^\\[[0-9]+\\]>" nil t)
	(forward-sexp 1)
	(forward-char 1)
	(prog1 (buffer-substring-no-properties (point) b)
	  (goto-char (point-max))))))

(defun ilb-parse (output)
  (let ((values (split-string (string-trim output) ";\n")))
    (concat* values :in "; ")))

(cl-defun repl-wait-for-ouput (&optional (max-milliseconds *repl-max-waiting-time*))
  (let ((max-waiting-time *repl-max-waiting-time*))
    (loop for i from 0
	  for waiting-time = (expt 2 i)
	  for total-waiting-time = 0 then (+ total-waiting-time waiting-time)
	  while (and (null *repl-output*)
		     (not *repl-error*)
		     (< total-waiting-time max-waiting-time))
	  do (sleep-for 0 waiting-time)
	  if *repl-last-output-time*
	  do (setf max-waiting-time
		   (+ (diff-current-time-msec *repl-last-output-time* (current-time))
		      *repl-max-waiting-time-between-outputs*))))
  *repl-output*)
;;(repl-wait-for-ouput)

(defun lisp-eval-expression-1 (expression-string &optional waiting-for-result)
  "Send EXPRESSION-STRING to the inferior Lisp process and return the result.
This should only be used for 'small' commands requiring just one
comint result text package to be sent. Also, the process time
should be fairly small"
  (setf *repl-output* nil)
  (setf *repl-last-output-time* nil)
  (comint-simple-send (inferior-lisp-proc) expression-string)
  (if waiting-for-result
    (repl-wait-for-ouput (or (numberp waiting-for-result) 2000))
    *repl-output*))
;;(lisp-eval-expression-1 "(let ((res (loop for i below 5000 collect i))) (last res))" t)
;;(lisp-eval-expression-1 "1" t)
;;(comint-simple-send (inferior-lisp-proc) "1")

(defun repl-current-package-name ()
  (let ((*repl-discard-message* t)
	(*repl-discard-output* t))
    (first (lisp-eval-expression-1 "(package-name *package*)" t))))
;;(repl-current-package-name)

(defun repl-set-package (package-name)
  (let* ((*repl-discard-message* t)
	 (*repl-discard-output* nil)
	 (arg (format "(in-package %s)" 
		(upcase (if (symbolp package-name)
			  (symbol-name package-name)
			  package-name))))
	 (res (first (lisp-eval-expression-1 arg t))))))
;;(repl-set-package ":utils")

(defun mb-lisp-change-in-package (prefix)
  (interactive "P")
  (let ((buffer-package-name (buffer-package-name))
	(repl-package-name (repl-current-package-name)))
    (repl-set-package
     (if prefix
       (read-from-minibuffer "Package name: ")
       (if (or (not repl-package-name)
	       (string= (upcase buffer-package-name) repl-package-name))
	 :cl-user buffer-package-name)))))

(defmacro repl-with-buffer-package (&rest body)
  `(let ((current-package-name (repl-current-package-name))
	 (buffer-package-name (buffer-package-name)))
     (assert current-package-name)
     (if (equalp current-package-name buffer-package-name)
       (progn ,@body)
       (prog2 
	   (repl-set-package buffer-package-name)
	   ,@body
	 (repl-set-package current-package-name)))))
  (def-edebug-spec repl-with-buffer-package t)

(defun lisp-eval-print-result (result)
  (if (and (vectorp (first result))
	   (> (mvec-size (first result)) 20))
    (mvec->table (first result))
    (message "==> %s"
	     (concat* result
	       :in "; "
	       :key #'(lambda (x) (if (stringp x)
				    (format "\"%s\"" x)
				    (sstring x)))))))

(defun lisp-eval-expression (expression-string)
    "Sends EXPRESSION-STRING to the inferior Lisp process.

TODO: prefix argument means to keep package. Default behaviour is
temporarily to switch to the package defined in the buffer is
this is different from the REPL package."
    (setf *repl-error* nil)
    (setf *repl-comint-output-list* nil)
    (repl-with-buffer-package
     (let ((result (lisp-eval-expression-1 expression-string t)))
       (lisp-eval-print-result result)
       (when *repl-error* (mb-lisp-repl-mode))
       ;;(eread (first result)) doesnt work for strings
       (first result))))

(defun lisp-eval-expression-old (expression-string)
  "Sends EXPRESSION-STRING to the inferior Lisp process.

TODO: prefix argument means to keep package. Default behaviour is
temporarily to switch to the package defined in the buffer is
this is different from the REPL package."
  (setf *repl-error* nil)
  (setf *repl-comint-output-list* nil)
  (repl-with-buffer-package
   (let ((result (lisp-eval-expression-1 expression-string t)))
     (message "==> %s" (concat* result
			 :in "; " :key #'(lambda (x)
					   (if (stringp x)
					     (format "\"%s\"" x)
					     (sstring x)))))
     (when *repl-error* (mb-lisp-repl-mode))
     ;;(eread (first result)) doesnt work for strings
     (first result))))

(defun multi-substitute (map sequence &rest plist)
  "Substitute all OLD elements in MAP with its corresponding NEW value.
MAP is an alist with elements (NEW . OLD). The method calls
repeatedly `cl-substitute' and all keywords in `cl-substitute'
are allowed.
 
   Note: in this version be careful when an element is both a NEW
   and an OLD value."
  (if map
    (apply #'multi-substitute (rest map)
	   (apply #'cl-substitute
		  (car (first map))
		  (cdr (first map))
		  sequence plist)
	   plist)
    sequence))
;;(multi-substitute '((?a . ?b) (?n . ?c)) "abc")

(defun string-to-valid-pathname (string)
  "Note that the CARs are the resulting character"
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
  "EXPRESSION-STRING is a string that can be sent to repl. Note
that strings must be quoted withing such strings."
  (if prefix
    (case prefix
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
	   (awhen (getf res :TARGET) ;case sensitive!
	     (if (string= (file-name-extension it) "pdf")
	       (find-file it)
	       (message "Target file is not viewable")))))
      (t (insert (format "%S" (lisp-eval-expression expression-string)))))
    (lisp-eval-expression expression-string)))

(defun mb-previous-sexp ()
  (save-excursion 
     (backward-sexp 1) 
     (prin1-to-string (sexp-at-point))))
;;(mb-previous-sexp)

(defun mb-previous-sexp ()
  "Returns the sexp form immediately to the left of point.
This is a more robust version than the previous one, which used
`sexp-at-point'. Nothing is wrong with sexp-at-point in Elisp,
but since it uses `read-from-string', it is vulnerable to
non-Elisp syntax in Common Lisp. E.g. #(1 2 3) fails in
`read-from-string'."
  (let ((end (point)))
    (save-excursion 
      (backward-sexp 1) 
      (string-trim (buffer-substring-no-properties (point) end)))))
;;(mb-previous-sexp)

(defun mb-lisp-eval-sexp (prefix)
  (interactive "P")
  (mb-lisp-eval-1 (mb-previous-sexp) prefix))

(defun mb-lisp-eval-defun (prefix)
  (interactive "P")
  (mb-lisp-eval-1 (substring-no-properties (thing-at-point 'defun)) prefix))

(defun mb-lisp-eval-region (beg end)
  (interactive "r")
  (mb-lisp-eval-1 (replace-regexp-in-string "(in-package :[^)]*)" ""
						   (buffer-substring-no-properties beg end))
		  nil))

(defun mb-lisp-eval-buffer ()
  (interactive "")
  (mb-lisp-eval-region (point-min) (point-max)))

(defun mb-lisp-load-buffer (prefix)
  (interactive "P")
  (mb-lisp-eval-1 (format "(load \"%s\")" (buffer-file-name)) prefix))

(defun inferior-lisp-program-with-lispinit ()
  (format "clisp -q -norc -M %s" (file-truename "~/lispinit.mem")))

(setq-local paragraph-start "\"\\|\f\\|[ \t]*$")
(setq-local paragraph-start "\"")

(defvar mb-lisp-mode-hook nil)
(define-derived-mode mb-lisp-mode lisp-mode "MB-Lisp"
  "An extension of Lisp mode
\\{mb-lisp-mode-map\\}"
  (run-hooks))

(defun lisp-process-buffer ()
  (if (boundp 'inferior-lisp-buffer)
    inferior-lisp-buffer))

(defun mb-inferior-lisp-set-locals ()
  (define-key inferior-lisp-mode-map [(f12)] 'repl-abort)
  (setq-local comint-output-filter-functions
	      '(mb-lisp-scroll-to-bottom
		shell-strip-ctrl-m
		mb-lisp-on-output)))
(add-hook (derived-mode-hook-name 'inferior-lisp-mode) #'mb-inferior-lisp-set-locals)

(define-key mb-lisp-mode-map "\C-x\C-e" 'mb-lisp-eval-sexp)
(define-key mb-lisp-mode-map "\C-ce" 'mb-lisp-eval-buffer)
(define-key mb-lisp-mode-map "\C-c\C-e" 'mb-lisp-load-buffer)
(define-key mb-lisp-mode-map "\C-cr" 'mb-lisp-eval-buffer)
(define-key mb-lisp-mode-map "\C-\M-x" 'mb-lisp-eval-defun)
(define-key mb-lisp-mode-map "\C-cz" 'mb-lisp-change-in-package)
(define-key mb-lisp-mode-map "\C-hf" 'mb-common-lisp-hyperspec)
(define-key mb-lisp-mode-map [(control \;)] 'comment-region)

;;; Smart comment stuff
(defun commented-string-p (string)
  "Returns nil iff current line is not commented"
  (string-match* "^[[:space:]]*;" string))

(defun commented-line-p ()
  "Returns nil iff current line is not commented"
  (commented-string-p (current-line-as-string)))
;;(commented-line-p)

(defun toggle-comment-line ()
  (interactive)
  (if (commented-line-p)
    (comment-line '(4)) ;; this equals C-u prefix
    (comment-line 2)))

(defun toggle-comment-region (beg end)
  (interactive "r")
  (when (< beg end)
    (save-excursion
      (goto-char beg)
      (bol)
      (while (< (point) end)
	(toggle-comment-line)
	(forward-line 1)))))


;;(nilf comint-preoutput-filter-functions)
;;(pushnew #'mb-lisp-on-output comint-preoutput-filter-functions)

(defun tree->mvec (tree)
  (if (atom tree) tree (map 'vector #'tree->mvec tree)))
;;(tree->mvec '((1 2)))

(defun mvec-dimensions (mvec)
  (if (vectorp mvec)
    (if (zerop (length mvec))
      0
      (cons (length mvec) (mvec-dimensions (elt mvec 0))))))
;;(mvec-dimensions (tree->mvec '(((1 2) (1 2)) ((1 2) (1 2)) ((1 2) (1 2)))))

(defun tree-dimensions (tree)
  (when (consp tree)
    (cons (length tree) (tree-dimensions (first tree)))))
;;(tree-dimensions '(((1 2) (1 2)) ((1 2) (1 2)) ((1 2) (1 2))))

(defun mvec->tree (x)
  (if (vectorp x) (map 'list #'mvec->tree x) x))
;;(mvec->tree (tree->mvec '((1 2))))

(defun mvec->string (mvec)
  (let ((tree (mvec->tree mvec)))
    (csv-string (if (= (length (tree-dimensions tree)) 1)
		  (transpose (list tree)) tree)
		" ")))
;;(mvec->string (tree->mvec '((1 2) (3 4))))
;;(mvec->string (tree->mvec (a-b 0 10)))

(cl-defun mvec->table (mvec &optional (buffer (get-buffer-create "*mvec*")))
  (with-buffer* buffer
    (org-mode)
    (insert (mvec->string mvec))
    (mark-whole-buffer)
    (org-table-create-or-convert-from-region nil))
  (switch-to-buffer buffer))
;;(mvec->table (tree->mvec '((1 2) (3 4))))

(defun mvec-size (mvec)
  (if (vectorp mvec)
    (sum (map 'list #'mvec-size mvec)) 1))
;;(mvec-size [[1 2 3] [1 2 3]])

(defun eread (string)
  "Reads a common lisp expression. It handles CL print syntax in the following ways:
Arrays are converted to vectors (or vectors of vectors):
#(1 2 3)              --> [1 2 3]
#2A((1 2) (3 4))      --> ((1 2) (1 2)) ;i.e. a tree
Other unprintable paths are converted to string: 
#P/home/MBe/.emacs.d/ --> \"/home/MBe/.emacs.d\"
#<PACKAGE EGINA>      --> \"#<PACKAGE EGINA>\""
  (let ((s (string-trim string)))
    (when (stringp string)
      (acond
	;;vector
	((string-match* "`#(" s)
	 (coerce (read (substring string 1)) 'vector))
	;;array
	((string-match* "#.A(" string)
	 (tree->mvec (eread (substring string (1- (length it))))))
	((string-match* "#.A(\\(.*\\))" string :num 1)
	 (tree->mvec (read (format "(%s)" it))))
	;;complex
	((string-match* "#C(" s)
	 (cons 'complex (eread (substring s (1- (length it))))))
	;;object
	((string-match* "#<\\(.*\\)>" string :num 1)
	 (format "(%s)" it))
	;;path
	((string-match* "#P\"\\(.*\\)\"" string :num 1)
	 (format "(%s)" it))
	((string-match* "\\([[:digit:].]+\\)L0" string :num 1)
	 (read it))
	(t (unless (empty-string-p string)
	     (condition-case nil
		 (read string)
	       (error nil))))))))

(defun read-cl-expression (beg end)
  "Assumes we are in buffer where the cl expression exactly
contained in the region BEG END"
  (save-excursion
    (goto-char beg)
    (if (looking-at "(")
      ;;handle subexps: find beg end of each subexp and call this recursively
      ;;handle atom
      )))

(defun eread (string)
  "Reads a common lisp expression. It handles CL print syntax in the following ways:
Arrays are converted to vectors (or vectors of vectors):
#(1 2 3)              --> [1 2 3]
#2A((1 2) (3 4))      --> ((1 2) (1 2)) ;i.e. a tree
Other unprintable paths are converted to string: 
#P/home/MBe/.emacs.d/ --> \"/home/MBe/.emacs.d\"
#<PACKAGE EGINA>      --> \"#<PACKAGE EGINA>\"
TODO: This function handles atomic expression ok, but when they are elements in a list, it fails.
For instance #P/home/MBe/.emacs.d/ is ok, but (#P/home/MBe/.emacs.d/) is not. This must be fixed at some point.
Edit: I have scetched a reader for this, see above."
  (when (stringp string)
    (setf string (replace-regexp-in-string "#(" "(vector " string))
    (acond
      ;;vector
      ((string-match* "^#(" string)
       (coerce (read (substring string 1)) 'vector))
      ;;array
      ((string-match* "#.A(" string)
       (tree->mvec (read (substring string (1- (length it))))))
      ((string-match* "#.A(\\(.*\\))" string :num 1)
       (tree->mvec (read (format "(%s)" it))))
      ;;object
      ((string-match* "#<\\(.*\\)>" string :num 1)
       (format "(%s)" it))
      ;;path
      ((string-match* "#P\"\\(.*\\)\"" string :num 1)
       (format "(%s)" it))
      ((string-match* "\\([[:digit:].]+\\)L0" string :num 1)
       (read it))
      (t (unless (empty-string-p string)
	   (condition-case nil
	       (read string)
	     (error nil)))))))
;;(eread qwe)
;;(mapcar #'eread '("asdf#<PACKAGE YAMAL-MODEL>" nil))

(defun mb-lisp-process-output ()
  "Returns the output from REPL as an Elisp object. Note that
multiple values are returned as a list, cfr cl-values"
  (let* ((string (concat* (reverse *repl-comint-output-list*))))
   (when (string-match "^[A-Z0-9-.]*\\[[0-9,]+\\]> " string)
;;     (message "[REDUCED OUPUT: %s]\n" string)
     (setf *repl-comint-output-list* nil)    
     (setf *repl-output* (mapcar #'eread (split-string (cl-substitute 10 13 string) " ;\n\n"))))))

(defun mb-lisp-scroll-to-bottom (string)
  (let ((current (current-buffer)))
    (dolist (w (get-buffer-window-list current nil t))
      (set-window-point w (point-max)))))

(defun mb-lisp-on-output (string)
  "Echoes the result part of string.
This function is called by comint when the process has finished a
command. The input STRING is the default output. It is untouched
as output, but it is used by this function to extract the result
of the lisp evaulation the started the command."
  ;; The following didn't work. Instead, try let binding around every call to comint from here.
  ;;(unless mb-lisp-mode-p string)
  ;;(message "[NEW OUPUT: %s]\n" string)
  (push string *repl-comint-output-list*)
  ;;(message "[OUPUT STACK: %s]\n" (concat* *repl-comint-output-list*))
  ;;(message "[OUPUT STACK DEPTH: %d]\n" (length *repl-comint-output-list*))
  (mb-lisp-process-output)

  (when (repl-error-p string)
    (setf *repl-error* t))

  (setf *repl-last-output-time* (current-time))

  (if *repl-discard-output*
    "" string))
;;(repl-current-package-name)
;;(push #'mb-lisp-on-output comint-preoutput-filter-functions)

(defconst +repl-error-regexp+ 
  (regexp-opt '("*** - " 
		"** - Continuable Error")))

(defvar *repl-window-list* nil
  "The list of windows at the time of invoking mb-repl-mode. If
  the lisp process window is not present in this window, it is
  deleted when exiting mb-repl-mode.")

(defun repl-error-p (string)
  (string-match +repl-error-regexp+ string))

(define-derived-mode mb-lisp-repl-mode fundamental-mode "MB-Lisp REPL mode"
  "Controls the CL REPL from current buffer
\\{+mb-lisp-repl-mode-map+}"
  (setf *repl-window-list* (window-list))
  (read-only-mode 1)
  (make-local-variable 'mb-lisp-repl-mode-p)
  (setf mb-lisp-repl-mode-p t)
  (use-local-map +mb-lisp-repl-mode-map+)
  (display-buffer inferior-lisp-buffer))

(defun repl-read-expression (from-minibuffer-p)
  (let ((expression (if from-minibuffer-p (read-from-minibuffer "Eval: ") (mb-previous-sexp)))
	(package-name (buffer-package-name)))
    (comint-simple-send (inferior-lisp-proc)
			(if package-name
			    (format "%s::%s\n" package-name expression)
			    (format "%s\n" expression)))))
;;(repl-read-expression-from-minibuffer)

(defmacro repl-define-simple-key-map (map command-string repl-command)
  "Defines a key map from CHAR to entering REPL-COMMAND in the REPL"
  `(define-key ,map ,command-string
     #'(lambda () 
	 (interactive)
	 (comint-simple-send (inferior-lisp-proc)
			     (format "%s\n" ,repl-command)))))
;;(macroexpand (repl-define-simple-key-map +mb-lisp-repl-mode-map+ "w" ":w"))

(defmacro repl-define-simple-key-maps (map char-sequence)
  "For each character C in CHAR-SEQUENCE, it defines a key map
from C to entering :C in the REPL."
  `(progn ,@(mapcar #'(lambda (x)
			`(repl-define-simple-key-map 
			  ,map (string ,x) (format ":%c" ,x)))
		    char-sequence)))
;;(macroexpand (repl-define-simple-key-maps +mb-lisp-repl-mode-map+ "w"))

(defmacro repl-define-number-key-maps (map number-sequence)
  "For each character C in CHAR-SEQUENCE, it defines a key map
from C to entering :C in the REPL."
  `(progn ,@(mapcar #'(lambda (x)
			`(repl-define-simple-key-map 
			  ,map (string ,x) (format ":R%c" ,x)))
		    number-sequence)))
;;(macroexpand (repl-define-number-key-maps +mb-lisp-repl-mode-map+ "123"))

(defun exit-mb-lisp-repl-mode (&optional send-abort-message-p)
  ;; (let ((new-windows (cl-set-difference (window-list) *repl-window-list*)))
  ;;   (loop for w in new-windows
  ;; 	  do (delete-window w)))
  (mb-lisp-mode)
  (read-only-mode -1)
  (when send-abort-message-p
    (comint-simple-send (inferior-lisp-proc) "abort\n")))

(defconst +mb-lisp-repl-mode-map+
  (let ((map (make-sparse-keymap)))
    (define-key map "Q" #'(lambda () (interactive) (exit-mb-lisp-repl-mode nil)))
    (define-key map "q" #'(lambda () (interactive) (exit-mb-lisp-repl-mode t)))
    (define-key map "E" #'(lambda () (interactive) (repl-read-expression t)))
    (define-key map "S" #'(lambda () (interactive) (repl-read-expression nil)))
    (repl-define-simple-key-map map "c" "continue")
    (repl-define-simple-key-maps map "wudtbe")
    (repl-define-number-key-maps map "123456789")
    map)
  "Key map for Readonly interaction mode with REPL")

(require 'lilypond-mode)
(cl-defun pdf-music (string &optional (prefix "yamal"))
  "Converts lilypond STRING to an PDF-file and open it.
The file name is a generated name and is put somewhere under
/tmp. PREFIX can be used to tag these temporary files"
  (interactive)
  (let* ((temp-file (make-temp-file prefix))
	 (temp-ly-file (format "%s.ly" temp-file)))
    (with-temp-file temp-ly-file
      (setf LilyPond-master-file temp-ly-file)
      (insert string)
      (write-file temp-ly-file)
      (setf compilation-finish-function
	    (lexical-let ((temp-pdf-file (format "%s.pdf" temp-file)))
	      #'(lambda (buffer status)
		  (when (string-match "finished" status)
		    (find-file temp-pdf-file)
		    (other-window 1)
		    (bury-buffer)
		    (other-window 1)
;;		    (delete-other-windows)
		    )
		  (setf compilation-finish-function nil))))
      (LilyPond-compile-file
       (format "lilypond %s" temp-ly-file) "*LilyPond-compile*"))))

(cl-defun pdf-music (string &optional (prefix "yamal"))
  "Converts lilypond STRING to an PDF-file and open it.
The file name is a generated name and is put somewhere under
/tmp. PREFIX can be used to tag these temporary files"
  (interactive)
  (let* ((temp-file (make-temp-file prefix))
	 (temp-ly-file (format "%s.ly" temp-file)))
    (with-temp-file temp-ly-file
      (let ((LilyPond-master-file temp-ly-file))
	(insert string)
	(write-file temp-ly-file)
	(setf compilation-finish-function
	      (lexical-let ((temp-pdf-file (format "%s.pdf" temp-file)))
		#'(lambda (buffer status)
		    (when (string-match "finished" status)
		      (find-file temp-pdf-file)
		      (other-window 1)
		      (bury-buffer)
		      (other-window 1)
		      ;;		    (delete-other-windows)
		      )
		    (setf compilation-finish-function nil))))
	(LilyPond-compile-file
	 (format "lilypond %s" temp-ly-file) "*LilyPond-compile*"))
      )))

(defun lisp-doc-region ()
  "Returns the region (BEG END) of the documentation string of
the defun at POINT. If the defun does not have a documentation
string, it returns nil. Notqe also that this will not work in the
documentation strings of defclass, defconstant etc."
  (save-excursion
    (beginning-of-defun-raw 1)
    (forward-char 1)
    ;; Protect the forward-sexp ops in case defun is malformed.
    (condition-case nil
	;; Belive me, if (forward-sexp 3) instead of the next two forms
	;; (sexp-at-point) is not guaranteed to return the doc string
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
  "Dette er en setning. Dette er nok en setning. Dette er ogsaa en setning."
  (if (member major-mode '(emacs-lisp-mode mb-lisp-mode lisp-mode))
    (destructuring-bind (beg end) (lisp-doc-region)
      (save-restriction
	(narrow-to-region (1+ beg) (1- end))
	(apply orig-fun args)))
    (apply orig-fun args)))
(advice-add 'forward-sentence :around #'restrict-to-lisp-doc)

(provide 'mb-lisp)
