;;;; Light weight scilab mode
;;;; Main points are
;;;; 1. Easy command evaluations
;;;; 2. Degugging
(require 'scilab)
(require 'mbscilab-debug)
(require 'mbscilab-syntax)
(require 'mbscilab-font-lock)

;;(defconst +with-scilab-process-p+ t)
(defconst +with-scilab-process-p+ t)
(defvar *scilab-partial-answer* "")
(defvar *scilab-answer* nil)
(defvar *scilab-display-answer* nil)

;;; Error signals
(define-error :mbscilab-error "General error in mbscilab mode" 'error)
(define-error :mbscilab-no-process "General error in mbscilab mode" 'error)

;;; Comint methods
(cl-defun scilab-command ()
  (if (eql (emacs-os) :linux)
    "scilab-adv-cli"
    "/cygdrive/c/Program Files (x86)/scilab-5.5.2/bin/Scilex.exe"))

(cl-defun scilab-command-line-string ()
  (if +scilab-init-file+
    (format "%s -f %s" (scilab-command) +scilab-init-file+)
    (scilab-command)))
;;(scilab-command-line-string)

(defconst +scilab-init-files+
  (list
   (list :viewer "~/cvs/sources/SciLab/toolboxes/LSSensorViewer/macros/LSSensorViewer.ini")
   (list :no-init nil)))

(defconst +scilab-init-file+
  (awhen (second (assoc :no-init +scilab-init-files+))
    (if (eql (emacs-os) :linux)
      path
      (cygpath path))))
;;(cygpath "~/sources/SciLab/toolboxes/LSSensorViewer/macros/LSSensorViewer.ini")

(cl-defun inferior-mbscilab-buffer ()
  "TODO scilab here and scilab there. Make this a variable with a suitable name!"
  (get-buffer "*scilab*"))

(cl-defun mbscilab-show-process-buffer ()
  "Make sure that `inferior-inferior-mbscilab-buffer' is displayed."
  (interactive)
  (aif (get-buffer (inferior-mbscilab-buffer))
    (display-buffer it '(display-buffer-use-some-window))
    (message "No buffer named %s" (inferior-mbscilab-buffer))))

(cl-defun scilab-process ()
  (get-buffer-process (inferior-mbscilab-buffer)))

(cl-defun mbscilab-comint-clean-prompt (&optional string)
  "TODO: write scilab-last-output.
Strip trailing `^M' characters from the current output group.
This function could be on `comint-output-filter-functions' or bound to a key."
  (interactive)
  (let ((pmark (process-mark (get-buffer-process (current-buffer))))
	(res (and string 
		  (mb-string-replace string "?\[[[:digit:]]+m" ""))))
    (save-excursion
      (condition-case nil
	  (goto-char comint-last-input-end) ;another nice var is comint-last-output-start
	(error nil))
      (while (re-search-forward "\^\[\[[[:digit:]]+m" pmark t)
	(replace-match "" t t))
      res)))
;;(add-hook 'comint-output-filter-functions 'mbscilab-comint-clean-prompt)

(defconst +scilab-prompt+ "-\\([[:digit:]]*\\)->")
(defconst +scilab-prompt-regexp+ (format "^%s" +scilab-prompt+))

(cl-defun mbscilab-comint-add-prompt (string)
  (concat string +scilab-prompt+))

(cl-defun mbscilab-comint-prompt-substringp (string)
  (and string (string-match +scilab-prompt-regexp+ string)))
;;(mbscilab-comint-prompt-substringp +scilab-prompt+)

(cl-defun mbscilab-comint-show-result (&optional string)
  "Prints a `message' with the result of the last scilab evaluation.
Also, strips STRING from any ^M's
TODO: better name?"
;;  (message "mbscilab-comint-show-result received string %s" string)
  (let ((res (remove 13 string)))
    (setf *scilab-partial-answer* (concat *scilab-partial-answer* res))
    (when (string-match* "TEPPEABO-END\\([[:space:]]*\n\\)*\\'" *scilab-partial-answer*)
      (setf *scilab-answer*
	    (last-elt (substring-intv *scilab-partial-answer* (interval-oo "TEPPEABO-START" "TEPPEABO-END") :all)))
      (setf *scilab-partial-answer* "")
      (when *scilab-display-answer* (message "%s" (string-trim *scilab-answer*))))))
;;(mbscilab-eval-raw "1+1" t)

(define-derived-mode scilab-comint-mode comint-mode "Inferior SciLab"
  "An extension of comint mode
\\{scilab-comint-mode-map\\}")

(cl-defun scilab-comint-mode-set-filter-functions ()
  (setq-local comint-output-filter-functions
	      '(scilab-output-garbage-filter
		mbscilab-comint-clean-prompt
		shell-strip-ctrl-m
		comint-watch-for-password-prompt
		comint-postoutput-scroll-to-bottom
		scilab-debug-status
		mbscilab-comint-show-result)))

(cl-defun scilab-comint-mode-set-locals ()
  (scilab-comint-mode-set-filter-functions)
  (setq-local comint-move-point-for-output t)
  (setq-local comint-prompt-regexp "-[1-100]?-> *")
  (setq-local comint-delimiter-argument-list (list [ 59 ])))
(add-hook (derived-mode-hook-name 'scilab-comint-mode) #'scilab-comint-mode-set-locals)

;;; Eval methods
(cl-defun mbscilab-eval-raw (expression &optional wait-for-result-p)
  (message "About to send scilab expression: %s" expression)
  (comint-simple-send
   (scilab-process)
   (format "disp('TEPPEABO-START');%s\ndisp('TEPPEABO-END');" expression))
  (when wait-for-result-p
    (let ((*scilab-answer* nil))
      (cl-loop with ms-interval = 10
	    for ms below 1000 by ms-interval
	    if *scilab-answer* return (let ((res (copy-sequence *scilab-answer*)))
					(setf *scilab-answer* nil)
					(message "%S" (string-trim res))
					res)
	    do (sleep-for 0 10)))))
;;(let ((*scilab-display-answer* t)) (mbscilab-eval-raw "[r c] = where()" t))

(cl-defun mbscilab-eval (expression &optional wait-for-result-p)
  (scilab-refresh-all-breakpoints)
  (let ((*scilab-display-answer* (not wait-for-result-p)))
    (mbscilab-eval-raw expression wait-for-result-p)))
;;(mbscilab-eval "2+2" t)

(cl-defun my-cygpath (path)
  (if (eql (emacs-os) :linux)
    path
    (format "C:\\cygwin64%s" (replace-regexp-in-string "/" "\\\\" path))))

(cl-defun mbscilab-exec (path &optional (mode -1))
  (let ((expression (format "exec('%s', %d)" (my-cygpath path) mode)))
    (mbscilab-eval expression)))
;;(mbscilab-exec "qwe")


(defvar *scilab-eval-history* nil)
;;(nilf *scilab-eval-history*)

(cl-defun eval-scilab-expression ()
  "Interactive evaluation of SciLab expression."
  (interactive)
  (let* ((default (first *scilab-eval-history*))
	 (exp (read-string (format "Eval SciLab expression: (%s): " default)
		nil '*scilab-eval-history* default)))
    (mbscilab-eval exp t)))
;;(eval-scilab-expression)
;;(read-string "test: ")

(cl-defun scilab-expression-at-point (&optional (point (point)))
  "Returns the scilab expression before POINT. 
The rules for expression extraction depends on point context. Examples:
'expression;|'               ==> expression;      (side effect only, scilab returns nothing)
'\\ expression;|'            ==> expression;      (ditto)
'\\ x = expression;|'        ==> x = expression;  (ditto)
'if (remainder == 0)| then'  ==> (remainder == 0) (returns %T or %F)
'if (remainder| == 0) then'  ==> remainder        (returns the value of remainder)
'if (remainder == 0) then|'  ==> then ;           (illegal)
'foo(bar)|'                  ==> foo(bar)         (returns the result of calling fn foo with argument bar)
'foo(bar|)'                  ==> bar              (returns the value of bar)"
  (if (and (looking-at "[[:space:]/]*$") (looking-back ";"))
    ;; case 1 (trivial): point is at the end of a semicolon terminated statement
    ;; ==> return the statement
    (string-trim-left* (line-string) "[[:space:]/]*")
    ;; else return the scilab "sexp" before point. This is more tricky:
    (cl-flet ((bs (&optional allow-linebreak-p)
		(let ((bol (save-excursion (bol))))
		  (backward-sexp 1)
		  (if allow-linebreak-p (point) (max (point) bol)))))
      (save-excursion
	(let ((s2 (string-trim (buffer-substring-no-properties (bs) point))))
	  (if (or (looking-back "[([][[:space:]]*")       ; (!expr)
		  (not (looking-at "[[:space:]]*[([]")))  ; not !(expr)
	    s2
	    (let ((s1 (string-trim (buffer-substring-no-properties (point) (bs)))))
	      (if (member s1 '("if" "elseif" "for"))         ; if|elseif|for !(expr)
		s2 (concat s1 s2)))))))))

(cl-defun mbscilab-eval-sexp (prefix)
  (interactive "P")
  (let ((expression (scilab-expression-at-point)))
    (message "Evaluating SciLab expression '%s' ..." expression)
    (if current-prefix-arg
      (scilab-step-into (scilab-function-at-line expression))
      (mbscilab-eval expression))))

(cl-defun mbscilab-eval-defun ()
  (interactive)
  (let ((env (scilab-fn-info)))
    (mbscilab-eval (apply #'buffer-substring-no-properties (getf env :region)))))

(cl-defun mbscilab-eval-region (beg end)
  (interactive "r")
  (mbscilab-eval (buffer-substring-no-properties beg end)))

(cl-defun mbscilab-eval-buffer ()
  (interactive "")
  (aif (buffer-file-name (current-buffer))
    (mbscilab-exec it)
    (mbscilab-eval-region (point-min) (point-max))))

(defconst +scilab-tags-path+ "~/.SCILABTAGS")

(cl-defun scilab-tags-buffer ()
  (or (get-buffer (file-name-nondirectory +scilab-tags-path+))
      (find-file-noselect +scilab-tags-path+)))
;;(scilab-tags-buffer)

(cl-defun mbscilab-delete-autosaved-files ()
  "Locates and deletes any auto saved files for the current buffer.
NOTE: This could method could be obsolete since line markings in
debug mode no longer triggers the buffer's modified mark."
  (let ((auto-save (format "%s.#%s"
		     (file-name-directory (buffer-file-name))
		     (file-name-nondirectory (buffer-file-name)))))
    
    (when (file-attributes auto-save)
      (delete-file auto-save))))

(define-derived-mode mbscilab-mode c-mode "Scilab"
  "An extension of c mode
\\{mbscilab-mode-map}"
  (setq-local mbscilab-mode-p t)
  (setq-local beginning-of-defun-function #'mbscilab-beginning-of-defun)
  (setq-local end-of-defun-function #'scilab-end-of-defun)

  (setq-local tags-file-name "~/.SCILABTAGS")
  (when (file-exists-p +scilab-tags-path+)
    (visit-tags-table +scilab-tags-path+ t))

  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local c-electric-flag nil) ;;todo refine c-mode instead
  (setq indent-line-function 'scilab-indent-line)
  (setq indent-region-function nil) ;; inhibit c-indent-region
  (setq font-lock-defaults
	'((scilab-font-lock-keywords scilab-really-gaudy-font-lock-keywords)
	  t               ; do not do string/comment highlighting
	  nil             ; keywords are case sensitive.
	  ((?_ . "w")) 	  ; puts _ as a word constituent, simplifying our keywords significantly
	  ))
  (scilab-show-all-breakpoints)
  (display-line-numbers-mode)

  ;; finally launch scilab
  (when +with-scilab-process-p+
    (save-buffer)
    (mbscilab-delete-autosaved-files)
    (accept-process-output (scilab-process) 1)
  (if +scilab-init-file+
    (make-comint "scilab" (scilab-command) nil "-f" +scilab-init-file+)
    (make-comint "scilab" (scilab-command)))
    (with-buffer (inferior-mbscilab-buffer)
      (scilab-comint-mode))))
;;(make-comint "scilab" (scilab-command-line-string))
;;(make-comint "clisp" "clisp")

(defmacro push-back-local (newelt place &optional force-top)
  `(if (memql ,newelt ,place)
     (when ,force-top
       (progn (setf ,place (delete ,newelt ,place))
	      (setq-local ,place (nconc ,place (list ,newelt)))))
     (setq-local ,place (nconc ,place (list ,newelt)))))

(defmacro push-local (newelt place &optional force-top)
  `(unless (memql ,newelt ,place)
     (setq-local ,place (cons ,newelt ,place))))


(cl-defun scilab-switch-to-repl ()
  (interactive))

(cl-defun scilab-whereami ()
  (interactive)
  (scilab-debug-stack))

(cl-defun scilab-insert-parentheses ()
  (interactive)
  (insert-parentheses* 0 :ensure-space :none))

(cl-defun scilab-beginning-of-defun-p ()
  (let ((point (point)))
    (save-excursion
      (scilab-beginning-of-defun)
      (= point (point)))))

(cl-defun mbscilab-beginning-of-defun ()
  "Move backward to the beginning of a defun.
Unfortunately, this had to be written, since
scilab-beginning-of-defun do not move point if point is already
at the beginning of a defun."
  (interactive)
  (if (scilab-beginning-of-defun-p)
    (scilab-beginning-of-prev-defun)
    (scilab-beginning-of-defun)))

(cl-defun scilab-mark-function ()
  (interactive)
  (scilab-end-of-defun)
  (forward-line 1)
  (set-mark-command nil)
  (mbscilab-beginning-of-defun))

(define-key mbscilab-mode-map "\C-x\C-e" 'mbscilab-eval-sexp)
(define-key mbscilab-mode-map "\C-ce" 'mbscilab-eval-buffer)
(define-key mbscilab-mode-map "\C-cr" 'mbscilab-eval-region)
(define-key mbscilab-mode-map "\C-\M-x" 'mbscilab-eval-defun)
(define-key mbscilab-mode-map "\C-i" 'scilab-indent-line)
(define-key mbscilab-mode-map "\C-\M-a" 'mbscilab-beginning-of-defun)
(define-key mbscilab-mode-map "\C-\M-e" 'scilab-end-of-defun)
(define-key mbscilab-mode-map "\C-\M-h" 'scilab-mark-function)
(define-key mbscilab-mode-map "\M-(" 'scilab-insert-parentheses)

(define-key mbscilab-mode-map [(f5)] #'scilab-resume)
(define-key mbscilab-mode-map [(shift f5)] #'scilab-resume-to-cursor)
(define-key mbscilab-mode-map [(ctrl f5)] #'scilab-abort)
(define-key mbscilab-mode-map [(meta f5)] #'eval-scilab-expression)
(define-key mbscilab-mode-map [(f9)] #'scilab-toggle-breakpoint)
(define-key mbscilab-mode-map [(ctrl f9)] #'scilab-delete-all-breakpoints-in-buffer)
(define-key mbscilab-mode-map [(ctrl shift f9)] #'scilab-delete-all-breakpoints)
(define-key mbscilab-mode-map [(f10)] #'scilab-step-over)
(define-key mbscilab-mode-map [(ctrl f10)] #'scilab-step-into)
(define-key mbscilab-mode-map [(shift f10)] #'scilab-step-outof)
(define-key mbscilab-mode-map [(meta f10)] #'scilab-resume-to-cursor)
(define-key mbscilab-mode-map [(f12)] #'scilab-whereami)
;;(define-key mbscilab-mode-map [(f11)] #'(lambda () (interactive) (switch-to-buffer (other-buffer))))

(provide 'mbscilab)
