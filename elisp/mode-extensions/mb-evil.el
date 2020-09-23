;;;; My evil adaptations
;;;; See also http://wikemacs.org/wiki/Evil

;;;; I want 'df' to be key chord for leaving insert state.
;;;; In normal mode I want 'vn' (or something) to be ido-switch-buffer.
;;;; In normal mode I want 'vs' (or something) to be smart-swap.
;;;; For some buffers, I want to switch to Norwegian keyboard in
;;;; insert state.
;;;; For some modes, I want normal Emacs behavior.
;;;; Also, I want colors to indicate state

;;;; All of these wishes have been fullfilled with the implementation
;;;; below. Also, I have managed to use evil-cleverparens for lisp

;;;; TODO: auto loading of this file
;;;; TODO: auto loading of evil-cleverparens for lisp modes
;;;; TODO: tweek keys. L <--> l and H <--> h in evil-cleverparens.
;;;; TODO: norsk i insert-mode og overwrite-mode

;;;; Also useful to look at key bindings in ~/.emacs.d/elpa/evil-20160525.1148/evil-maps.el
(require 'evil)
(require 'evil-cleverparens)
(require 'evil-cleverparens-text-objects)
(require 'evil-textobj-line)

(evil-mode)
(setf evil-move-beyond-eol t)
(setf evil-cross-lines t)
(evil-select-search-module 'evil-search-module 'evil-search)

(require 'evil-exchange)
(setf evil-exchange-key "go")
(setf evil-exchange-cancel-key "gO")
(evil-exchange-install)

(defun set-mb-lisp-locals ()
  "My local modifications of lisp modes."
  (evil-cleverparens-mode)
  (setf evil-symbol-word-search t))

(add-hook 'emacs-lisp-mode-hook #'set-mb-lisp-locals)
(add-hook 'lisp-mode-hook #'set-mb-lisp-locals)

(setf evil-cleverparens-swap-move-by-word-and-symbol t)
(setf parens-require-spaces t)

;;; Smartparens adjustments for elisp (should do the same for common list etc)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "`" "'" :when '(sp-in-string-p sp-in-comment-p))

;;; Finally, need to revert .emacs* buffers since they have not yet been
;;; hooked by the functionality loaded here
(loop for b in (list (get-buffer ".emacs") (get-file-buffer +emacs-local+))
      do (with-current-buffer b (revert-buffer nil t)))

;;; Keyboard shortcuts
(defun evil-key-chord-define (state keymap key def &rest bindings)
  "Bind KEY to DEF in evil STATE in keymap.
STATE can take the same values as in `evil-define-key'."
  (let ((aux-maps
         (cond ((listp state)
                (mapcar
                 (lambda (st)
                   (evil-get-auxiliary-keymap keymap st t))
                 state))
               (state
                (list (evil-get-auxiliary-keymap keymap state t)))
               (t
                (list keymap)))))
    (while key
      (dolist (map aux-maps)
	(key-chord-define map key def))
      (setq key (pop bindings)
            def (pop bindings)))
    ;; ensure the prompt string comes first
    (dolist (map aux-maps)
      (evil-set-keymap-prompt map (keymap-prompt map)))))

(define-key global-map "\M-x" 'execute-extended-command)
(define-key evil-normal-state-map "\M-x" 'execute-extended-command)

(define-key evil-normal-state-map " " 'scroll-up-command)
(define-key evil-normal-state-map [return] 'scroll-down-command)
(define-key evil-normal-state-map "ga" 'what-cursor-position)

;;(evil-define-key '(motion) global-map [tab] #'forward-button)
;;(evil-define-key '(normal) global-map [tab] #'self-insert-command)
;;(define-key global-map [tab] #'self-insert-command)

(defun evil-move-past-close ()
  "Is made for insert state."
  (interactive)
  (up-list 1)
  (case major-mode
    (emacs-lisp-mode
     (insert " "))))

(defun save-buffer-file ()
  "Same as `save-buffer', but only if buffer-file exists."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)))
;;; Save on exit insert state ("df")
(add-hook 'evil-insert-state-exit-hook 'save-buffer-file)
;;(pop evil-insert-state-exit-hook)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "dg" 'evil-move-past-close)
(key-chord-define evil-insert-state-map "df" 'evil-normal-state)
(key-chord-define evil-insert-state-map "a;" 'yank)
(key-chord-define evil-normal-state-map "j;" 'save-buffer)
(key-chord-define evil-normal-state-map "J:" 'save-some-buffers)
(evil-key-chord-define '(normal visual motion) global-map "uu" 'undo-tree-redo)
(evil-key-chord-define '(normal visual motion) global-map "vn" 'ido-switch-buffer)

(define-key vc-prefix-map "j" 'log-edit-done)
(key-chord-define evil-normal-state-map "vk" vc-prefix-map)

(define-key evil-normal-state-map  [?g ? ] 'just-one-space)
(define-key evil-normal-state-map  [?g return] 'delete-blank-lines)
(define-key evil-normal-state-map "gp" 'next-error)
(define-key evil-normal-state-map "gP" 'previous-error)
(define-key evil-normal-state-map "gI" 'mb-show-process-buffer)
(define-key evil-normal-state-map "gs" 'isearch-forward)
(define-key evil-normal-state-map "gS" 'isearch-backward)
(define-key evil-normal-state-map "gb" 'sp-down-sexp)

(defun mb-show-process-buffer ()
  (interactive)
  (case major-mode
    (mbscilab-mode (mbscilab-show-process-buffer))
    (octave-mode (octave-show-process-buffer))
    (emacs-lisp-mode (display-buffer "*scratch*" '(display-buffer-use-some-window)))
    (mb-lisp-mode (mblisp-show-process-buffer))))

(evil-define-motion evil-goto-line-keep-column (count)
  "Go to the first non-blank character of line COUNT.
By default the last line."
  :jump t
  :type line
  (let ((column (current-column)))
    (if (null count)
      (with-no-warnings (end-of-buffer))
      (forward-line (- count (line-number-at-pos))))
    (eol)
    (let ((c (current-column)))
      (loop while (> (current-column) column) do (backward-char)))))

(evil-key-chord-define '(normal visual motion) global-map "GG" 'evil-goto-line-keep-column)

(defun ffap-read-file-or-url-no-prompt (prompt guess)
  "Same as `ffap-read-file-or-url' but without prompting."
  (or guess (setq guess default-directory))
  (let (dir)
    (unless (ffap-url-p guess)
      (unless (ffap-file-remote-p guess)
	(setq guess
	      (abbreviate-file-name (expand-file-name guess))))
      (setq dir (file-name-directory guess)))
    (or (ffap-url-p guess)
	(substitute-in-file-name guess))))

(defun ffap-no-prompt (&optional filename)
  "Same as `ffap' but without prompting."
  (interactive)
  (advice-add #'ffap-read-file-or-url :override #'ffap-read-file-or-url-no-prompt)
  (ffap filename)
  (advice-remove #'ffap-read-file-or-url #'ffap-read-file-or-url-no-prompt))

(defun ffap-no-prompt-read-only (&optional filename)
  "Same as `ffap-no-prompt' with read only mode."
  (interactive)
  (ffap-no-prompt filename)
  (setf buffer-read-only t))

(defun ffap-previous ()
  (interactive)
  (ffap-next t))

(defun transpose-split-orientation ()
  "If there is a simple horizontal split, change to vertical, and vice versa.
A simple split consists of two windows only."
  (interactive)
  (when (= 2 (count-windows))
    (let ((func (if (window-full-height-p)
                  #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
	(other-window 1)
	(switch-to-buffer (other-buffer))))))

(defun rotate-windows ()
  "Rotate windows."
  (interactive)
  (let ((cb (current-buffer)))
    (loop with buffers = (loop for w in (window-list) collect (window-buffer w))
	  for b in (rotate-list buffers)
	  for w in (window-list)
	  do (set-window-buffer w b))
    (select-window (get-buffer-window cb))))

(defun swap-windows ()
  "Rotate windows."
  (interactive)
  (rotate-windows)
  (other-window -1))
;;(swap-windows)

(defun find-tag-no-prompt ()
  (interactive)
  (let* ((tagname (find-tag-default))
	 (buf (find-tag-noselect tagname))
	 (pos (with-current-buffer buf (point))))
    (condition-case nil
	(switch-to-buffer buf)
      (error (pop-to-buffer buf)))
    (goto-char pos)))
;;(find-tag-no-prompt)

(defun find-tag-no-prompt-read-only ()
  (interactive)
  (let ((n (length (buffer-list))))
    (find-tag-no-prompt)
    (when (> (length (buffer-list)) n)
      (setf buffer-read-only t))))

(let ((window-map (make-sparse-keymap)))
  (evil-key-chord-define '(normal motion) global-map "vw" window-map)
  (define-key window-map "h" #'split-window-right)
  (define-key window-map "v" #'split-window-below)
  (define-key window-map "t" #'transpose-split-orientation)
  (define-key window-map "s" #'swap-windows)
  (define-key window-map "r" #'rotate-windows)
  (define-key window-map "d" #'delete-window)
  (define-key window-map "o" #'other-window)
  (define-key window-map "O" #'(lambda () (interactive) (other-window -1))))

(let ((swap-map (make-sparse-keymap)))
  (evil-key-chord-define '(normal motion) global-map "vo" swap-map)
  (define-key swap-map "a" #'ffap-no-prompt)
  (define-key swap-map "A" #'ffap-no-prompt-read-only)
  (define-key swap-map "b" #'bury-buffer)
  (define-key swap-map "B" #'unbury-buffer)
  (define-key swap-map "d" #'(lambda () (interactive)
				     (kill-buffer (current-buffer))))
  (define-key swap-map "f" #'find-file)
  (define-key swap-map "g" #'mb-grep)
  (define-key swap-map "G" #'mb-grep-interactive)
  (define-key swap-map "F" #'find-file-read-only)
  (define-key swap-map "k" #'browse-kill-ring)
  (define-key swap-map "n" #'ffap-next)
  (define-key swap-map "N" #'ffap-previous)
  (define-key swap-map "r" #'revert-buffer)
  (define-key swap-map "s" #'smart-swap)
  (define-key swap-map "T" #'find-tag-no-prompt)
  (define-key swap-map "t" #'find-tag-no-prompt-read-only)
  (define-key swap-map "v" #'(lambda () (interactive)
				     (switch-to-buffer (other-buffer)))))

(let ((div-map (make-sparse-keymap)))
  ;; memo: by default we are incrementing decimeters! (dm)
  (evil-key-chord-define '(normal motion) global-map "dm" div-map)
  (define-key div-map "l" (lambda (n) (interactive "p") (inc-thing-at-point n 1)))
  (define-key div-map "h" (lambda (n) (interactive "p") (inc-thing-at-point (- n) 1)))
  (define-key div-map "j" (lambda (n) (interactive "p") (inc-thing-at-point n 2)))
  (define-key div-map "k" (lambda (n) (interactive "p") (inc-thing-at-point (- n) 2)))
  (define-key div-map "\M-j" (lambda (n) (interactive "p") (inc-thing-at-point n 3)))
  (define-key div-map "\M-k" (lambda (n)
			       (interactive "p") (inc-thing-at-point (- n) 3))))

(require 'mb-emacs-lisp)
(require 'LS)
(require 'mb-utils-buffer)

(let ((insert-map (make-sparse-keymap)))
  (key-chord-define evil-normal-state-map "vi" insert-map)
  (define-key insert-map "a" #'ls-insert-arrival-time)
  (define-key insert-map "A" #'ls-insert-depature-time)
  (define-key insert-map "b" #'insert-symbol-bullet)
  (define-key insert-map "c" #'comment-region*)
  (define-key insert-map "d" #'insert-date)
  (define-key insert-map "D" #'(lambda () (interactive) (insert-date-and-time 3)))
  (define-key insert-map "f" #'cl-ify-form)
  (define-key insert-map "F" #'cl-ify-defun)
  (define-key insert-map "q" #'fill-paragraph)
  (define-key insert-map "p" #'insert-provide)
  (define-key insert-map "r" #'reverse-sexps)
  (define-key insert-map "R" #'rotate-sexps)
  (define-key insert-map "t" #'insert-time)
  (define-key insert-map "T" #'(lambda () (interactive) (insert-time 3)))
  (define-key insert-map "u" #'uncomment-region*)
  (define-key insert-map "*" #'(lambda (n) (interactive "P") (mb-surround "*" (or n 1))))
  (define-key insert-map "+" #'(lambda (n) (interactive "P") (mb-surround "+" (or n 1))))
  (define-key insert-map "'" #'(lambda (n) (interactive "P") (mb-surround "'" (or n 1))))
  (define-key insert-map "\"" #'(lambda (n) (interactive "P") (mb-surround "\"" (or n 1))))
  (define-key insert-map "`" #'(lambda (n) (interactive "P") (mb-surround "`" (or n 1))))
  (define-key insert-map "g" #'(lambda (n) (interactive "P") (mb-surround "«" (or n 1))))
  (define-key insert-map "[" #'(lambda (n) (interactive "P") (mb-surround "[" (or n 1))))
  (define-key insert-map "{" #'(lambda (n) (interactive "P") (mb-surround "{" (or n 1))))
  (define-key insert-map "<" #'(lambda (n) (interactive "P") (mb-surround "<" (or n 1))))
  (define-key insert-map "\\" #'(lambda (n) (interactive "P") (mb-undo-surround (or n 1))))
  (define-key insert-map "@" #'texinfo-insert-@var))

(let ((big-insert-map (make-sparse-keymap)))
  (key-chord-define evil-normal-state-map "vI" big-insert-map)
  (define-key big-insert-map "\"" #'(lambda (n) (interactive "P") (mb-surround "\\\"" (or n 1)))))

;; TODO: move these two defuns elsewhere
(require 'mb-metafont)

(defun minor-mode-p (mode)
  "Check if symbol MODE is an active minor-mode in the current buffer."
  (condition-case nil
      (and (symbolp mode)
	   (symbol-value mode)
	   (find mode minor-mode-list))
    (error nil)))
;;(mapcar #'minor-mode-p '(slime-mode undo-tree-mode))

(defun slime-p ()
  "Return nil iff slime-mode is not active"
  (minor-mode-p 'slime-mode))
;;(slime-p)

;;; Eval machinery
(defun mb-eval-string (string &rest args)
  "This is the core function of the mb-eval machinery. Most other
eval function should end up here to secure a sort of conformity
between different"
  (case major-mode
    (mb-lisp-mode
     (if (slime-p)
       (slime-eval string)
       (apply #'mb-lisp-eval-1 string args)))
    (emacs-lisp-mode (eval (read string)))
    (octave-mode (octave-send-string string))
    (js-mode (apply #'js-eval-string string args))
    (mbscilab-mode (mbscilab-eval string))))
;;(mb-eval-string "(+ 2 2)")

(defun eval-form ()
  "This is special to Lisp languages only."
  (interactive)
  (case major-mode
    ((emacs-lisp-mode mb-lisp-mode)
     (save-excursion
       (backward-up-list 1)
       (forward-sexp 1)
       (mb-eval-last-sexp)))
    (octave-mode (octave-send-block))))

(defun mb-eval-last-sexp (&rest args)
  "Eval the syntaks expression preceding point."
  (interactive)
  (case major-mode
    (emacs-lisp-mode (eval-last-sexp args))
    (mb-lisp-mode (if (slime-p)
		    (if args
		      (slime-pprint-eval-last-expression)
		      (slime-eval-last-expression))
		    (mb-lisp-eval-sexp args)))
    (python-mode (apply #'python-shell-send-region
		   (mb-python-last-sexp-region)))
    (octave-mode (apply #'octave-eval-last-sexp args))
    (t (apply #'mb-eval-region (append (last-sexp-region) args)))))

(defun eval-current-sexp ()
  (interactive)
  (case major-mode
    ((emacs-lisp-mode mb-lisp-mode)
     (save-excursion
       (forward-sexp 1)
       (if (slime-p)
	 (slime-eval-last-expression)
	 (eval-last-sexp nil))))
    (octave-mode
     (save-excursion (eol) (mb-eval-last-sexp)))
    (otherwise
     (princ (mb-eval-string
	     (string-match* "\\(?:#\\|//+\\|[[:space:]]\\)*\\(.*\\)"
	       (line-string) :num 1))))))
;;(+ (+ 111 2) 3)

(defun eval-defun-test (&optional no-eval-p)
  (interactive)
  (unless no-eval-p (mb-eval-defun))
  (save-excursion
    (cl-case major-mode
      ((emacs-lisp-mode mb-lisp-mode python-mode)
       (evil-cp-end-of-defun)
       (eol :offset 1)
       (mb-eval-last-sexp))
      (metafont-mode (meta-eval-buffer))
      (otherwise
       (end-of-defun)
       (eol)
       (mb-eval-last-sexp)))))
;;(eval-defun-test)

(defun gp-eval-buffer ()
  (interactive)
  (gnuplot-run-buffer)
  (let ((filename (string-match* "set[\t ]+output[\t ]+['\"]\\([^'\"]*\\)"
		    (buffer-string-no-properties) :num 1)))
    (find-file-other-window filename)))

(defun strip-ssh (filename)
  "Strip the ssh prefix of filename"
  (string-match* "/ssh:[^:]*:\\(.*\\)" filename :num 1))
;;(strip-ssh "/ssh:pf:/home/mats_progfab_no/git/problem-server/")

(defun mb-eval-buffer (&optional args)
  (interactive)
  (cl-case major-mode
    (emacs-lisp-mode (apply #'eval-buffer args))
    (gnuplot-mode (gnuplot-send-buffer-to-gnuplot))
    (mb-lisp-mode (if (slime-p)
		    (slime-compile-and-load-file)
		    (lisp-load-file (strip-ssh (buffer-file-name)))))
    (metafont-mode (meta-compile-file (buffer-file-name)))
    (octave-mode (octave-source-buffer))
    (mbscilab-mode (mbscilab-eval-buffer))
    (python-mode (python-shell-send-buffer nil))
    ((c++-mode cc-mode makefile-mode) (compile "make -k"))
    (latex-mode (TeX-command-run-all nil))
    (LilyPond-mode (mb-lilypond-compile))
    (otherwise (mb-eval-region (point-min) (point-max)))))

(cl-defun mb-eval-defun ()
  (interactive)
  (cl-case major-mode
    (emacs-lisp-mode (eval-defun current-prefix-arg))
    (mb-lisp-mode (when (slime-p)
		    (slime-eval-defun)))
    (python-mode (apply #'python-shell-send-region (mb-python-defun-region)))
    (otherwise (mb-eval-region (bod*) (eod*) t))))

(defun mb-eval-region (start end &optional printflag read-function)
  (interactive "r")
  (case major-mode
    (emacs-lisp-mode (eval-region start end printflag read-function))
    (python-mode (python-shell-send-region start end nil))
    ((mbscilab-mode scilab-mode) (mbscilab-eval-region start end))
    (sh-mode (sh-execute-region start end))
    (latex-mode (TeX-command-run-all-region))
;;    (octave-mode (octave-send-region start end))
    (otherwise
     (princ (mb-eval-string
	     (buffer-substring-no-properties start end)
	     printflag)))))

(defun mb-eval-region-from-point (&optional printflag read-function)
  "Evalutates content from POINT to the end of the buffer."
  (interactive "r")
  (mb-eval-region (bol*) (point-max) printflag read-function))

(defun mb-eval-region-to-point (&optional printflag read-function)
  "Evalutates content from the end of the buffer to POINT."
  (interactive "r")
  (mb-eval-region (point-min) (eol*) printflag read-function))

(defun mb-compile-buffer ()
  (interactive)
  (case major-mode
    (octave-mode (octave-source-buffer))))

(defvar *eval-map* (make-sparse-keymap))
(key-chord-define evil-normal-state-map "kj" *eval-map*)
 (define-key *eval-map* "k" #'mb-compile-buffer)
 (define-key *eval-map* "d" #'mb-eval-defun)
 (define-key *eval-map* "b" #'mb-eval-buffer)
 (define-key *eval-map* "s" #'mb-eval-region-from-point)
 (define-key *eval-map* "S" #'mb-eval-region-to-point)
 (define-key *eval-map* "r" #'mb-eval-region)
 (define-key *eval-map* "l" #'mb-eval-last-sexp)
 (define-key *eval-map* "f" #'eval-form)
 (define-key *eval-map* "c" #'eval-current-sexp)
 (define-key *eval-map* "e" #'eval-expression)
 (define-key *eval-map* "t" #'eval-defun-test)
 (define-key *eval-map* "T" #'(lambda () (interactive) (eval-defun-test t)))


(defun mb-normal-state-init ()
  (key-chord-mode 1))
;;(add-hook 'evil-normal-state-entry-hook 'mb-normal-state-init)

(defun quailify-key-chord-input-method (result)
  "Modify the RESULT of `key-chord-input-method'.
If RESULT is a key-chord, i.e. it is a list on the form
\(KEYCHORD CHAR1 CHAR2 ...\), RESULT is returned unmodified. If
not, RESULT should be a list of one character (an error is
signaled otherwise). In this case, if `current-input-method' is
\"norwegian-keyboard\", this character is passed to
`quail-input-method' which result is returned. If
`current-input-method' is nil \(or another language the the above
mentioned\) RESULT is returned unmodified."
  (if (or (not current-input-method)
	  (not (cl-member current-input-method
			  '("norwegian-keyboard" "croatian")
			  :test #'string=))
	  (and (listp result)
	       (> (length result) 2)
	       (eql (first result) 'key-chord)))
    result
    ;; else translate RESULT with quail:
    (if (and (listp result)
	     (= (length result) 1))
      (quail-input-method (first result))
      (error "Unexpected output from KEY-CHORD-INPUT-METHOD: %S" result))))

(defun mb-key-chord-advice (x)
  (setq input-method-function 'key-chord-input-method))
;; activate-input-method overrides input-method-function, so therfore:
(advice-add #'activate-input-method :after #'mb-key-chord-advice)

(defun mb-insert-state-init ()
  ;;(key-chord-mode 1)
  (when (member (buffer-name) '("arbeidslog" "log.org"))
    (activate-input-method 'norwegian-keyboard))
  (advice-add #'key-chord-input-method
	      :filter-return #'quailify-key-chord-input-method))
(add-hook 'evil-insert-state-entry-hook #'mb-insert-state-init)

(defun mb-insert-state-cleanup ()
  (advice-remove #'key-chord-input-method #'quailify-key-chord-input-method))
(add-hook 'evil-insert-state-exit-hook #'mb-insert-state-cleanup)

(lexical-let ((default-color (cons (face-background 'mode-line)
				   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
	    (lambda ()
	      (let ((color (cond ((minibufferp) default-color)
				 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
				 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
				 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
				 (t default-color))))
		(set-face-background 'mode-line (car color))
		(set-face-foreground 'mode-line (cdr color))))))

;; move to mode ext
(loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                              (inferior-lisp-mode . emacs)
                              (inferior-octave-mode . emacs)
                              (shell-mode . emacs)
                              (git-commit-mode . emacs)
                              (git-rebase-mode . emacs)
                              (term-mode . emacs)
                              (help-mode . emacs)
                              (helm-grep-mode . emacs)
                              (grep-mode . emacs)
                              (bc-menu-mode . emacs)
                              (magit-branch-manager-mode . emacs)
                              (rdictcc-buffer-mode . emacs)
                              (dired-mode . emacs)
                              (wdired-mode . normal))
      do (evil-set-initial-state mode state))


(defun alf/key-chord-undefine (keys)
  "Undefine the key chord identified by KEYS.
This should be done by key-chord-unset-global, however that
does not work for me."
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (if (eq key1 key2)
        (global-unset-key (vector 'key-chord key1 key2))
      ;; else
      (global-unset-key (vector 'key-chord key1 key2))
      (global-unset-key (vector 'key-chord key2 key1)))))
;;(alf/key-chord-undefine "df")

(evil-define-text-object mb-evil-buffer (count &optional beg end type)
  "Smartparens sexp object."
  (evil-range (point-min) (point-max) 'inclusive :expanded t))

(evil-define-text-object mb-evil-inner-defun (count &optional beg end type)
  "Object for inner defun of c++ like languages"
  (evil-range (bol* :point (bod*) :offset 1)
	      (eol* :point (eod*) :offset -2)
	      'inclusive :expanded t))

(evil-define-text-object mb-evil-inner-variable-name (count &optional beg end type)
  "Smartparens sexp object. E.g. +foobar+ --> foobar"
  (destructuring-bind (beg end) (last-sexp-region)
    (evil-range (1+ beg) (1- end) 'inclusive :expanded t)))

(define-key evil-outer-text-objects-map "g" #'mb-evil-buffer)
(define-key evil-inner-text-objects-map "v" #'mb-evil-inner-variable-name)
(define-key evil-inner-text-objects-map "D" #'mb-evil-inner-defun)

(evil-define-operator evil-yank-line (beg end type register yank-handler)
  "Yank to end of line.
MB: I have overrided the original evil-yank-line to comform to
evil-delete-line. I.e. Y yanks the same region as C changes (and
D deletes). 

This function is simply a clone of `evil-delete-line', with every
occurence of 'delete' replaced with 'yank'."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  ;; act linewise in Visual state
  (let* ((beg (or beg (point)))
         (end (or end beg)))
    (when (evil-visual-state-p)
      (unless (memq type '(line block))
        (let ((range (evil-expand beg end 'line)))
          (setq beg (evil-range-beginning range)
                end (evil-range-end range)
                type (evil-type range))))
      (evil-exit-visual-state))
    (cond
     ((eq type 'block)
      ;; equivalent to $d, i.e., we use the block-to-eol selection and
      ;; call `evil-yank'. In this case we fake the call to
      ;; `evil-end-of-line' by setting `temporary-goal-column' and
      ;; `last-command' appropriately as `evil-end-of-line' would do.
      (let ((temporary-goal-column most-positive-fixnum)
            (last-command 'next-line))
        (evil-yank beg end 'block register yank-handler)))
     ((eq type 'line)
      (evil-yank beg end type register yank-handler))
     (t
      (evil-yank beg (line-end-position) type register yank-handler)))))

(evil-define-motion evil-goto-line-preserve-column (count)
  "Go to the first non-blank character of line COUNT.
By default the last line."
  :jump t
  :type line
  (let ((c (current-column)))
    (if (null count)
      (with-no-warnings
	(end-of-buffer)
	(evil-previous-line))
      (goto-char (point-min))
      (forward-line (1- count)))
    (evil-goto-column c)))
(define-key evil-normal-state-map "gG" 'evil-goto-line-preserve-column)

(defun print-last-pdf-in-Messages ()
  (interactive)
  (with-buffer "*Messages*"
    (save-excursion
      (eob)
      (re-search-backward "\\.pdf\"")
      (ffap-no-prompt-read-only))))
;;(print-last-pdf-in-Messages)

;;;; Insert quotes (put this somewhere else when finished)
(cl-defun mb-surround-region-1 (region left right n)
  (insert-at left (first region) n)
  (insert-at right (+ (second region) (length left)) n))

(cl-defun mb-surround-lookup-right (left)
  (string-case left
    ("(" ")")
    ("`" "'")
    ("«" "»")
    ("<" ">")
    ("[" "]")
    ("{" "}")
    (otherwise left)))
;;(mapcar #'mb-surround-lookup-right (list "`" "'" "(" "<"))

(cl-defun mb-surround-region (region left n)
  (mb-surround-region-1 region left (mb-surround-lookup-right left) n))

(defun mb-surround-word (left n)
  (mb-surround-region (cons (bow*) (eow* n)) left 1))

(defun mb-surround-symbol (left n)
  (message "%S" (list (point) (bos*) (eos*)))
  (mb-surround-region (list (bos*) (eos* n)) left 1))

(defun mb-surround (left n)
  (if (use-region-p)
    (mb-surround-region (region) left n)
    ;; (mb-surround-word left n)
    (mb-surround-symbol left n)))

;; Undo surround: strictly assume surround is 1 char wide on both
;; sides
(defun mb-undo-surround (n)
  (if (use-region-p)
    (error "Undo surround region not implemented!")
    (save-excursion
      (bow)
      (backward-delete-char n)
      (eow)
      (delete-char n))))

(defun evil-increment-rectangle ()
  (interactive)
  (let ((p (point))
	(m (mark)))
    (let ((b (min p m))
	  (e (1- (max p m))))
      (message "%d %d" b e)
      (evil-apply-on-block
       (lexical-let (last)
	 #'(lambda (beg end)
	     (if last
	       (let ((s (number-to-string (incf last))))
		 (overwrite-region s beg end)
		 (qwe beg end))
	       (setf last (string-to-number (region-string beg (1+ end)))))))
       b e nil))))
;;(evil-increment-rectangle)

(provide 'mb-evil)
