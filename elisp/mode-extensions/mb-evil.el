;; -*- lexical-binding: t; -*-
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

;;;; Also useful to look at key bindings in
;;;; ~/.emacs.d/elpa/evil-20160525.1148/evil-maps.el
(require 'evil)
(require 'evil-cleverparens)
(require 'evil-cleverparens-text-objects)
(require 'evil-textobj-line)

(evil-mode)
(setf evil-move-beyond-eol t)
(setf evil-cross-lines t)
(evil-select-search-module 'evil-search-module 'evil-search)

(require 'evil-exchange)
(evil-exchange-install)

(cl-defun set-mb-lisp-locals ()
  "My local modifications of lisp modes."
  (evil-cleverparens-mode)
  (setf evil-symbol-word-search t)
  (display-fill-column-indicator-mode))

(add-hook 'emacs-lisp-mode-hook #'set-mb-lisp-locals)
(add-hook 'lisp-mode-hook #'set-mb-lisp-locals)

(setf evil-cleverparens-swap-move-by-word-and-symbol t)
(setf parens-require-spaces t)

;;; Smartparens adjustments for elisp (should do the same for common list etc)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "`" "'" :when '(sp-in-string-p sp-in-comment-p))

;;; Finally, need to revert .emacs* buffers since they have not yet been
;;; hooked by the functionality loaded here
(cl-loop for b in (list (get-buffer ".emacs") (get-file-buffer +emacs-local+))
      do (with-current-buffer b (revert-buffer nil t)))

;;; Keyboard shortcuts
(cl-defun evil-key-chord-define (state keymap key def &rest bindings)
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

(cl-defun evil-move-past-close ()
  "Is made for insert state."
  (interactive)
  (up-list 1)
  (cl-case major-mode
    (emacs-lisp-mode
     (insert " "))))

(cl-defun save-buffer-file ()
  "Same as `save-buffer', but only if buffer-file exists."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)))
;;; Save on exit insert state ("df")
(add-hook 'evil-insert-state-exit-hook 'save-buffer-file)
;;(pop evil-insert-state-exit-hook)

(cl-defun mb-show-process-buffer ()
  (interactive)
  (cl-case major-mode
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
      (cl-loop while (> (current-column) column) do (backward-char)))))

(cl-defun ffap-read-file-or-url-no-prompt (prompt guess)
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

(cl-defun ffap-no-prompt (&optional filename)
  "Same as `ffap' but without prompting."
  (interactive)
  (advice-add #'ffap-read-file-or-url :override #'ffap-read-file-or-url-no-prompt)
  (ffap filename)
  (advice-remove #'ffap-read-file-or-url #'ffap-read-file-or-url-no-prompt))

(cl-defun ffap-no-prompt-read-only (&optional filename)
  "Same as `ffap-no-prompt' with read only mode."
  (interactive)
  (ffap-no-prompt filename)
  (setf buffer-read-only t))

(cl-defun ffap-previous ()
  (interactive)
  (ffap-next t))

(cl-defun transpose-split-orientation ()
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

(cl-defun rotate-windows ()
  "Rotate windows."
  (interactive)
  (let ((cb (current-buffer)))
    (cl-loop with buffers = (cl-loop for w in (window-list) collect (window-buffer w))
	  for b in (rotate-list buffers)
	  for w in (window-list)
	  do (set-window-buffer w b))
    (select-window (get-buffer-window cb))))

(cl-defun swap-windows ()
  "Rotate windows."
  (interactive)
  (rotate-windows)
  (other-window -1))
;;(swap-windows)

(cl-defun find-tag-no-prompt ()
  (interactive)
  (let* ((tagname (find-tag-default))
	 (buf (find-tag-noselect tagname))
	 (pos (with-current-buffer buf (point))))
    (condition-case nil
	(switch-to-buffer buf)
      (error (pop-to-buffer buf)))
    (goto-char pos)))
;;(find-tag-no-prompt)

(cl-defun find-tag-no-prompt-read-only ()
  (interactive)
  (let ((n (length (buffer-list))))
    (find-tag-no-prompt)
    (when (> (length (buffer-list)) n)
      (setf buffer-read-only t))))

(require 'mb-emacs-lisp)
(require 'LS)
(require 'mb-utils-buffer)
(require 'mb-metafont)

(cl-defun minor-mode-p (mode)
  "Check if symbol MODE is an active minor-mode in the current buffer."
  (condition-case nil
      (and (symbolp mode)
	   (symbol-value mode)
	   (find mode minor-mode-list))
    (error nil)))
;;(mapcar #'minor-mode-p '(slime-mode undo-tree-mode))

(cl-defun slime-p ()
  "Return nil iff slime-mode is not active"
  (minor-mode-p 'slime-mode))
;;(slime-p)

;;; Eval machinery
(cl-defun mb-eval-string (string &rest args)
  "This is the core function of the mb-eval machinery. Most other
eval function should end up here to secure a sort of conformity
between different"
  (cl-case major-mode
    (mb-lisp-mode
     (if (slime-p)
       (slime-eval string)
       (apply #'mb-lisp-eval-1 string args)))
    (emacs-lisp-mode (eval (read string)))
    (octave-mode (octave-send-string string))
    (js-mode (apply #'js-eval-string string args))
    (mbscilab-mode (mbscilab-eval string))))
;;(mb-eval-string "(+ 2 2)")

(cl-defun eval-form ()
  "This is special to Lisp languages only."
  (interactive)
  (cl-case major-mode
    ((emacs-lisp-mode mb-lisp-mode)
     (save-excursion
       (backward-up-list 1)
       (forward-sexp 1)
       (mb-eval-last-sexp)))
    (octave-mode (octave-send-block))))

(cl-defun mb-eval-last-sexp (&rest args)
  "Eval the syntaks expression preceding point."
  (interactive)
  (cl-case major-mode
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

(cl-defun eval-current-sexp ()
  (interactive)
  (cl-case major-mode
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

(cl-defun eval-defun-test (&optional no-eval-p)
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

(cl-defun gp-eval-buffer ()
  (interactive)
  (gnuplot-run-buffer)
  (let ((filename (string-match* "set[\t ]+output[\t ]+['\"]\\([^'\"]*\\)"
		    (buffer-string-no-properties) :num 1)))
    (find-file-other-window filename)))

(cl-defun strip-ssh (filename)
  "Strip the ssh prefix of filename"
  (or (string-match* "/ssh:[^:]*:\\(.*\\)" filename :num 1)
      filename))
;;(strip-ssh "/ssh:pf:/home/mats_progfab_no/git/problem-server/")

(cl-defun mb-eval-buffer (&optional args)
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

(cl-defun mb-eval-region (start end &optional printflag read-function)
  (interactive "r")
  (cl-case major-mode
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

(cl-defun mb-eval-region-from-point (&optional printflag read-function)
  "Evalutates content from POINT to the end of the buffer."
  (interactive "r")
  (mb-eval-region (bol*) (point-max) printflag read-function))

(cl-defun mb-eval-region-to-point (&optional printflag read-function)
  "Evalutates content from the end of the buffer to POINT."
  (interactive "r")
  (mb-eval-region (point-min) (eol*) printflag read-function))

(cl-defun mb-compile-buffer ()
  (interactive)
  (cl-case major-mode
    (octave-mode (octave-source-buffer))))

(cl-defun mb-normal-state-init ()
  (key-chord-mode 1))
;;(add-hook 'evil-normal-state-entry-hook 'mb-normal-state-init)

(cl-defun quailify-key-chord-input-method (result)
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

(cl-defun mb-key-chord-advice (x)
  (setq input-method-function 'key-chord-input-method))
;; activate-input-method overrides input-method-function, so therfore:
(advice-add #'activate-input-method :after #'mb-key-chord-advice)

(cl-defun mb-insert-state-init ()
  ;;(key-chord-mode 1)
  (when (member (buffer-name) '("arbeidslog" "log.org"))
    (activate-input-method 'norwegian-keyboard))
  (advice-add #'key-chord-input-method
	      :filter-return #'quailify-key-chord-input-method))
(add-hook 'evil-insert-state-entry-hook #'mb-insert-state-init)

(cl-defun mb-insert-state-cleanup ()
  (advice-remove #'key-chord-input-method #'quailify-key-chord-input-method))
(add-hook 'evil-insert-state-exit-hook #'mb-insert-state-cleanup)

(let ((default-color (cons (face-background 'mode-line)
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
(cl-loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
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

(cl-defun alf/key-chord-undefine (keys)
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
  (cl-destructuring-bind (beg end) (last-sexp-region)
    (evil-range (1+ beg) (1- end) 'inclusive :expanded t)))

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

(cl-defun print-last-pdf-in-Messages ()
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
    ("/" "/")
    (otherwise left)))
;;(mapcar #'mb-surround-lookup-right (list "`" "'" "(" "<"))

(cl-defun mb-surround-region (region left n)
  (mb-surround-region-1 region left (mb-surround-lookup-right left) n))

(cl-defun mb-surround-word (left n)
  (mb-surround-region (cons (bow*) (eow* n)) left 1))

(cl-defun mb-surround-symbol (left n)
  (message "%S" (list (point) (bos*) (eos*)))
  (mb-surround-region (list (bos*) (eos* n)) left 1))

(cl-defun mb-surround (left n)
  (if (use-region-p)
    (mb-surround-region (region) left n)
    ;; (mb-surround-word left n)
    (mb-surround-symbol left n)))

;; Undo surround: strictly assume surround is 1 char wide on both
;; sides
(cl-defun mb-undo-surround (n)
  (if (use-region-p)
    (error "Undo surround region not implemented!")
    (save-excursion
      (bow)
      (backward-delete-char n)
      (eow)
      (delete-char n))))

(cl-defun evil-increment-rectangle ()
  (interactive)
  (let ((p (point))
	(m (mark)))
    (let ((b (min p m))
	  (e (1- (max p m))))
      (message "%d %d" b e)
      (evil-apply-on-block
       (let (last)
	 #'(lambda (beg end)
	     (if last
	       (let ((s (number-to-string (cl-incf last))))
		 (overwrite-region s beg end))
	       (setf last (string-to-number (region-string beg (1+ end)))))))
       b e nil))))
;;(evil-increment-rectangle)

(evil-define-operator evil-rot13 (beg end type)
  "Convert text to its rot13 counterpart."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-rot13 beg end nil)
    (rot13-region beg end)))

(evil-define-operator evil-rot47 (beg end type)
  "Convert text to its rot47 counterpart."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-rot47 beg end nil)
    (rot47-region beg end)))

(evil-define-operator evil-split-5-points-string (beg end type)
  "Convert text to its rot47 counterpart."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-split-5-points-string beg end nil)
    (region-replace-raw
     (split-5-points-string (region-string beg end))
     (list beg end))))

(provide 'mb-evil)
