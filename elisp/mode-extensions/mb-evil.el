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
;;(require 'evil-org) ; does not work

(require 'evil-exchange)
(setf evil-exchange-key "go")
(setf evil-exchange-cancel-key "gO")
(evil-exchange-install)

(evil-mode 1)
(setf evil-move-beyond-eol t)
(add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
(add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
(setf evil-cleverparens-swap-move-by-word-and-symbol t)

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
(evil-define-key '(normal visual) global-map " " 'scroll-up-command)
(define-key evil-normal-state-map [return] 'scroll-down-command)
;;(evil-define-key '(motion) global-map [tab] #'forward-button)
;;(evil-define-key '(normal) global-map [tab] #'self-insert-command)
;;(define-key global-map [tab] #'self-insert-command)

(defun evil-move-past-close ()
  "Is made for insert state."
  (interactive)
  (up-list 1)
  (insert " "))

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "dg" 'evil-move-past-close)
(key-chord-define evil-insert-state-map "df" 'evil-normal-state)
(key-chord-define evil-insert-state-map "f;" 'yank)
(key-chord-define evil-normal-state-map "j;" 'save-buffer)
(key-chord-define evil-normal-state-map "J:" 'save-some-buffers)
(evil-key-chord-define '(normal visual motion) global-map "vn" 'ido-switch-buffer)

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

(defun rotate-windows ()
  "Rotate windows."
  (interactive)
  (loop with buffers = (loop for w in (window-list) collect (window-buffer w))
	for b in (rotate-list buffers)
	for w in (window-list)
	do (set-window-buffer w b)))
;;(rotate-windows)

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

(let ((swap-map (make-sparse-keymap)))
  (evil-key-chord-define '(normal motion) global-map "vo" swap-map)
  (define-key swap-map "v" #'(lambda () (interactive) (switch-to-buffer (other-buffer))))
  (define-key swap-map "s" #'smart-swap)
  (define-key swap-map "b" #'bury-buffer)
  (define-key swap-map "B" #'unbury-buffer)
  (define-key swap-map "o" #'other-window)
  (define-key swap-map "c" #'rotate-windows)
  (define-key swap-map "D" #'delete-window)
  (define-key swap-map "r" #'revert-buffer)
  (define-key swap-map "T" #'find-tag-no-prompt)
  (define-key swap-map "t" #'find-tag-no-prompt-read-only)
  (define-key swap-map "f" #'find-file)
  (define-key swap-map "F" #'find-file-read-only)
  (define-key swap-map "a" #'ffap-no-prompt)
  (define-key swap-map "A" #'ffap-no-prompt-read-only)
  (define-key swap-map "n" #'ffap-next)
  (define-key swap-map "N" #'ffap-previous)
  (define-key swap-map "d" #'(lambda () (interactive) (kill-buffer (current-buffer))))
  (define-key swap-map "k" #'kill-buffer))

(let ((insert-map (make-sparse-keymap)))
  (key-chord-define evil-normal-state-map "vi" insert-map)
  (define-key insert-map "f" #'cl-ify-form)
  (define-key insert-map "F" #'cl-ify-defun)
  (define-key insert-map "d" #'insert-date)
  (define-key insert-map "t" #'insert-time)
  (define-key insert-map "c" #'comment-region)
  (define-key insert-map "u" #' uncomment-region))

(require 'mb-metafont)

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

(defun mb-eval-string (string &rest args)
  (case major-mode
    (mb-lisp-mode (apply #'mb-lisp-eval-1 string args))
    (emacs-lisp-mode (eval (read string)))))
;;(mb-eval-string "(+ 2 2)")


(defun eval-form ()
  (interactive)
  (case major-mode
    ((emacs-lisp-mode mb-lisp-mode)
     (save-excursion
       (evil-cp-up-sexp 1)
       (forward-char 1)
       (eval-last-sexp nil)))
    (octave-mode (octave-send-block))))

(defun mb-eval-last-sexp (&rest args)
  (interactive)
  (case major-mode
    (emacs-lisp-mode (eval-last-sexp args))
    (mb-lisp-mode
     (if args
       (slime-pprint-eval-last-expression)
       (slime-eval-last-expression)))
    (python-mode (apply #'python-shell-send-region
			(mb-python-last-sexp-region)))
    (t (apply #'mb-eval-region (last-sexp-region) args))))

(defun eval-current-sexp ()
  (interactive)
  (case major-mode
    ((emacs-lisp-mode mb-lisp-mode)
     (save-excursion
      (forward-sexp 1)
      (eval-last-sexp nil)))
    (octave-mode
     (save-excursion
       (octave-send-string
	(string-match* "#*\\([^#]*\\)" (line-string) :num 1))))))
;;(+ (+ 111 2) 3)

(defun eval-defun-test (&optional no-eval-p)
  (interactive)
  (case major-mode
    ((emacs-lisp-mode mb-lisp-mode python-mode)
     (save-excursion
     (unless no-eval-p
       (mb-eval-defun))
     (evil-cp-end-of-defun)
     (eol :offset 1)
     (mb-eval-last-sexp)))
    (metafont-mode (meta-eval-buffer))
    (octave-mode
     (save-excursion
       (unless no-eval-p
	 (octave-send-defun))
       (end-of-defun)
       (eval-current-sexp)))))
;;(eval-defun-test)

(defun gp-eval-buffer ()
  (interactive)
  (gnuplot-run-buffer)
  (let ((filename (string-match* "set[\t ]+output[\t ]+['\"]\\([^'\"]*\\)"
		    (buffer-string-no-properties) :num 1)))
    (find-file-other-window filename)))

(defun mb-eval-buffer (&optional args)
  (interactive)
  (case major-mode
    (emacs-lisp-mode (apply #'eval-buffer args))
    (gnuplot-mode (gnuplot-send-buffer-to-gnuplot))
    (mb-lisp-mode (if (slime-p)
		    (slime-compile-and-load-file)
		    (mb-lisp-eval-buffer)))
    (metafont-mode (meta-compile-file (buffer-file-name)))
    (octave-mode (octave-send-buffer))
    (python-mode (python-shell-send-buffer nil))
    ((c++-mode cc-mode) (compile "make -k"))))

(defun mb-eval-defun ()
  (interactive)
  (case major-mode
    (emacs-lisp-mode (eval-defun nil))
    (mb-lisp-mode
     (when (slime-p)
       (slime-eval-defun)))
    (octave-mode (octave-send-defun))
    (python-mode (apply #'python-shell-send-region (mb-python-defun-region)))))

(defun mb-eval-region (start end &optional printflag read-function)
  (interactive "r")
  (case major-mode
    (emacs-lisp-mode (eval-region start end printflag read-function))
    (python-mode (python-shell-send-region start end nil))
    (octave-mode (octave-send-region start end))))

(defun mb-eval-region-from-point (&optional printflag read-function)
  (interactive "r")
  (mb-eval-region (bol*) (point-max) printflag read-function))

(let ((eval-map (make-sparse-keymap)))
  (key-chord-define evil-normal-state-map "kj" eval-map)
  (define-key eval-map "d" #'mb-eval-defun)
  (define-key eval-map "b" #'mb-eval-buffer)
  (define-key eval-map "s" #'mb-eval-region-from-point)
  (define-key eval-map "r" #'mb-eval-region)
  (define-key eval-map "l" #'mb-eval-last-sexp)
  (define-key eval-map "f" #'eval-form)
  (define-key eval-map "c" #'eval-current-sexp)
  (define-key eval-map "e" #'eval-expression)
  (define-key eval-map "t" #'eval-defun-test)
  (define-key eval-map "T" #'(lambda () (interactive) (eval-defun-test t))))


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
	  (string/= current-input-method "norwegian-keyboard")
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
  (when (member (buffer-name) '("arbeidslog" "todo.org" "log.org"))
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

(define-key evil-outer-text-objects-map "g" #'mb-evil-buffer)

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

(provide 'mb-evil)
