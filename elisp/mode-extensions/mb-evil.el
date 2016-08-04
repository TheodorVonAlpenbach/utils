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

;;;; Also useful to look at key bindings in ~/.emacs.d/elpa/evil-20160525.1148/evil-maps.el
(require 'evil)
(require 'evil-cleverparens)
(require 'evil-cleverparens-text-objects)
;;(require 'evil-org) ; does not work

(evil-mode 1)
(setf evil-move-beyond-eol t)
(add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
(add-hook 'lisp-mode-hook #'evil-cleverparens-mode)

;;; Finally, need to revert .emacs* buffers since they have not yet been
;;; hooked by the functionality loaded here
(loop for b in (list (get-buffer ".emacs") (get-file-buffer +emacs-local+))
      do (with-current-buffer b (revert-buffer nil t)))

;;; Keyboard shortcuts
(define-key global-map "\M-x" 'execute-extended-command)
(define-key evil-normal-state-map "\M-x" 'execute-extended-command)
(define-key evil-normal-state-map " " 'scroll-up-command)
(define-key evil-normal-state-map [return] 'scroll-down-command)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "df" 'evil-normal-state)
(key-chord-define evil-normal-state-map ";j" 'save-buffer)
(key-chord-define evil-normal-state-map "vn" 'ido-switch-buffer)

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

(let ((swap-map (make-sparse-keymap)))
  (key-chord-define evil-normal-state-map "vo" swap-map)
  (define-key swap-map "v" #'(lambda () (interactive) (switch-to-buffer (other-buffer))))
  (define-key swap-map "s" #'smart-swap)
  (define-key swap-map "b" #'bury-buffer)
  (define-key swap-map "B" #'unbury-buffer)
  (define-key swap-map "o" #'other-window)
  (define-key swap-map "c" #'rotate-windows)
  (define-key swap-map "r" #'revert-buffer)
  (define-key swap-map "f" #'find-file)
  (define-key swap-map "F" #'find-file-read-only)
  (define-key swap-map "a" #'ffap-no-prompt)
  (define-key swap-map "n" #'ffap-next)
  (define-key swap-map "N" #'ffap-previous)
  (define-key swap-map "k" #'kill-buffer))

(let ((insert-map (make-sparse-keymap)))
  (key-chord-define evil-normal-state-map "vi" insert-map)
  (define-key insert-map "d" #'insert-date)
  (define-key insert-map "t" #'insert-time))

;; TODO: move these two defuns elsewhere
(defun eval-form ()
  (interactive)
  (save-excursion
    (evil-cp-up-sexp 1)
    (forward-char 1)
    (eval-last-sexp nil)))

(defun eval-current-sexp ()
  (interactive)
  (save-excursion
    (forward-sexp 1)
    (eval-last-sexp nil)))
;;(+ (+ 111 2) 3)

(defun eval-defun-test (&optional no-eval-p)
  (interactive)
  (save-excursion
    (unless no-eval-p
      (eval-defun nil))
    (evil-cp-end-of-defun)
    (eol :offset 1)
    (eval-last-sexp nil)))
;;(eval-defun-test)

(let ((eval-map (make-sparse-keymap)))
  (key-chord-define evil-normal-state-map "kj" eval-map)
  (define-key eval-map "d" #'eval-defun)
  (define-key eval-map "b" #'eval-buffer)
  (define-key eval-map "r" #'eval-region)
  (define-key eval-map "l" #'eval-last-sexp)
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

(provide 'mb-evil)
