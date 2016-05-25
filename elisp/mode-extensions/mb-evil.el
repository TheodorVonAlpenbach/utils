(require 'evil)

(define-key global-map "\M-x" 'execute-extended-command)
(define-key evil-normal-state-map "\M-x" 'execute-extended-command)

(require 'key-chord)
(key-chord-define evil-insert-state-map "df" 'evil-normal-state)
;;(key-chord-mode t)

;; (sp-pair "'" nil :actions :rem)
;; (require 'evil-lisp-state)
;; (evil-leader/set-leader "<SPC>")
;; (global-evil-leader-mode)
;; (evil-mode 1)

;; (dolist (mm evil-lisp-state-major-modes)
;;   (evil-leader/set-key-for-mode mm "<SPC>" 'scroll-up-command)
;; )

(defun quailify-key-chord-input-method (result)
  "Modifies if necessary the RESULT of `key-chord-input-method'
If RESULT is a key-chord, i.e. it is a list on the form
\(KEYCHORD CHAR1 CHAR2 ...\), RESULT is returned unmodified. If
not, RESULT should be a list of one character (an error is
signaled otherwise), and this character is passed to
`quail-input-method' which result is finally returned."
  (if (and (listp result)
	   (> (length result) 2)
	   (eql (first result) 'key-chord))
    result
    (if (and (listp result)
	     (= (length result) 1))
      (quail-input-method (first result))
      (error "Unexpected output from KEY-CHORD-INPUT-METHOD: %S" result))))

(defun mb-insert-state-init ()
  (key-chord-mode t)
    (when (member (buffer-name) '("arbeidslog" "todo.org" "log.org"))
      (activate-input-method 'norwegian-keyboard)
    ;; now, activate-input-method overrides input-method-function, so therfore:
    (setq input-method-function 'key-chord-input-method)
    (advice-add #'key-chord-input-method :filter-return #'quailify-key-chord-input-method)))

(defun mb-insert-state-cleanup ()
  (key-chord-mode nil)
  (setq input-method-function nil)
  (advice-remove #'key-chord-input-method #'quailify-key-chord-input-method))

(add-hook 'evil-insert-state-entry-hook 'mb-insert-state-init)
(add-hook 'evil-insert-state-exit-hook 'mb-insert-state-cleanup)

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
(provide 'mb-evil)
