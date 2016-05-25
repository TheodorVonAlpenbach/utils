;;(require 'lisp-config)
(require 'hyperspec)
(require 'mb-inferior-lisp)

; note the important distinction between lisp and inferiorlisp
(define-key lisp-mode-map "\C-c;" 'comment-region)
(define-key lisp-mode-map "\C-c\C-d" 'lisp-compile-defun)
;;(define-key inferior-lisp-mode-map [(f12)] 'clisp-abort)
(define-key lisp-mode-shared-map "\C-c\C-q" 'lisp-indent-region)
(define-key global-map [(control h) (d)] 'mb-common-lisp-hyperspec)

;; overrules the corresponding shared-lisp
;; 2015-02-03 MB: what's this?
(define-key lisp-mode-map "\C-c\C-t"
  [?\M-- ?\M-x ?l ?i ?s ?p ?- ?e ?v ?a ?l ?- ?s ?e ?x ?p return ?\C-p ?\C-e C-M-backspace ?\C-y ?\M-> ?\C-x ?b return ?  ?= ?= ?> ?  ?\C-y])

;; proper indentations
(cl-indent 'aif 'if)

(provide 'lisp-map)

