;; todo: 
; 1 base function should be surround-region
; 2 take a cons as argument (prefix . suffix)
(load-library "~/lisp/progmodes/make-mode")

(defun mb-enclose-word (enclosure)
  "Encloses a word with prefix and suffix strings of enclosure.
ENCLOSURE is a cons (PREFIX . SUFFIX). Note that if point is beteen
two consequtive words, the former is enclosed"
  (interactive "*")
  (backward-word 1)
  (insert (first enclosure))
  (forward-word 1)
  (insert (rest enclosure)))
ahhlo

(defun mb-enclose-region (beg end enclosure)
  "Encloses a word with prefix and suffix strings of enclosure.
ENCLOSURE is a cons (PREFIX . SUFFIX)."
  (interactive "*r")
  (goto-char end)
  (insert (rest enclosure))
  (goto-char beg)
  (insert (first enclosure)))

(defun mb-make-reference-word () "" (interactive "*")
  (mb-enclose-word (cons "\$\(" ")")))

(defun mb-make-reference-region (beg end) "" (interactive "*r")
  (upcase-region beg end)
  (mb-enclose-region beg end (cons "\$\(" ")")))

; (mb-enclose-word (cons "foo" "bar"))  qwe

(setq mb-makefile-mode-map (make-sparse-keymap ""))
(define-key makefile-mode-map "\C-ci" mb-makefile-mode-map)
(define-key mb-makefile-mode-map "w" 'mb-make-reference-word)
(define-key mb-makefile-mode-map "r" 'mb-make-reference-region)

;; "global" c++-mode
(define-key makefile-mode-map "\M-n" 'next-error)
(define-key makefile-mode-map "\M-p" 'previous-error)

(provide 'mb-make-map)
