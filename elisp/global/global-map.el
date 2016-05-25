;; General loads
;(require 'mb-vc)
;(load "mb-vc.el")
(require 'mb-paragraph)
(require 'info)

;; Print expression
(define-key global-map "\C-c\C-p" 'print)

;; Move point
(define-key global-map "\M-n" 'forward-paragraph)
(define-key Info-mode-map "\M-n" 'forward-paragraph) ;override
(define-key global-map "\M-p" 'backward-paragraph)

(define-key global-map [(meta control shift u)] 'up-list)
(define-key global-map [(control meta backspace)] 'backward-kill-sexp)

;; Kill
(define-key global-map [(meta U)] 'kill-paragraph)
(define-key global-map [(control backspace)] 'backward-kill-paragraph)

;; Sexp
(require 'mb-sexp)
(define-key global-map [(control meta shift l)] 'lift-sexp)

;; Indent
(define-key global-map "\C-\M-m" #'back-to-indentation-and-indent)

;; Mark commands
(define-key global-map "\M-#" 'mark-whole-word)

;; Transpose commands
(define-key global-map [(meta T)] 'transpose-paragraphs)
(define-key global-map [(meta control shift T)] 'transpose-sentences)

;; Case
(define-key global-map "\M-\S-u" 'upcase-previous-word)
(define-key global-map "\M-\S-l" 'downcase-previous-word)
(define-key global-map "\M-\S-c" 'capitalize-previous-word)

;; Fill
(define-key global-map "\C-c\C-q" 'fill-region)
(define-key global-map "\C-c\M-q" 'fill-region-as-paragraph)

;; Global c++
(define-key global-map "\C-c+" 'c++-mode)
(define-key global-map "\C-cc" 'compile)

;; Compile mode (make, grep, etc)
(define-key global-map [(control c) (meta n)] 'next-error)
(define-key global-map [(control c) (meta p)] 'previous-error)
(define-key global-map [(control c) (meta <)] 'first-error)

;; Buffer swaps
(define-key global-map [(f11)] #'(lambda () (interactive) (switch-to-buffer (other-buffer))))
(define-key global-map [(S-f11)] #'bury-buffer)
(define-key global-map [(M-f11)] #'smart-swap)
(define-key global-map [(control meta f11)] #'delete-window)

(define-key global-map [(control tab)] #'other-window)
(define-key global-map [(shift control tab)] #'(lambda () (interactive) (other-window -1)))

;; Useful file swap
(require 'mb-files)
(define-key global-map [(f9)] 'radio-playlist) ;earlier 'swap-l-y-file
(define-key global-map [(shift f9)] '-playlist) ;earlier 'swap-l-y-file

;; Misc use of define-key define-key global-map
(define-key global-map [(shift f8)] 'goto-line)
(define-key global-map [(f8)] 'shell)
(define-key global-map [(f7)]
  #'(lambda () (interactive) (inferior-lisp inferior-lisp-program)))
(define-key global-map [(shift f7)]
  #'(lambda () (interactive) (inferior-lisp (inferior-lisp-program-with-lispinit))))
(define-key global-map [(f6)]
  #'(lambda () (interactive) (lynx)))
(define-key global-map [(shift f6)]
  #'(lambda () (interactive) (ielm)))
(define-key global-map [(f5)] 'load-file)
(define-key global-map [(f3)] 'shrink-window)
(define-key global-map [(f4)] 'enlarge-window)
(define-key global-map [(shift f3)] 'shrink-window-horizontally)
(define-key global-map [(shift f4)] 'enlarge-window-horizontally)
(define-key global-map [(f1)] 'dic-lookup-at-point)
(define-key global-map [(f12)] #'(lambda () (interactive) (switch-to-buffer-other-window "*scheme*") (other-window 1)))
(define-key global-map [(f12)] #'(lambda () (interactive)
				  (tex-compile default-directory 
					       (concat "pdflatex \\\\nonstopmode\\\\input "
						       "MUS3090-oppgave-370931.tex"))))
(define-key global-map [(control f12)] #'(lambda () (interactive)
				  (tex-compile default-directory 
					       (concat "pdflatex \\\\input "
						       "MUS3090-oppgave-370931.tex"))))



(require 'mb-frames)
(define-key global-map [(meta f3)] #'shrink-frame)
(define-key global-map [(meta f4)] #'enlarge-frame)
(define-key global-map [(shift meta f3)] #'shrink-frame-horizontally)
(define-key global-map [(shift meta f4)] #'enlarge-frame-horizontally)


;; Global prefix keys
(require 'mb-print)
(setq print-map (make-sparse-keymap "Print"))
(define-key global-map "\C-cp" print-map)
(define-key print-map "r" 'print-notepad-region)
(define-key print-map "f" 'print-notepad-file)
(define-key print-map "b" 'print-notepad-buffer)

(require 'mb-insert)
(setq insert-map (make-sparse-keymap "Insert"))
(define-key global-map "\C-ci" insert-map)

;; Delimiters
(define-key insert-map "g" 'insert-guillemets)
(define-key insert-map "q" 'insert-quotes)
(define-key insert-map "\'" 'insert-single-quotes)
(define-key insert-map "\"" 'insert-double-quotes)
(define-key insert-map "*" 'insert-asterisks)
(define-key insert-map "[" 'insert-square-brackets)
(define-key insert-map "{" 'insert-curly-brackets)
(define-key insert-map "<" 'insert-angle-brackets)
(define-key insert-map "x" 'insert-xml-tag)
(define-key insert-map "p" 'insert-post-scriptum)

;; The following inserts are deprecated. Use standard C-x 8 ... instead
(define-key insert-map "1" (insert-n "¹"))
(define-key insert-map "2" (insert-n "²"))
(define-key insert-map "3" (insert-n "³"))
(define-key insert-map "0" (insert-n "°"))
(define-key insert-map "o" (insert-n "º"))
(define-key insert-map "a" (insert-n "ª"))
(define-key insert-map "m" (insert-n "¯"))
(define-key insert-map "b" (insert-n "·"))
(define-key insert-map "B" (insert-n "ß"))
(define-key insert-map "c" (insert-n "ç"))

(define-key insert-map "s" 'insert-skip-quote)
(define-key insert-map "u" 'insert-underline)
(define-key insert-map "d" 'insert-date)
(define-key insert-map "t" 'insert-time)
(define-key insert-map "D" 'insert-date-and-time)
(define-key insert-map "i" 'insert-todo-item)

(define-key insert-map "e" 'insert-expression)

;; Greek letters shortcut
;; abcdefghijklmnopqrstuvwxyz
;; αβψδεφγηιξκλμνοπ;ρστθωςχυζ
;;   !    ! !      !   !!! 
(setf greek-map (make-sparse-keymap "Greek"))
(define-key global-map (kbd "C-x 8 g") greek-map)
(loop for lc across "ABCDEFGHIJKLMNOPRSTUVWXYZabcdefghijklmnoprstuvwxyz"
      for gc across "ΑΒΨΔΕΦΓΗΙΞΚΛΜΝΟΠΡΣΤΘΩςΧΥΖαβψδεφγηιξκλμνοπρστθωςχυζ"
      do (define-key greek-map (string lc) (insert-n (string gc))))

(require 'mb-things)
(define-key global-map [(control +)]  #'(lambda (n) (interactive "p") (inc-thing-at-point n 1)))
(define-key global-map [(meta +)]  #'(lambda (n) (interactive "p") (inc-thing-at-point n 2)))
(define-key global-map [(meta control +)]  #'(lambda (n) (interactive "p") (inc-thing-at-point n 3)))

(require 'mb-grep)
(setq find-map (make-sparse-keymap "Find"))
(define-key global-map "\C-cf" find-map)
(define-key find-map "g" 'mb-grep)
(define-key find-map "h" 'mb-h-grep)
(define-key find-map "c" 'mb-cpp-grep)
(define-key find-map "a" 'mb-all-grep)

(cl-defun gfind-compile (pattern &optional (file-extension "") (level 0))
  (compile (format "gfind -u %d %s %s" level pattern file-extension)))
;;(gfind-compile "defun")

(cl-defun gfind (n)
  (interactive "P")
  (gfind-compile
     (or (symbol-at-point)
	 (read-from-minibuffer "Find in files: "))
     (file-name-extension buffer-file-name)
     (or n 0)))

(require 'mb-hexl)

(provide 'global-map)
