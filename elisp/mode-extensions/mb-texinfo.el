;;;; This module contains extensions the Emacs' texinfo mode.

(cl-defun texinfo-atfiy (function-name &optional (argument ""))
  "Prepend \"@\" to FUNCTION-NAME and append curly parentheses.
If ARGUMENT is a string insert it in the pair of curly parentheses."
  (format "@%s{%s}" function-name argument))
;;(texinfo-atfiy "foo" "bar")

(defun texinfo-insert-@ (code)
  (if (use-region-p)
    (region-replace (format "@%s{%%s}" code))
    (if (symbol-at-point)
     (destructuring-bind (beg . end) (bounds-of-thing-at-point 'symbol)
       (insert (texinfo-atfiy code (delete-and-extract-region beg end))))
     (insert (format "@%s{}" code))
     (backward-char 1))))

(defmacro texinfo-def-insert-@-fn (code)
  "Define an interactive function with only form (texinfo-insert-@ code)."
  `(defun ,(intern (concat "texinfo-insert-@" code)) ()
     "Not documented."
     (interactive)
     (texinfo-insert-@ ,code)))
;;(texinfo-def-insert-@-fn "xref")

(defconst +texinfo-inserts+
  '(var xref ref pxref code result uref file samp verb))

(cl-defmacro texinfo-define-inserts
    (&optional (symbols +texinfo-inserts+))
  `(progn ,@(loop for code in symbols collect
		  `(texinfo-def-insert-@-fn ,(sstring code)))))
(texinfo-define-inserts)
;; (macroexpand `(progn ,@(loop for code in '(var xref ref pxref)
;; 		  collect `(texinfo-def-insert-@-fn ,(sstring code)))))

(defun mb-texinfo-ref-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "x" 'texinfo-insert-@xref)
    (define-key map "r" 'texinfo-insert-@ref)
    (define-key map "p" 'texinfo-insert-@pxref)
    (define-key map "u" 'texinfo-insert-@uref)
    map))

(defun mb-texinfo-insert-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'texinfo-insert-@code)
    (define-key map "d" 'texinfo-insert-@dfn)
    (define-key map "D" 'texinfo-start-menu-description)
    (define-key map "e" 'texinfo-insert-@end)
    (define-key map "E" 'texinfo-insert-@emph)
    (define-key map "f" 'texinfo-insert-@file)
    (define-key map "i" 'texinfo-insert-@item)
    (define-key map "k" 'texinfo-insert-@kbd)
    (define-key map "m" 'texinfo-insert-@email)
    (define-key map "n" 'texinfo-insert-@node)
    (define-key map "o" 'texinfo-insert-@noindent)
    (define-key map "q" 'texinfo-insert-@quotation)
    (define-key map "r" 'texinfo-insert-@result)
    (define-key map "s" 'texinfo-insert-@samp)
    (define-key map "S" 'texinfo-insert-@strong)
    (define-key map "t" 'texinfo-insert-@table)
    (define-key map "u" 'texinfo-insert-@uref)
    (define-key map "v" 'texinfo-insert-@var)
    (define-key map "V" 'texinfo-insert-@verb)
    (define-key map "x" 'texinfo-insert-@example)
    map))

(defun mb-texinfo-snippet-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "e" 'texinfo-insert-group/example-snippet)
    (define-key map "E" 'texinfo-insert-example-snippet)
    (define-key map "g" 'texinfo-insert-group-snippet)
    (define-key map "i" 'texinfo-insert-newline)
    map))

(defun mb-texinfo-map ()
  (let ((map (make-sparse-keymap))
	(update-map         (make-sparse-keymap))  ; u
	(tex-map            (make-sparse-keymap))  ; t
	(makeinfo-map       (make-sparse-keymap))  ; m
	(snippet-map        (make-sparse-keymap))  ; s
	(texinfo-format-map (make-sparse-keymap))) ; e
    ;; map is currently valid only on normal and visual state
    (key-chord-define evil-normal-state-local-map "gh" map)
    (key-chord-define evil-visual-state-local-map "gh" map)

    (define-key map "s" 'texinfo-show-structure)
    (define-key map "]" 'up-list)
    (define-key map "/" 'texinfo-insert-@end)
    (define-key map "{" 'texinfo-insert-braces)
    (define-key map "o" 'texinfo-insert-block)

    (define-key map "r" (mb-texinfo-ref-map))
    (define-key map "s" (mb-texinfo-snippet-map))
    (define-key map "i" (mb-texinfo-insert-map))

    (define-key map "u" update-map)
    (define-key update-map "m" 'texinfo-make-menu)
    (define-key update-map "M" 'texinfo-master-menu)
    (define-key update-map "n" 'texinfo-update-node)
    (define-key update-map "e" 'texinfo-every-node-update)
    (define-key update-map "a" 'texinfo-all-menus-update)

    (define-key map "t" tex-map)
    (define-key tex-map "k" 'tex-kill-job)
    (define-key tex-map "x" 'texinfo-quit-job)
    (define-key tex-map "l" 'tex-recenter-output-buffer)
    (define-key tex-map "d" 'texinfo-delete-from-print-queue)
    (define-key tex-map "q" 'tex-show-print-queue)
    (define-key tex-map "p" 'texinfo-tex-print)
    (define-key tex-map "v" 'texinfo-tex-view)
    (define-key tex-map "i" 'texinfo-texindex)
    (define-key tex-map "r" 'texinfo-tex-region)
    (define-key tex-map "b" 'texinfo-tex-buffer)

    (define-key map "m" makeinfo-map)
    (define-key makeinfo-map "k" 'kill-compilation)
    (define-key makeinfo-map "l" 'makeinfo-recenter-compilation-buffer)
    (define-key makeinfo-map "r" 'makeinfo-region)
    (define-key makeinfo-map "b" 'makeinfo-buffer)
    (define-key makeinfo-map "h" 'mb-texinfo-make-html)
    (define-key makeinfo-map "H" 'mb-texinfo-make-html)
    (define-key makeinfo-map "n" 'next-error)
    (define-key makeinfo-map "p" 'previous-error)

    (define-key map "e" texinfo-format-map)
    (define-key texinfo-format-map "r" 'texinfo-format-region)
    (define-key texinfo-format-map "b" 'texinfo-format-buffer)

    map))
;;(mb-texinfo-map)

(add-hook 'Texinfo-mode-hook 'mb-texinfo-map)

(defun mb-texinfo-make-html ()
  (interactive)
  (makeinfo-buffer)
  (call-process* "makeinfo" "--html" (buffer-file-name)))

(defun mb-texinfo-install-html ()
  (interactive)
  (call-process* "cp" "-r" "lsbin" "/ls/platinum/u1/mbe/html/"))

;;; Snippets
(defun texinfo-insert-newline (&optional n no-trailing-space-p)
  "Insert a texinfo commented newline at point"
  (interactive)
  (bol)
  (loop with s = (format "##%s\n" (if no-trailing-space-p "" " "))
	repeat (or n 1) do (insert s))
  (unless no-trailing-space-p (eol)))

(defun texinfo-insert-env (env &optional no-space-p no-intraspace-p)
  "Insert an texinfo ENV enviroment snippet within a comment
region surrounded by empty lines. If optional NO-SPACE-P is nil,
then skip the surrounding empty lines. If optional
NO-INTRASPACE-P is nil, then the additional space between the
environment delimiter lines is skipped."
  (bol)
  (unless no-space-p (texinfo-insert-newline 1 t))
  (insert (format "## @%s\n" (sstring env) (sstring env)))
  (texinfo-insert-newline 1 t)
  (insert (format "## @end %s\n" (sstring env) (sstring env)))
  (unless no-space-p (texinfo-insert-newline 1 t))
  (previous-line (+ 2 (if no-space-p 0 1)))
  (eol))

(defun texinfo-insert-group-snippet (&optional no-space-p)
  "Insert a group snippet within a comment region."
  (interactive)
  (texinfo-insert-env 'group no-space-p))

(defun texinfo-insert-example-snippet (&optional no-space-p)
  "Insert a example snippet within a comment region."
  (interactive)
  (texinfo-insert-env 'example no-space-p))

(defun texinfo-insert-group/example-snippet (&optional no-space-p)
  "Insert a group example snippet within a comment region."
  (interactive)
  (texinfo-insert-env 'group nil t)
  (bol)
  (kill-line 1)
  (texinfo-insert-env 'example t))


(provide 'mb-texinfo)
