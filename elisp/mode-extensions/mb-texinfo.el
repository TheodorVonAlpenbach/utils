;;;; This module contains extensions the Emacs' texinfo mode.

(define-skeleton texinfo-insert-@xref
  "Insert a `@xref{...}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (bow)
  "@ref{" _ "}")

(defun mb-texinfo-map ()
  (let ((map (make-sparse-keymap))
	(insert-map         (make-sparse-keymap))  ; i
	(update-map         (make-sparse-keymap))  ; u
	(tex-map            (make-sparse-keymap))  ; t
	(makeinfo-map       (make-sparse-keymap))  ; m
	(ref-map            (make-sparse-keymap))  ; r
	(texinfo-format-map (make-sparse-keymap))) ; e
    ;; map is currently valid only on normal and visual state
    (key-chord-define evil-normal-state-local-map "gh" map)
    (key-chord-define evil-visual-state-local-map "gh" map)

    (define-key map "s" 'texinfo-show-structure)
    (define-key map "]" 'up-list)
    (define-key map "/" 'texinfo-insert-@end)
    (define-key map "{" 'texinfo-insert-braces)
    (define-key map "o" 'texinfo-insert-block)

    (define-key map "r" ref-map)
    (define-key ref-map "x" 'texinfo-insert-@xref)
    (define-key ref-map ";" 'texinfo-insert-@xref)
    (define-key ref-map "r" 'texinfo-insert-@ref)
    (define-key ref-map "p" 'texinfo-insert-@pxref)
    (define-key ref-map "u" 'texinfo-insert-@uref)

    (define-key map "i" insert-map)
    (define-key insert-map "c" 'texinfo-insert-@code)
    (define-key insert-map "d" 'texinfo-insert-@dfn)
    (define-key insert-map "D" 'texinfo-start-menu-description)
    (define-key insert-map "e" 'texinfo-insert-@end)
    (define-key insert-map "E" 'texinfo-insert-@emph)
    (define-key insert-map "f" 'texinfo-insert-@file)
    (define-key insert-map "i" 'texinfo-insert-@item)
    (define-key insert-map "k" 'texinfo-insert-@kbd)
    (define-key insert-map "m" 'texinfo-insert-@email)
    (define-key insert-map "n" 'texinfo-insert-@node)
    (define-key insert-map "o" 'texinfo-insert-@noindent)
    (define-key insert-map "q" 'texinfo-insert-@quotation)
    (define-key insert-map "s" 'texinfo-insert-@samp)
    (define-key insert-map "S" 'texinfo-insert-@strong)
    (define-key insert-map "t" 'texinfo-insert-@table)
    (define-key insert-map "u" 'texinfo-insert-@uref)
    (define-key insert-map "v" 'texinfo-insert-@var)
    (define-key insert-map "x" 'texinfo-insert-@example)

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

(provide 'mb-texinfo)