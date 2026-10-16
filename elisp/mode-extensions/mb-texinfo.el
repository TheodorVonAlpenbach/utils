;;; mb-texinfo.el --- Personal Texinfo mode extensions -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tex-mode)
(require 'texinfo)
(require 'thingatpt)
(require 'xref)

(declare-function key-chord-define "key-chord" (keymap keys command))
(defvar evil-normal-state-local-map)
(defvar evil-visual-state-local-map)

(defun texinfo-atfiy (function-name &optional argument)
  "Wrap ARGUMENT in a Texinfo command named FUNCTION-NAME."
  (format "@%s{%s}" function-name (or argument "")))

(defun texinfo-insert-@ (code)
  "Wrap the active region or symbol at point in Texinfo command CODE.
When neither is available, insert an empty command and leave point
between its braces."
  (let ((code (format "%s" code)))
    (cond
     ((use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (text (delete-and-extract-region beg end)))
        (goto-char beg)
        (insert (texinfo-atfiy code text))
        (deactivate-mark)))
     ((bounds-of-thing-at-point 'symbol)
      (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point 'symbol)))
        (let ((text (delete-and-extract-region beg end)))
          (goto-char beg)
          (insert (texinfo-atfiy code text)))))
     (t
      (insert (texinfo-atfiy code))
      (backward-char 1)))))

(defmacro mb-texinfo--define-insertion-command (code)
  "Define a personal interactive insertion command for Texinfo CODE."
  (let* ((code-name (format "%s" code))
         (function-name (intern (concat "mb-texinfo-insert-@" code-name))))
    `(defun ,function-name ()
       ,(format "Insert or wrap text in @%s{}." code-name)
       (interactive)
       (texinfo-insert-@ ,code-name))))

(eval-and-compile
  (defconst +texinfo-inserts+
    '(var xref ref pxref code result uref file samp verb dots)
    "Texinfo command names with generated insertion functions."))

(defmacro mb-texinfo--define-insertion-commands (symbols)
  "Define personal Texinfo insertion commands for SYMBOLS."
  `(progn
     ,@(mapcar (lambda (code)
                 `(mb-texinfo--define-insertion-command ,code))
               symbols)))

(mb-texinfo--define-insertion-commands
 (var xref ref pxref code result uref file samp verb dots))

(defalias 'texinfo-insert-@xref #'mb-texinfo-insert-@xref)
(defalias 'texinfo-insert-@ref #'mb-texinfo-insert-@ref)
(defalias 'texinfo-insert-@pxref #'mb-texinfo-insert-@pxref)
(defalias 'texinfo-insert-@result #'mb-texinfo-insert-@result)
(defalias 'texinfo-insert-@verb #'mb-texinfo-insert-@verb)
(defalias 'texinfo-insert-@dots #'mb-texinfo-insert-@dots)

(defun mb-texinfo-ref-map ()
  "Return the Texinfo reference prefix map."
  (let ((map (make-sparse-keymap)))
    (define-key map "x" #'mb-texinfo-insert-@xref)
    (define-key map "r" #'mb-texinfo-insert-@ref)
    (define-key map "p" #'mb-texinfo-insert-@pxref)
    (define-key map "u" #'mb-texinfo-insert-@uref)
    map))

(defun mb-texinfo-insert-map ()
  "Return the Texinfo insertion prefix map."
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'mb-texinfo-insert-@code)
    (define-key map "d" #'mb-texinfo-insert-@dots)
    (define-key map "D" #'texinfo-start-menu-description)
    (define-key map "e" #'texinfo-insert-@end)
    (define-key map "E" #'texinfo-insert-@emph)
    (define-key map "f" #'mb-texinfo-insert-@file)
    (define-key map "i" #'texinfo-insert-@item)
    (define-key map "k" #'texinfo-insert-@kbd)
    (define-key map "m" #'texinfo-insert-@email)
    (define-key map "n" #'texinfo-insert-@node)
    (define-key map "o" #'texinfo-insert-@noindent)
    (define-key map "q" #'texinfo-insert-@quotation)
    (define-key map "r" #'mb-texinfo-insert-@result)
    (define-key map "s" #'mb-texinfo-insert-@samp)
    (define-key map "S" #'texinfo-insert-@strong)
    (define-key map "t" #'texinfo-insert-@table)
    (define-key map "u" #'mb-texinfo-insert-@uref)
    (define-key map "v" #'mb-texinfo-insert-@var)
    (define-key map "V" #'mb-texinfo-insert-@verb)
    (define-key map "x" #'texinfo-insert-@example)
    map))

(defun mb-texinfo-snippet-map ()
  "Return the Texinfo snippet prefix map."
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'texinfo-insert-group/example-snippet)
    (define-key map "E" #'texinfo-insert-example-snippet)
    (define-key map "g" #'texinfo-insert-group-snippet)
    (define-key map "i" #'texinfo-insert-newline)
    map))

(defun mb-texinfo-map ()
  "Build and optionally install the personal Texinfo prefix map.
Install the `gh' key chord only when both key-chord and the
corresponding Evil local maps are available."
  (let ((map (make-sparse-keymap))
        (update-map (make-sparse-keymap))
        (tex-map (make-sparse-keymap))
        (makeinfo-map (make-sparse-keymap))
        (texinfo-format-map (make-sparse-keymap))
        (texinfo-goto-map (make-sparse-keymap)))
    (define-key map "]" #'up-list)
    (define-key map "/" #'texinfo-insert-@end)
    (define-key map "{" #'texinfo-insert-braces)
    (define-key map "o" #'texinfo-insert-block)
    (define-key map "r" (mb-texinfo-ref-map))
    (define-key map "s" (mb-texinfo-snippet-map))
    (define-key map "i" (mb-texinfo-insert-map))

    (define-key map "u" update-map)
    (define-key update-map "m" #'texinfo-make-menu)
    (define-key update-map "M" #'texinfo-master-menu)
    (define-key update-map "n" #'texinfo-update-node)
    (define-key update-map "e" #'texinfo-every-node-update)
    (define-key update-map "a" #'texinfo-all-menus-update)

    (define-key map "t" tex-map)
    (define-key tex-map "k" #'tex-kill-job)
    (define-key tex-map "x" #'texinfo-quit-job)
    (define-key tex-map "l" #'tex-recenter-output-buffer)
    (define-key tex-map "d" #'texinfo-delete-from-print-queue)
    (define-key tex-map "q" #'tex-show-print-queue)
    (define-key tex-map "p" #'texinfo-tex-print)
    (define-key tex-map "v" #'texinfo-tex-view)
    (define-key tex-map "i" #'texinfo-texindex)
    (define-key tex-map "r" #'texinfo-tex-region)
    (define-key tex-map "b" #'texinfo-tex-buffer)

    (define-key map "m" makeinfo-map)
    (define-key makeinfo-map "k" #'kill-compilation)
    (define-key makeinfo-map "l" #'makeinfo-recenter-compilation-buffer)
    (define-key makeinfo-map "r" #'makeinfo-region)
    (define-key makeinfo-map "b" #'makeinfo-buffer)
    (define-key makeinfo-map "h" #'mb-texinfo-make-html)
    (define-key makeinfo-map "H" #'mb-texinfo-make-html)
    (define-key makeinfo-map "n" #'next-error)
    (define-key makeinfo-map "p" #'previous-error)

    (define-key map "e" texinfo-format-map)
    (define-key texinfo-format-map "r" #'texinfo-format-region)
    (define-key texinfo-format-map "b" #'texinfo-format-buffer)

    (define-key map "g" texinfo-goto-map)
    (define-key texinfo-goto-map "s" #'texinfo-goto-source)

    (when (fboundp 'key-chord-define)
      (when (and (boundp 'evil-normal-state-local-map)
                 (keymapp evil-normal-state-local-map))
        (key-chord-define evil-normal-state-local-map "gh" map))
      (when (and (boundp 'evil-visual-state-local-map)
                 (keymapp evil-visual-state-local-map))
        (key-chord-define evil-visual-state-local-map "gh" map)))
    map))

(add-hook 'texinfo-mode-hook #'mb-texinfo-map)

(defun mb-texinfo--call-process (program &rest arguments)
  "Run PROGRAM synchronously with ARGUMENTS and signal on failure."
  (with-temp-buffer
    (let ((status (apply #'call-process
                         program nil (current-buffer) nil arguments)))
      (unless (and (integerp status) (zerop status))
        (error "%s failed (%S): %s"
               program status (string-trim (buffer-string))))
      status)))

(defun mb-texinfo-make-html ()
  "Run Makeinfo's normal buffer command, then produce HTML."
  (interactive)
  (unless buffer-file-name
    (user-error "The current buffer is not visiting a file"))
  (makeinfo-buffer)
  (mb-texinfo--call-process "makeinfo" "--html" buffer-file-name))

(defun mb-texinfo-install-html ()
  "Copy the generated lsbin HTML directory to its historical destination."
  (interactive)
  (mb-texinfo--call-process
   "cp" "-r" "lsbin" "/ls/platinum/u1/mbe/html/"))

(defun texinfo-insert-newline (&optional n no-trailing-space-p)
  "Insert N Texinfo-commented newlines at the start of the current line.
Insert a space after each comment prefix unless
NO-TRAILING-SPACE-P is non-nil."
  (interactive "p")
  (beginning-of-line)
  (dotimes (_ (or n 1))
    (insert (format "##%s\n" (if no-trailing-space-p "" " ")))))

(defun texinfo-insert-env (environment
                           &optional no-space-p no-intraspace-p)
  "Insert a commented Texinfo ENVIRONMENT block.
Unless NO-SPACE-P is non-nil, surround the block with commented
blank lines.  Unless NO-INTRASPACE-P is non-nil, include a
commented content line and leave point at its end."
  (beginning-of-line)
  (unless no-space-p
    (insert "##\n"))
  (insert (format "## @%s\n" environment))
  (let ((content-point (point)))
    (unless no-intraspace-p
      (insert "##\n"))
    (insert (format "## @end %s\n" environment))
    (unless no-space-p
      (insert "##\n"))
    (goto-char content-point)
    (unless no-intraspace-p
      (end-of-line))))

(defun texinfo-insert-group-snippet (&optional no-space-p)
  "Insert a commented group block.
Do not add surrounding space when NO-SPACE-P is non-nil."
  (interactive)
  (texinfo-insert-env 'group no-space-p))

(defun texinfo-insert-example-snippet (&optional no-space-p)
  "Insert a commented example block.
Do not add surrounding space when NO-SPACE-P is non-nil."
  (interactive)
  (texinfo-insert-env 'example no-space-p))

(defun texinfo-insert-group/example-snippet (&optional no-space-p)
  "Insert a commented example block nested inside a group block.
Do not add surrounding space when NO-SPACE-P is non-nil."
  (interactive)
  (texinfo-insert-env 'group no-space-p t)
  (texinfo-insert-env 'example t))

(defun texinfo-goto-source ()
  "Find the tag named by the current or preceding @subsection."
  (interactive)
  (let ((tag
         (save-excursion
           (unless (re-search-backward "@subsection" nil t)
             (user-error "No preceding @subsection"))
           (forward-word 2)
           (thing-at-point 'symbol t))))
    (unless tag
      (user-error "The @subsection has no tag name"))
    (let ((tags-table-list (list (expand-file-name "~/.OTAGS"))))
      (xref-find-definitions tag))))

(provide 'mb-texinfo)

;;; mb-texinfo.el ends here
