;;; mb-emacs-lisp.el --- Personal Emacs Lisp editing commands -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'elisp-mode)
(require 'mb-ert)

(declare-function key-chord-define "key-chord" (keymap keys command))
(declare-function mb-mysql-map "ada-mysql" ())
(defvar evil-normal-state-local-map)

(defconst mb-elisp-defun-regexp
  (regexp-opt
   (mapcar #'symbol-name
           '(defun cl-defun
             defmacro cl-defmacro
             defparameter defconstant
             ert-deftest)))
  "Regexp matching definition forms recognized by `elisp-goto-defun'.")

(defun mb-elisp-kbd-maps ()
  "Build and optionally install the personal Emacs Lisp prefix map.
The ERT submap is always present.  Add the project-specific MySQL
submap and Evil/key-chord binding only when those integrations are
already available.  Return the prefix map."
  (let ((map (make-sparse-keymap)))
    (define-key map "t" (mb-ert-map))
    (when (fboundp 'mb-mysql-map)
      (define-key map "q" (mb-mysql-map)))
    (when (and (fboundp 'key-chord-define)
               (boundp 'evil-normal-state-local-map)
               (keymapp evil-normal-state-local-map))
      (key-chord-define evil-normal-state-local-map "gh" map))
    map))

(add-hook 'emacs-lisp-mode-hook #'mb-elisp-kbd-maps)

(defun mb-elisp--form-head-position (form-start)
  "Return (POSITION . SYMBOL) for the form beginning at FORM-START."
  (save-excursion
    (goto-char form-start)
    (unless (eq (char-after) ?\()
      (user-error "Point is not at the beginning of a Lisp form"))
    (forward-char 1)
    (skip-syntax-forward " ")
    (let ((position (point))
          (symbol (symbol-at-point)))
      (unless symbol
        (user-error "The Lisp form has no head symbol"))
      (cons position symbol))))

(defun mb-elisp--cl-ify-at (form-start)
  "Prefix the head at FORM-START with `cl-'.
Do nothing when it already has that prefix.  Signal a user error
when Emacs has no corresponding `cl-' function or macro."
  (pcase-let ((`(,position . ,symbol)
               (mb-elisp--form-head-position form-start)))
    (let ((name (symbol-name symbol)))
      (unless (string-prefix-p "cl-" name)
        (let ((cl-symbol (intern (concat "cl-" name))))
          (unless (fboundp cl-symbol)
            (user-error "No cl-lib counterpart for `%s'" symbol))
          (goto-char position)
          (insert "cl-"))))))

(defun cl-ify-defun ()
  "Prefix the current top-level definition's head with `cl-'."
  (interactive)
  (save-excursion
    (unless (beginning-of-defun)
      (user-error "Point is not inside a top-level definition"))
    (mb-elisp--cl-ify-at (point))))

(defun cl-ify-form ()
  "Prefix the innermost containing form's head with `cl-'."
  (interactive)
  (save-excursion
    (let ((form-start
           (if (eq (char-after) ?\()
               (point)
             (nth 1 (syntax-ppss)))))
      (unless form-start
        (user-error "Point is not inside a Lisp form"))
      (mb-elisp--cl-ify-at form-start))))

(defun mb-elisp--current-definition-name ()
  "Return the name of the current recognized definition, or nil."
  (save-excursion
    (when (beginning-of-defun)
      (condition-case nil
          (let ((form (read (current-buffer))))
            (when (and (listp form)
                       (symbolp (car form))
                       (string-match-p
                        (format "\\`%s\\'" mb-elisp-defun-regexp)
                        (symbol-name (car form)))
                       (symbolp (cadr form)))
              (symbol-name (cadr form))))
        (error nil)))))

(cl-defun elisp-goto-defun
    (name &optional
          (defun-regexp mb-elisp-defun-regexp)
          force-beginning-p)
  "Move to the definition of NAME in the current buffer.
DEFUN-REGEXP selects recognized definition heads.  If point is
already inside NAME, leave it unchanged unless FORCE-BEGINNING-P
is non-nil.  Return point when found and nil otherwise."
  (setq name (format "%s" name))
  (setq defun-regexp (or defun-regexp mb-elisp-defun-regexp))
  (if (and (not force-beginning-p)
           (equal (mb-elisp--current-definition-name) name))
      (point)
    (let ((position
           (save-excursion
             (goto-char (point-min))
             (when (re-search-forward
                    (format
                     "^[[:space:]]*(%s[[:space:]]+%s\\_>"
                     defun-regexp
                     (regexp-quote name))
                    nil t)
               (line-beginning-position)))))
      (if position
          (goto-char position)
        (message "Couldn't find a definition associated with %s" name)
        nil))))

(provide 'mb-emacs-lisp)

;;; mb-emacs-lisp.el ends here
