(defun mb-elisp-kbd-maps ()
  (let ((mb-local-map (make-sparse-keymap)))
    (key-chord-define evil-normal-state-local-map "gh" mb-local-map)
    (define-key mb-local-map "t" (mb-ert-map))))

(add-hook 'emacs-lisp-mode-hook #'mb-elisp-kbd-maps)

(defun cl-ify-defun ()
  "Insert prefix 'cl-' at the head symbol of the current defun form.
DEFUN forms include all top-level forms with a car symbol name having the prefix 'def'"
  (interactive)
  (save-excursion
    (bod)
    (re-search-forward "([[:space:]]*def" nil t)
    (bow)
    (insert "cl-")))

(defun cl-ify-form ()
  "Insert prefix 'cl-' at the head symbol of the current form."
  (interactive)
  (save-excursion
    (bof)
    (re-search-forward "([[:space:]]*" nil t)
    (insert "cl-")))

(cl-defun elisp-goto-defun (name &optional (defun-regexp +defun-regexp+)
				force-beginning-p)
  "Move point to the beginning of the defun NAME.
If point is already in the target defun, do not move the point
unless FORCE-BEGINNING-P is true. `defun' here means any
defun-ish construct like `cl-defun', `defmacro' etc."
  (message "%S %s" (buffer-name) (point))
  (unless (string= (sstring (defun-symbol)) name)
    (bob)
    (if (re-search-forward (format "^([[:space:]]*%s[[:space:]]+%s\\_>"
				 defun-regexp name)
	  nil t)
      (bod)
      ;; else issue warning message and return nil
      (message "Couldn't find a defun associated with %s" name)
      nil)))
;;(elisp-goto-defun "cl-ify-form")

(provide 'mb-emacs-lisp)

