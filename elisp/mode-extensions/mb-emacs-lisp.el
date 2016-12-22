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

(require 'mb-emacs-lisp)
