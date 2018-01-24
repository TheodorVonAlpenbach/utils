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

(defun forward-ert-test ()
  (re-search-forward "^[[:space:]]*([[:space:]]*ert-deftest[[:space:]]*" nil t))

(cl-defun elisp-test-buffer-path (buffer)
  "Return the path to the BUFFER's test module.
The function assumes that if BUFFER's path is \"path/buffer-name.el\", then
the associated test buffer's path is \"path/test-buffer-name.el\"."
  (let ((name (buffer-name)))
    (if (string-match "^test-.*\.el$" name)
      (buffer-file-name buffer)
      (expand-file-name (concat "test-" (buffer-name buffer))
			(buffer-directory buffer)))))
;;(elisp-test-buffer-path (get-buffer "test-mb-utils-strings.el"))
;;(elisp-test-buffer-path (get-buffer "mb-utils-strings.el"))

(defun elisp-test-symbols (filename)
  "Return a list of all test function symbols in FILENAME."
  (with-temp-buffer
    (insert-file filename)
    (loop while (forward-ert-test)
	  collect (ert-test-at-point))))
;;(elisp-test-symbols "~/projects/utils/elisp/utils/test-mb-utils-strings.el")

(defun elisp-test (buffer selector)
  (load (elisp-test-buffer-path buffer))
  (ert selector))

(cl-defun elisp-test-buffer (&optional (buffer (current-buffer)))
  (interactive)
  (let ((test-path (elisp-test-buffer-path buffer)))
    (aif (get-buffer (file-name-nondirectory test-path))
      (eval-buffer it)
      (load test-path))
    (ert (cons 'member (elisp-test-symbols test-path)))))
;;(elisp-test-buffer (get-buffer "mb-utils-div.el"))
(buffer-live-p "~/projects/utils/elisp/mode-extensions/mb-octave.el")

(buffer-live-p (get-buffer "mb-emacs-lisp.el"))

(cl-defun elisp-test-all (&optional (buffer (current-buffer)))
  "Run `ert' on all mb-elisp tests that exists.
Currently it only tests the test functions that have been
evaluated or compiled."
  (interactive)
  ;; TODO load all test buffers here
  (ert t))
;;(elisp-test-all (get-buffer "mb-utils-div.el"))

(provide 'mb-emacs-lisp)
