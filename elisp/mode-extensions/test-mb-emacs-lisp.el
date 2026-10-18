;;; test-mb-emacs-lisp.el --- Tests for mb-emacs-lisp -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-emacs-lisp)

(ert-deftest mb-emacs-lisp-keymap-and-hook ()
  (should (memq #'mb-elisp-kbd-maps emacs-lisp-mode-hook))
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((map (mb-elisp-kbd-maps)))
      (should (keymapp map))
      (should (keymapp (lookup-key map "t"))))))

(ert-deftest mb-emacs-lisp-cl-ify-defun ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun alpha ()\n  t)\n")
    (goto-char (point-max))
    (cl-ify-defun)
    (should (equal (buffer-string) "(cl-defun alpha ()\n  t)\n"))
    (cl-ify-defun)
    (should (equal (buffer-string) "(cl-defun alpha ()\n  t)\n")))
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defvar alpha t)\n")
    (goto-char (point-max))
    (should-error (cl-ify-defun) :type 'user-error)))

(ert-deftest mb-emacs-lisp-cl-ify-form ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(loop for item in items\n      collect item)")
    (search-backward "items")
    (cl-ify-form)
    (should (equal (buffer-string)
                   "(cl-loop for item in items\n      collect item)"))))

(ert-deftest mb-emacs-lisp-goto-definition ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun alpha () 1)\n\n"
            "(cl-defun beta () 2)\n\n"
            "(defmacro gamma () nil)\n")
    (goto-char (point-max))
    (should (elisp-goto-defun 'beta))
    (should (looking-at "(cl-defun beta"))
    (search-forward "2")
    (let ((inside (point)))
      (should (= (elisp-goto-defun "beta") inside))
      (should (= (elisp-goto-defun "beta" nil t)
                 (line-beginning-position)))
      (should (looking-at "(cl-defun beta")))))

(ert-deftest mb-emacs-lisp-missing-definition-preserves-point ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun alpha () 1)\n")
    (goto-char 8)
    (let ((origin (point)))
      (should-not (elisp-goto-defun "missing"))
      (should (= (point) origin)))))

(provide 'test-mb-emacs-lisp)

;;; test-mb-emacs-lisp.el ends here
