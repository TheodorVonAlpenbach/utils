;;; test-mb-evil.el --- Tests for personal Evil integration -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mb-evil)

(ert-deftest mb-evil-strip-ssh ()
  (should
   (equal (strip-ssh "/home/Mats/git/utils/elisp/mode-extensions")
	  "/home/Mats/git/utils/elisp/mode-extensions"))
  (should
   (equal (strip-ssh "/ssh:pf:/home/mats_progfab_no/git/problem-server/")
	  "/home/mats_progfab_no/git/problem-server/")))

(ert-deftest mb-evil-setup-is-idempotent ()
  (let ((mb-evil--setup-complete t))
    (cl-letf (((symbol-function 'evil-mode)
               (lambda (&rest _args)
                 (ert-fail "Repeated setup enabled Evil again"))))
      (should (mb-evil-setup)))))

(ert-deftest mb-evil-configures-open-lisp-buffers-without-reverting ()
  (with-temp-buffer
    (insert "unsaved")
    (set-buffer-modified-p t)
    (let ((buffer (current-buffer))
          (+emacs-local+ "/tmp/emacs-local")
          configured)
      (cl-letf (((symbol-function 'get-buffer)
                 (lambda (_name) buffer))
                ((symbol-function 'get-file-buffer)
                 (lambda (_file) nil))
                ((symbol-function 'set-mb-lisp-locals)
                 (lambda () (setq configured (current-buffer)))))
        (mb-evil--configure-open-lisp-buffers))
      (should (eq configured buffer))
      (should (string= (buffer-string) "unsaved"))
      (should (buffer-modified-p)))))

(ert-deftest mb-evil-ffap-advice-is-removed-after-error ()
  (cl-letf (((symbol-function 'ffap)
             (lambda (&optional _filename)
               (error "Expected FFAP failure"))))
    (should-error (ffap-no-prompt "/missing"))
    (should-not
     (advice-member-p #'ffap-read-file-or-url-no-prompt
                      #'ffap-read-file-or-url))))

(ert-deftest mb-evil-surrounds-regions-with-repeated-delimiters ()
  (with-temp-buffer
    (insert "value")
    (mb-surround-region '(1 6) "[" 2)
    (should (string= (buffer-string) "[[value]]"))))

(ert-deftest mb-evil-eval-from-point-has-no-region-interactive-arguments ()
  (with-temp-buffer
    (insert "first\nsecond\n")
    (goto-char 8)
    (let (arguments)
      (cl-letf (((symbol-function 'mb-eval-region)
                 (lambda (&rest args) (setq arguments args))))
        (call-interactively #'mb-eval-region-from-point))
      (should (equal arguments (list 7 (point-max) nil nil))))))

(provide 'test-mb-evil)

;;; test-mb-evil.el ends here
