;;; test-mb-metafont.el --- Tests for mb-metafont -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'mb-metafont)

(ert-deftest mb-metafont-parses-compilation-output ()
  (with-temp-buffer
    (insert "This is METAFONT, Version 2.71828182\n"
            "Output written on score.600gf (128 characters).\n")
    (should (equal (meta-check-compilation (current-buffer))
                   "score.600gf")))
  (with-temp-buffer
    (insert "mf: command not found\n")
    (should-error (meta-check-compilation (current-buffer)))))

(ert-deftest mb-metafont-compile-clears-output-and-checks-status ()
  (let ((meta-compilation-buffer-name " *mb-metafont-test*")
        process-directory
        process-argument
        displayed)
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create meta-compilation-buffer-name)
            (insert "stale output"))
          (cl-letf (((symbol-function 'call-process)
                     (lambda (_program _input destination _display &rest arguments)
                       (setq process-directory default-directory
                             process-argument (car arguments))
                       (with-current-buffer destination
                         (insert "This is METAFONT\n"
                                 "Output written on score.600gf (1 character).\n"))
                       0))
                    ((symbol-function 'display-buffer)
                     (lambda (buffer)
                       (setq displayed buffer))))
            (should (equal (meta-compile-file "/tmp/score.mf" t)
                           "score.600gf"))
            (should (equal process-directory "/tmp/"))
            (should (equal process-argument "score.mf"))
            (should (eq displayed (get-buffer meta-compilation-buffer-name))))
          (cl-letf (((symbol-function 'call-process)
                     (lambda (&rest _arguments) 1)))
            (should-error (meta-compile-file "/tmp/score.mf"))))
      (when-let ((buffer (get-buffer meta-compilation-buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest mb-metafont-gftodvi-uses-source-directory ()
  (let (program directory argument)
    (cl-letf (((symbol-function 'call-process)
               (lambda (value _input _destination _display &rest arguments)
                 (setq program value
                       directory default-directory
                       argument (car arguments))
                 0)))
      (should (equal (gftodvi "/tmp/fonts/score.600gf")
                     "/tmp/fonts/score.dvi"))
      (should (equal program "gftodvi"))
      (should (equal directory "/tmp/fonts/"))
      (should (equal argument "score.600gf")))))

(ert-deftest mb-metafont-eval-buffer-runs-pipeline ()
  (let ((source-buffer (generate-new-buffer " *mb-metafont-source*"))
        (dvi-buffer (generate-new-buffer " *mb-metafont-dvi*"))
        compiled
        converted
        displayed
        reverted)
    (unwind-protect
        (with-current-buffer source-buffer
          (setq buffer-file-name "/tmp/fonts/score.mf")
          (cl-letf (((symbol-function 'save-buffer) #'ignore)
                    ((symbol-function 'meta-compile-file)
                     (lambda (filename &optional show-buffer-p)
                       (setq compiled filename)
                       (should-not show-buffer-p)
                       "score.600gf"))
                    ((symbol-function 'gftodvi)
                     (lambda (filename)
                       (setq converted filename)
                       "/tmp/fonts/score.dvi"))
                    ((symbol-function 'find-file-other-window)
                     (lambda (filename)
                       (setq displayed filename)
                       dvi-buffer))
                    ((symbol-function 'revert-buffer)
                     (lambda (&rest _arguments)
                       (setq reverted t))))
            (should (eq (meta-eval-buffer) dvi-buffer))
            (should (equal compiled "/tmp/fonts/score.mf"))
            (should (equal converted "/tmp/fonts/score.600gf"))
            (should (equal displayed "/tmp/fonts/score.dvi"))
            (should reverted)))
      (kill-buffer source-buffer)
      (kill-buffer dvi-buffer))))

(provide 'test-mb-metafont)

;;; test-mb-metafont.el ends here
