;;; test-mb-lilypond.el --- Tests for mb-lilypond -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'mb-lilypond)

(ert-deftest mb-lilypond-compile-records-quoted-source ()
  (let ((source "/tmp/score with spaces.ly")
        command
        (compilation-buffer (generate-new-buffer " *mb-lilypond-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'compilation-start)
                   (lambda (value)
                     (setq command value)
                     compilation-buffer)))
          (should (eq (mb-lilypond-compile source) compilation-buffer))
          (should (equal command
                         (format "lilypond %s"
                                 (shell-quote-argument source))))
          (should
           (equal
            (buffer-local-value 'mb-lilypond-source-file compilation-buffer)
            source)))
      (kill-buffer compilation-buffer))))

(ert-deftest mb-lilypond-compile-requires-source-file ()
  (with-temp-buffer
    (should-error (mb-lilypond-compile) :type 'user-error)))

(ert-deftest mb-lilypond-finish-displays-existing-pdf ()
  (let* ((directory (make-temp-file "mb-lilypond-" t))
         (source (expand-file-name "score.ly" directory))
         (pdf (expand-file-name "score.pdf" directory))
         (compilation-buffer (generate-new-buffer " *mb-lilypond-test*"))
         (pdf-buffer (generate-new-buffer " *mb-lilypond-pdf-test*"))
         displayed
         reverted)
    (unwind-protect
        (progn
          (with-temp-file pdf)
          (with-current-buffer compilation-buffer
            (setq-local mb-lilypond-source-file source))
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (file)
                       (setq displayed file)
                       pdf-buffer))
                    ((symbol-function 'revert-buffer)
                     (lambda (&rest _arguments)
                       (setq reverted t))))
            (mb-lilypond-compile-view-pdf compilation-buffer "finished\n")
            (should (equal displayed pdf))
            (should reverted)
            (setq displayed nil)
            (mb-lilypond-compile-view-pdf compilation-buffer "exited abnormally\n")
            (should-not displayed)))
      (kill-buffer compilation-buffer)
      (kill-buffer pdf-buffer)
      (delete-directory directory t))))

(ert-deftest mb-lilypond-command-advice-is-narrow ()
  (let ((file-function (lambda () "/tmp/score.ly"))
        opened
        delegated)
    (cl-letf (((symbol-function 'find-file)
               (lambda (file)
                 (setq opened file))))
      (mb-lilypond--command-around
       (lambda (&rest arguments)
         (setq delegated arguments))
       "View"
       file-function)
      (should (equal opened "/tmp/score.pdf"))
      (should-not delegated)
      (mb-lilypond--command-around
       (lambda (&rest arguments)
         (setq delegated arguments)
         'result)
       "LilyPond"
       file-function)
      (should (equal delegated (list "LilyPond" file-function))))))

(provide 'test-mb-lilypond)

;;; test-mb-lilypond.el ends here
