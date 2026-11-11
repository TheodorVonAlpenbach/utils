;;; test-quiz-mail.el --- Tests for quiz mail composition -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'quiz-mail)

(ert-deftest quiz-mail-extracts-addresses ()
  (should
   (equal
    (quiz-mail-address
     "Example Person <person@example.com>")
    "person@example.com"))
  (should
   (equal
    (quiz-mail-address "person@example.com")
    "person@example.com")))

(ert-deftest quiz-mail-rejects-invalid-address ()
  (should-error
   (quiz-mail-address "not an address")
   :type 'user-error))

(ert-deftest quiz-mail-resolves-recipient-group ()
  (let ((quiz-default-mail-group "Test")
        (quiz-mail-groups
         '(("Test"
            ("One <one@example.com>"
             "two@example.com")))))
    (should
     (equal
      (quiz-mail-recipients)
      "one@example.com, two@example.com"))))

(ert-deftest quiz-mail-reports-invalid-groups ()
  (let ((quiz-mail-groups
         '(("Empty" nil))))
    (should-error
     (quiz-mail-recipients "Missing")
     :type 'user-error)
    (should-error
     (quiz-mail-recipients "Empty")
     :type 'user-error)))

(ert-deftest quiz-mail-composes-current-buffer ()
  (let ((quiz-default-mail-group "Test")
        (quiz-mail-groups
         '(("Test"
            ("one@example.com"))))
        (message-buffer
         (generate-new-buffer
          " *quiz-mail-test*"))
        recipients
        subject)
    (unwind-protect
        (with-temp-buffer
          (rename-buffer "Weekly quiz" t)
          (insert "Q1: Question?\nA1: Answer\n")
          (cl-letf
              (((symbol-function 'message-mail)
                (lambda (to message-subject
                         &rest _arguments)
                  (setq recipients to
                        subject message-subject)
                  (set-buffer message-buffer)))
               ((symbol-function 'message-goto-body)
                #'ignore))
            (quiz-mail-buffer))
          (should
           (equal recipients
                  "one@example.com"))
          (should
           (equal subject "Weekly quiz"))
          (with-current-buffer message-buffer
            (should
             (equal
              (buffer-string)
              "Q1: Question?\nA1: Answer\n"))))
      (kill-buffer message-buffer))))

(ert-deftest quiz-mail-refreshes-optional-address-data ()
  (let ((quiz-default-mail-group "Test")
        (quiz-mail-groups
         '(("Test"
            ("one@example.com"))))
        refreshed)
    (cl-letf
        (((symbol-function 'mb-refresh-addresses)
          (lambda ()
            (setq refreshed t)))
         ((symbol-function 'message-mail)
          (lambda (&rest _arguments)))
         ((symbol-function 'message-goto-body)
          #'ignore))
      (with-temp-buffer
        (quiz-mail-buffer t)))
    (should refreshed)))

(provide 'test-quiz-mail)

;;; test-quiz-mail.el ends here
