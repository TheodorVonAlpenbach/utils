;;; test-quiz-config.el --- Tests for quiz configuration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'quiz-config)

(ert-deftest quiz-config-builds-file-paths ()
  (let ((quiz-directory "/tmp/quiz/"))
    (should
     (equal
      (quiz-file-path "sample.qz")
      "/tmp/quiz/sample.qz"))
    (should
     (equal
      (quiz-file-path "sample.qz" "/tmp/other/")
      "/tmp/other/sample.qz"))))

(ert-deftest quiz-config-builds-dated-file-paths ()
  (let ((quiz-directory "/tmp/quiz/"))
    (should
     (equal
      (quiz-get-file "2005-02-22")
      "/tmp/quiz/mq-2005-02-22.qz"))))

(ert-deftest quiz-config-uses-current-date-by-default ()
  (cl-letf
      (((symbol-function 'format-time-string)
        (lambda (&rest _arguments)
          "2026-11-11")))
    (let ((quiz-directory "/tmp/quiz/"))
      (should
       (equal
        (quiz-get-file)
        "/tmp/quiz/mq-2026-11-11.qz")))))

(ert-deftest quiz-config-reuses-existing-buffer ()
  (with-temp-buffer
    (rename-buffer "existing-quiz.qz" t)
    (let ((quiz-mode-input-method nil))
      (should
       (eq
        (quiz-get-buffer "existing-quiz.qz")
        (current-buffer)))
      (should (derived-mode-p 'quiz-mode)))))

(ert-deftest quiz-config-visits-file-in-quiz-mode ()
  (let* ((directory
          (make-temp-name
           (expand-file-name
            "quiz-config-test-"
            temporary-file-directory)))
         (quiz-directory
          (file-name-as-directory directory))
         (quiz-mode-input-method nil)
         buffer)
    (unwind-protect
        (progn
          (setq buffer
                (quiz-get-buffer "sample.qz"))
          (should
           (equal
            (buffer-file-name buffer)
            (expand-file-name
             "sample.qz" directory)))
          (with-current-buffer buffer
            (should (derived-mode-p 'quiz-mode))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest quiz-config-retains-legacy-variable-aliases ()
  (let ((quiz-directory "/tmp/quiz/")
        (quiz-scratch-buffer-name "notes.qz"))
    (should (equal *quiz-dir* "/tmp/quiz/"))
    (should
     (equal
      *quiz-scratch-buffername*
      "notes.qz"))))

(provide 'test-quiz-config)

;;; test-quiz-config.el ends here
