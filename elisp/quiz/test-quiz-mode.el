;;; test-quiz-mode.el --- Tests for quiz major mode -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'quiz-mode)

(ert-deftest quiz-mode-defines-complete-font-lock-rules ()
  (dolist (prefix '("Q" "A" "S" "C" "E" "T" "D" "X"))
    (should
     (seq-some
      (lambda (rule)
        (string-match-p
         (car rule)
         (concat prefix "1: Item")))
      quiz-font-lock-keywords))))

(ert-deftest quiz-mode-derives-from-text-mode ()
  (with-temp-buffer
    (let ((quiz-mode-input-method nil))
      (quiz-mode))
    (should (derived-mode-p 'text-mode))
    (should (eq major-mode 'quiz-mode))
    (should (equal mode-name "Quiz"))
    (should buffer-offer-save)
    (should (eq fill-paragraph-function 'quiz-fill-item))
    (should
     (equal font-lock-defaults
            '(quiz-font-lock-keywords)))))

(ert-deftest quiz-mode-preserves-key-bindings ()
  (should
   (eq (lookup-key quiz-mode-map "\C-\\m")
       'quiz-mail-buffer))
  (should
   (eq (lookup-key quiz-mode-map "\C-\\ie")
       'quiz-insert-empty-question))
  (should
   (eq (lookup-key quiz-mode-map "\C-\\sp")
       'quiz-summary-print))
  (should
   (eq (lookup-key quiz-mode-map "\C-\\em")
       'quiz-move-active-region-to-a)))

(ert-deftest quiz-mode-configures-input-method ()
  (let (activated-input-method)
    (cl-letf
        (((symbol-function 'activate-input-method)
          (lambda (input-method)
            (setq activated-input-method input-method))))
      (with-temp-buffer
        (let ((quiz-mode-input-method "test-input-method"))
          (quiz-mode))))
    (should
     (equal activated-input-method
            "test-input-method"))))

(ert-deftest quiz-mode-registers-qz-files ()
  (should
   (eq
    (cdr (assoc "sample.qz" auto-mode-alist
                #'string-match-p))
    'quiz-mode)))

(provide 'test-quiz-mode)

;;; test-quiz-mode.el ends here
