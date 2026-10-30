;;; test-backus-naur-mode.el --- Tests for backus-naur-mode -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'backus-naur-mode)

(ert-deftest backus-naur-mode-sets-buffer-local-state ()
  (let ((hook-calls 0))
    (let ((bn-mode-hook (list (lambda () (cl-incf hook-calls)))))
      (with-temp-buffer
        (bn-mode)
        (should (eq major-mode 'bn-mode))
        (should (equal mode-name "Backus-Naur"))
        (should (= tab-width bn-tab-width))
        (should (eq indent-region-function #'bn-indent-region))
        (should (memq #'bn-change after-change-functions))
        (should (memq #'bn-stop-timer kill-buffer-hook))
        (should (= hook-calls 1))
        (should (eq (indirect-function 'backus-naur-mode)
                    (symbol-function 'bn-mode)))))))

(ert-deftest backus-naur-mode-indexes-and-links-nonterminals ()
  (with-temp-buffer
    (insert "<expr> ::= term\n"
            "<term> ::= 'x'\n")
    (set-buffer-modified-p nil)
    (bn-mode)
    (should-not (buffer-modified-p))
    (let ((index (bn-imenu)))
      (should (equal (mapcar #'car index) '("expr" "term")))
      (should (cl-every (lambda (entry) (markerp (cdr entry))) index)))
    (goto-char (point-min))
    (search-forward "expr")
    (should (eq (get-text-property (1- (point)) 'type) 'left))
    (goto-char (point-min))
    (search-forward "term" (line-end-position))
    (should (eq (get-text-property (1- (point)) 'type) 'right))
    (should (equal (get-text-property (1- (point)) 'nonterminal)
                   "term"))))

(ert-deftest backus-naur-mode-identifies-comments ()
  (with-temp-buffer
    (insert "% comment\n<expr> ::= 'x'\n")
    (goto-char (point-min))
    (should (bn-commented-line-p))
    (forward-line 1)
    (should-not (bn-commented-line-p))))

(ert-deftest backus-naur-mode-aligns-definitions ()
  (with-temp-buffer
    (insert "<a> ::= first\n"
            "<long-name> ::= second\n"
            "  continuation\n")
    (bn-mode)
    (bn-indent-region (point-min) (point-max))
    (should
     (equal
      (buffer-substring-no-properties (point-min) (point-max))
      (concat "<a>         ::= first\n"
              "<long-name> ::= second\n"
              "               continuation\n")))))

(ert-deftest backus-naur-mode-idle-fontification-keeps-origin-buffer ()
  (let (callback fontified-buffer cancelled-timer)
    (with-temp-buffer
      (bn-mode)
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (_seconds _repeat function)
                   (setq callback function)
                   :timer))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (setq cancelled-timer timer)))
                ((symbol-function 'bn-lazy-font-lock)
                 (lambda ()
                   (setq fontified-buffer (current-buffer)))))
        (bn-change 1 1 0)
        (should (eq bn-timer-var :timer))
        (with-temp-buffer
          (funcall callback))
        (should (eq fontified-buffer (current-buffer)))
        (bn-stop-timer)
        (should (eq cancelled-timer :timer))
        (should-not bn-timer-var)))))

(provide 'test-backus-naur-mode)

;;; test-backus-naur-mode.el ends here
