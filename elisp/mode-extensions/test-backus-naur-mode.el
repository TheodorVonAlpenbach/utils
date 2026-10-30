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

(ert-deftest backus-naur-mode-search-preserves-unrelated-overlays ()
  (with-temp-buffer
    (insert "<expr> ::= term expr\n"
            "<term> ::= expr\n")
    (bn-mode)
    (goto-char (point-min))
    (search-forward "expr")
    (backward-char)
    (let* ((positions (get-text-property (1- (point)) 'position))
           (unrelated (make-overlay (point-min) (1+ (point-min))))
           (keys '("n" "q")))
      (overlay-put unrelated 'category 'unrelated)
      (cl-letf (((symbol-function 'read-key-sequence)
                 (lambda (&rest _args) (pop keys))))
        (bn-search))
      (should (= (point) (ring-ref positions -1)))
      (should (overlay-buffer unrelated))
      (should-not bn-search-overlays)
      (delete-overlay unrelated))))

(ert-deftest backus-naur-mode-search-cleans-up-on-quit ()
  (with-temp-buffer
    (insert "<expr> ::= expr\n")
    (bn-mode)
    (goto-char (point-min))
    (search-forward "expr")
    (backward-char)
    (let ((unrelated (make-overlay (point-min) (1+ (point-min)))))
      (cl-letf (((symbol-function 'read-key-sequence)
                 (lambda (&rest _args) (signal 'quit nil))))
        (should
         (condition-case nil
             (progn (bn-search) nil)
           (quit t))))
      (should (overlay-buffer unrelated))
      (should-not bn-search-overlays)
      (delete-overlay unrelated))))

(ert-deftest backus-naur-mode-refreshes-navigation-properties ()
  (with-temp-buffer
    (insert "<expr> ::= term\n"
            "<term> ::= 'x'\n")
    (bn-mode)
    (goto-char (point-min))
    (search-forward "term" (line-end-position))
    (should (eq (get-text-property (1- (point)) 'type) 'right))
    (replace-match "nope")
    (bn-lazy-font-lock)
    (goto-char (point-min))
    (search-forward "nope")
    (should-not (get-text-property (1- (point)) 'type))
    (should-error (bn-search) :type 'user-error)))

(ert-deftest backus-naur-mode-exposes-named-key-commands ()
  (should (eq (lookup-key bn-mode-map [tab]) #'bn-indent))
  (should (eq (lookup-key bn-mode-map [home]) #'bn-beginning-or-line))
  (should (eq (lookup-key bn-mode-map [end]) #'bn-end-or-line))
  (should (eq (lookup-key bn-mode-map (kbd "C-x SPC"))
              #'bn-jump-to-definition-at-point))
  (should (eq (lookup-key bn-mode-map (kbd "C-;"))
              #'bn-completing-search)))

(ert-deftest backus-naur-mode-completion-jumps-to-definition ()
  (with-temp-buffer
    (insert "<expr> ::= term\n"
            "<term> ::= 'x'\n")
    (bn-mode)
    (let* ((definitions (mapcar #'car bn-left-nonterminal-list))
           (target (cdr (assoc-string "term" definitions)))
           observed-history)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection _predicate _require-match
                                  _initial-input history &rest _args)
                   (should (equal collection definitions))
                   (setq observed-history history)
                   "term")))
        (bn-completing-search))
      (should (eq observed-history 'bn-search-history))
      (should (= (point) target)))))

(ert-deftest backus-naur-mode-jumps-from-right-nonterminal ()
  (with-temp-buffer
    (insert "<expr> ::= term\n"
            "<term> ::= 'x'\n")
    (bn-mode)
    (let* ((definitions (mapcar #'car bn-left-nonterminal-list))
           (target (cdr (assoc-string "term" definitions))))
      (goto-char (point-min))
      (search-forward "term" (line-end-position))
      (backward-char)
      (bn-jump-to-definition-at-point)
      (should (= (point) target))
      (should-error (bn-jump-to-definition-at-point)
                    :type 'user-error))))

(provide 'test-backus-naur-mode)

;;; test-backus-naur-mode.el ends here
