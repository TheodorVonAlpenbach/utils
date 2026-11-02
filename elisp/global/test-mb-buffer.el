;;; test-mb-buffer.el --- Tests for mb-buffer -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mb-buffer)

(ert-deftest mb-buffer-inserts-in-current-buffer-without-moving-point ()
  (with-temp-buffer
    (insert "ac")
    (goto-char 2)
    (mb-buffer-insert-at-point "b")
    (should (equal (buffer-string) "abc"))
    (should (= (point) 2))))

(ert-deftest mb-buffer-inserts-at-target-buffer-point ()
  (let ((target (generate-new-buffer " *mb-buffer-target*")))
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "ac")
            (goto-char 2))
          (with-temp-buffer
            (goto-char (point-min))
            (mb-buffer-insert-at-point "b" nil target))
          (with-current-buffer target
            (should (equal (buffer-string) "abc"))
            (should (= (point) 2))))
      (kill-buffer target))))

(ert-deftest mb-buffer-replaces-region-without-changing-kill-ring ()
  (let ((kill-ring '("keep")))
    (with-temp-buffer
      (insert "before middle after")
      (goto-char 8)
      (mb-buffer-replace-region "center" 8 14)
      (should (equal (buffer-string) "before center after"))
      (should (= (point) 8))
      (should (equal kill-ring '("keep"))))))

(ert-deftest mb-buffer-sorts-characters-in-both-directions ()
  (with-temp-buffer
    (insert "dbca")
    (mb-buffer-sort-characters (point-min) (point-max))
    (should (equal (buffer-string) "abcd"))
    (mb-buffer-sort-characters (point-min) (point-max) t)
    (should (equal (buffer-string) "dcba"))))

(ert-deftest mb-buffer-indentation-command-uses-text-indentation ()
  (let (called)
    (cl-letf (((symbol-function 'back-to-indentation) #'ignore)
              ((symbol-function 'just-one-space) #'ignore)
              ((symbol-function 'indent-relative)
               (lambda () (setq called 'relative)))
              ((symbol-function 'indent-for-tab-command)
               (lambda () (setq called 'tab))))
      (let ((major-mode 'text-mode))
        (mb-buffer-back-to-indentation-and-indent)
        (should (eq called 'relative))))))

(ert-deftest mb-buffer-indentation-command-uses-mode-indentation ()
  (let (called)
    (cl-letf (((symbol-function 'back-to-indentation) #'ignore)
              ((symbol-function 'just-one-space) #'ignore)
              ((symbol-function 'indent-relative)
               (lambda () (setq called 'relative)))
              ((symbol-function 'indent-for-tab-command)
               (lambda () (setq called 'tab))))
      (let ((major-mode 'emacs-lisp-mode))
        (mb-buffer-back-to-indentation-and-indent)
        (should (eq called 'tab))))))

(ert-deftest mb-buffer-window-size-selects-dimension ()
  (cl-letf (((symbol-function 'window-width)
             (lambda (&optional window)
               (should (eq window 'test-window))
               80))
            ((symbol-function 'window-height)
             (lambda (&optional window)
               (should (eq window 'test-window))
               24)))
    (should (= (mb-buffer-window-size t 'test-window) 80))
    (should (= (mb-buffer-window-size nil 'test-window) 24))))

(ert-deftest mb-buffer-set-window-size-computes-resize-delta ()
  (let (arguments)
    (cl-letf (((symbol-function 'mb-buffer-window-size)
               (lambda (&optional horizontal _window)
                 (if horizontal 80 24)))
              ((symbol-function 'shrink-window)
               (lambda (delta &optional horizontal)
                 (setq arguments (list delta horizontal)))))
      (mb-buffer-set-window-size 12)
      (should (equal arguments '(12 nil)))
      (mb-buffer-set-window-size 50 t)
      (should (equal arguments '(30 t))))))

(ert-deftest mb-buffer-compatibility-aliases ()
  (dolist (pair
           '((back-to-indentation-and-indent
              . mb-buffer-back-to-indentation-and-indent)
             (insert-at-point . mb-buffer-insert-at-point)
             (buffer-replace-region . mb-buffer-replace-region)
             (sort-chars . mb-buffer-sort-characters)
             (get-window-size . mb-buffer-window-size)
             (set-window-size . mb-buffer-set-window-size)))
    (should
     (eq (indirect-function (car pair))
         (indirect-function (cdr pair))))))

(provide 'test-mb-buffer)

;;; test-mb-buffer.el ends here
