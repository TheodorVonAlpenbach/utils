;;; test-mb-doc-view.el --- Tests for mb-doc-view -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-doc-view)

(ert-deftest mb-doc-view-preserves-standard-goto-page ()
  (should (commandp 'doc-view-goto-page))
  (should (equal (help-function-arglist #'doc-view-goto-page t)
                 '(page))))

(ert-deftest mb-doc-view-document-navigation ()
  (let (calls)
    (cl-letf (((symbol-function 'doc-view-last-page)
               (lambda () (push 'last-page calls)))
              ((symbol-function 'image-eob)
               (lambda () (push 'image-end calls)))
              ((symbol-function 'doc-view-first-page)
               (lambda () (push 'first-page calls)))
              ((symbol-function 'image-bob)
               (lambda () (push 'image-beginning calls))))
      (doc-view-end-of-document)
      (should (equal (nreverse calls) '(last-page image-end)))
      (setq calls nil)
      (doc-view-beginning-of-document)
      (should (equal (nreverse calls)
                     '(first-page image-beginning))))))

(ert-deftest mb-doc-view-keybindings ()
  (should (eq (lookup-key doc-view-mode-map (kbd "C-M->"))
              #'doc-view-end-of-document))
  (should (eq (lookup-key doc-view-mode-map (kbd "C-M-<"))
              #'doc-view-beginning-of-document)))

(provide 'test-mb-doc-view)

;;; test-mb-doc-view.el ends here
