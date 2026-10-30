;;; test-global-map.el --- Tests for global-map -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'global-map)

(ert-deftest mb-global-map-load-has-no-global-binding-side-effect ()
  (should-not (lookup-key global-map (kbd "C-c p")))
  (should (fboundp 'mb-global-map-setup)))

(ert-deftest mb-global-map-setup-installs-retained-bindings ()
  (let ((map (make-sparse-keymap))
        (lisp-mode-shared-map (make-sparse-keymap))
        (Info-mode-map (make-sparse-keymap)))
    (should (eq (mb-global-map-setup map) map))
    (should (eq (lookup-key map (kbd "C-c p")) mb-global-print-map))
    (should (eq (lookup-key map (kbd "C-c i")) mb-global-insert-map))
    (should (eq (lookup-key map (kbd "C-x 8 g")) mb-global-greek-map))
    (should (eq (lookup-key map (kbd "C-c f")) mb-global-find-map))
    (should (eq (lookup-key map (kbd "<f11>"))
                #'mb-global-map-switch-to-other-buffer))
    (should (eq (lookup-key map (kbd "C-+"))
                #'mb-global-map-increment-1))
    (should-not (lookup-key map (kbd "<f12>")))
    (should (eq (lookup-key lisp-mode-shared-map (kbd "C-c e"))
                #'eval-buffer))
    (should (eq (lookup-key Info-mode-map (kbd "M-n"))
                #'forward-paragraph))
    (mb-global-map-setup map)
    (should (eq (lookup-key map (kbd "C-c p")) mb-global-print-map))))

(ert-deftest mb-global-map-prefix-aliases-and-insertions ()
  (let ((map (make-sparse-keymap))
        (lisp-mode-shared-map (make-sparse-keymap))
        (Info-mode-map (make-sparse-keymap)))
    (mb-global-map-setup map)
    (should (eq print-map mb-global-print-map))
    (should (eq insert-map mb-global-insert-map))
    (should (eq greek-map mb-global-greek-map))
    (should (eq find-map mb-global-find-map))
    (with-temp-buffer
      (call-interactively (lookup-key mb-global-insert-map "1"))
      (call-interactively (lookup-key mb-global-greek-map "a"))
      (should (equal (buffer-string) "¹α")))))

(ert-deftest mb-global-map-gfind-quotes-command-arguments ()
  (let (command)
    (cl-letf (((symbol-function 'compile)
               (lambda (value) (setq command value))))
      (gfind-compile "two words" "*.el" 3))
    (should (equal command "gfind -u 3 two\\ words \\*.el"))))

(provide 'test-global-map)

;;; test-global-map.el ends here
