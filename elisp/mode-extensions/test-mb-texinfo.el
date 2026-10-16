;;; test-mb-texinfo.el --- Tests for mb-texinfo -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-texinfo)

(ert-deftest mb-texinfo-command-formatting ()
  (should (equal (texinfo-atfiy "foo") "@foo{}"))
  (should (equal (texinfo-atfiy "foo" "bar") "@foo{bar}")))

(ert-deftest mb-texinfo-command-insertion ()
  (with-temp-buffer
    (insert "alpha")
    (goto-char 3)
    (texinfo-insert-@ 'code)
    (should (equal (buffer-string) "@code{alpha}")))
  (with-temp-buffer
    (insert "alpha")
    (goto-char (point-min))
    (set-mark (point-max))
    (activate-mark)
    (texinfo-insert-@ "var")
    (should (equal (buffer-string) "@var{alpha}")))
  (with-temp-buffer
    (texinfo-insert-@ "xref")
    (should (equal (buffer-string) "@xref{}"))
    (should (= (point) (1- (point-max))))))

(ert-deftest mb-texinfo-generated-insertion-command ()
  (should (commandp 'mb-texinfo-insert-@var))
  (should (equal (help-function-arglist #'texinfo-insert-@var t)
                 '(&optional str arg)))
  (with-temp-buffer
    (insert "name")
    (goto-char 2)
    (call-interactively #'mb-texinfo-insert-@var)
    (should (equal (buffer-string) "@var{name}"))))

(ert-deftest mb-texinfo-commented-snippets ()
  (with-temp-buffer
    (texinfo-insert-env 'example)
    (should (equal (buffer-string)
                   "##\n## @example\n##\n## @end example\n##\n"))
    (should (= (line-number-at-pos) 3)))
  (with-temp-buffer
    (texinfo-insert-group/example-snippet)
    (should (equal (buffer-string)
                   (concat "##\n"
                           "## @group\n"
                           "## @example\n"
                           "##\n"
                           "## @end example\n"
                           "## @end group\n"
                           "##\n")))
    (should (= (line-number-at-pos) 4))))

(ert-deftest mb-texinfo-keymap-and-hook ()
  (should (memq #'mb-texinfo-map texinfo-mode-hook))
  (should-not (boundp 'Texinfo-mode-hook))
  (let ((map (mb-texinfo-map)))
    (should (keymapp map))
    (should (eq (lookup-key map "o") #'texinfo-insert-block))
    (should (eq (lookup-key (lookup-key map "r") "x")
                #'mb-texinfo-insert-@xref)))
  (with-temp-buffer
    (texinfo-mode)
    (should (eq major-mode 'texinfo-mode))))

(provide 'test-mb-texinfo)

;;; test-mb-texinfo.el ends here
