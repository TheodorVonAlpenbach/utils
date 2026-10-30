;;; test-mb-octave.el --- Tests for mb-octave -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'mb-octave)

(ert-deftest mb-octave-mode-setup-is-buffer-local ()
  (let ((global-map-binding (lookup-key octave-mode-map (kbd "M-q"))))
    (with-temp-buffer
      (octave-mode)
      (should (bound-and-true-p display-line-numbers-mode))
      (should (bound-and-true-p eldoc-mode))
      (should (eq (char-syntax ?_) ?_))
      (should (eq (local-key-binding (kbd "M-q"))
                  #'mb-octave-fill-paragraph))
      (should (eq (local-key-binding (kbd "C-c i @"))
                  #'mb-texinfo-insert-@var)))
    (should (equal (lookup-key octave-mode-map (kbd "M-q"))
                   global-map-binding))))

(ert-deftest mb-octave-detects-mode-buffers ()
  (let ((octave-buffer (generate-new-buffer " *mb-octave-mode*"))
        (text-buffer (generate-new-buffer " *mb-octave-text*")))
    (unwind-protect
        (progn
          (with-current-buffer octave-buffer
            (octave-mode))
          (should (octave-mode-p octave-buffer))
          (should-not (octave-mode-p text-buffer))
          (should (memq octave-buffer (octave-buffers)))
          (should-not (memq text-buffer (octave-buffers))))
      (kill-buffer octave-buffer)
      (kill-buffer text-buffer))))

(ert-deftest mb-octave-parses-function-definitions ()
  (with-temp-buffer
    (insert (concat "function y = foo (x)\n"
                    "  y = x + 1;\n"
                    "endfunction\n"))
    (octave-mode)
    (goto-char (point-min))
    (should (equal (octave-main-defun-name) "foo"))
    (should (equal (octave-defun-name) "foo"))
    (should
     (equal (octave-main-defun-string)
            (string-trim-right (buffer-string)))))
  (should
   (equal (octave-parse-defun-header
           "function [a, b] = calculate (x)")
          '("function" " [a, b] = " "calculate")))
  (should
   (equal (octave-parse-defun-output-parameters
           "function [a, b] = calculate (x)")
          "[a, b] ="))
  (should
   (equal (octave-parse-defun-name
           "function [a, b] = calculate (x)")
          "calculate")))

(ert-deftest mb-octave-test-buffer-file-quotes-paths ()
  (let* ((directory (make-temp-file "mb-octave-" t))
         (buffer (generate-new-buffer " *mb-octave-test-file*"))
         (source (expand-file-name "quoted \"name\".m" directory))
         (startup (expand-file-name ".octaverc" directory))
         sent)
    (unwind-protect
        (progn
          (with-temp-file startup)
          (with-current-buffer buffer
            (setq buffer-file-name source)
            (cl-letf (((symbol-function 'octave-send-string)
                       (lambda (string &optional print-repl-p)
                         (push (list string print-repl-p) sent))))
              (mb-octave-test-buffer-file)))
          (should
           (equal
            (nreverse sent)
            (list
             (list
              (format "source (%s);" (mb-octave--quote-string startup))
              t)
             (list
              (format "test (%s);" (mb-octave--quote-string source))
              t)))))
      (kill-buffer buffer)
      (delete-directory directory t))))

(ert-deftest mb-octave-directory-runner-captures-options ()
  (let* ((directory (make-temp-file "mb-octave-" t))
         (runner (mb-octave-test-directory-fn directory t nil))
         sent)
    (unwind-protect
        (cl-letf (((symbol-function 'octave-send-string)
                   (lambda (string &optional print-repl-p)
                     (setq sent (list string print-repl-p)))))
          (funcall runner '(4))
          (should
           (equal sent
                  (list
                   (format "mbruntests %s 1 1"
                           (mb-octave--quote-string
                            (expand-file-name directory)))
                   t))))
      (delete-directory directory))))

(ert-deftest mb-octave-converts-seealso-lines ()
  (should
   (equal
    (filter-octave-texinfo-line
     "@seealso{alpha, missing, beta}" '("alpha" "beta"))
    "See also @ref{alpha} and @ref{beta} for related topics."))
  (should-error
   (filter-octave-texinfo-line "@seealso{missing}" '("alpha"))))

(ert-deftest mb-octave-converts-texinfo-files ()
  (let ((file (make-temp-file "mb-octave-" nil ".m")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (concat "## -*- texinfo -*-\n"
                            "## Summary.\n"
                            "## @seealso{alpha}\n"
                            "function y = alpha (x)\n")))
          (should
           (equal
            (mb-octave2texinfo-1 file "alpha" '("alpha"))
            (concat "@node alpha\n"
                    "@subsection alpha\n"
                    "@cindex subsection, alpha\n\n"
                    "Summary.\n"
                    "See also @ref{alpha} for related topics."))))
      (delete-file file))))

(provide 'test-mb-octave)

;;; test-mb-octave.el ends here
