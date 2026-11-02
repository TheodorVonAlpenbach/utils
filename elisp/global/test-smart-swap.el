;;; test-smart-swap.el --- Tests for smart-swap -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'smart-swap)

(defun mb-smart-swap-test--touch (file)
  "Create FILE and its parent directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file))

(ert-deftest mb-smart-swap-substitutes-extensions ()
  (should
   (equal (mb-smart-swap-substitute-extension "/tmp/report.tex" 'pdf)
          "/tmp/report.pdf"))
  (should-error
   (mb-smart-swap-substitute-extension nil 'pdf)
   :type 'user-error))

(ert-deftest mb-smart-swap-expands-regexp-target ()
  (let ((default-directory "/tmp/"))
    (with-temp-buffer
      (insert "output: generated/result.pdf")
      (should
       (equal
        (mb-smart-swap-expand-target '("output: \\(.+\\.pdf\\)" 1))
        "/tmp/generated/result.pdf")))))

(ert-deftest mb-smart-swap-rejects-invalid-targets ()
  (should-error
   (mb-smart-swap-expand-target '(not-a-regexp))
   :type 'user-error)
  (should-error
   (mb-smart-swap-expand-target 42)
   :type 'user-error)
  (should-error
   (mb-smart-swap-find-target '(invalid-rule))
   :type 'user-error))

(ert-deftest mb-smart-swap-matches-files-and-nonfile-buffers ()
  (let ((buffer-file-name "/tmp/example.cpp"))
    (should (mb-smart-swap-match-current-buffer-p 'cpp))
    (should-not (mb-smart-swap-match-current-buffer-p 'h)))
  (with-temp-buffer
    (rename-buffer "Lilypond-compile" t)
    (should
     (mb-smart-swap-match-current-buffer-p "Lilypond-compile"))))

(ert-deftest mb-smart-swap-finds-extension-target-in-both-directions ()
  (let* ((directory (make-temp-file "smart-swap-target-" t))
         (source (expand-file-name "example.cpp" directory))
         (header (expand-file-name "example.h" directory)))
    (unwind-protect
        (progn
          (mb-smart-swap-test--touch source)
          (mb-smart-swap-test--touch header)
          (let ((buffer-file-name source))
            (should
             (equal (mb-smart-swap-find-target '((cpp h)))
                    header)))
          (let ((buffer-file-name header))
            (should
             (equal (mb-smart-swap-find-target '((cpp h)))
                    source))))
      (delete-directory directory t))))

(ert-deftest mb-smart-swap-requires-existing-derived-target ()
  (let ((buffer-file-name "/tmp/no-such-source.tex"))
    (should-not
     (mb-smart-swap-find-target '((tex pdf))))))

(ert-deftest mb-smart-swap-extracts-target-from-buffer-output ()
  (let* ((directory (make-temp-file "smart-swap-output-" t))
         (target (expand-file-name "score.pdf" directory))
         (default-directory directory))
    (unwind-protect
        (progn
          (mb-smart-swap-test--touch target)
          (with-temp-buffer
            (setq default-directory directory)
            (rename-buffer "Lilypond-compile" t)
            (insert "created score.pdf")
            (should
             (equal
              (mb-smart-swap-find-target
               '(("Lilypond-compile" ("created \\(.+\\.pdf\\)" 1))))
              target))))
      (delete-directory directory t))))

(ert-deftest mb-smart-simple-swap-visits-counterpart ()
  (let* ((directory (make-temp-file "simple-swap-" t))
         (first (expand-file-name "first.txt" directory))
         (second (expand-file-name "second.txt" directory))
         visited)
    (unwind-protect
        (progn
          (mb-smart-swap-test--touch first)
          (mb-smart-swap-test--touch second)
          (let ((buffer-file-name first))
            (cl-letf (((symbol-function 'find-file)
                       (lambda (file) (setq visited file) 'visited)))
              (should
               (eq (simple-swap `(("first.txt" "second.txt" ,directory)))
                   'visited))))
          (should (equal visited second)))
      (delete-directory directory t))))

(ert-deftest mb-smart-simple-swap-validates-rules ()
  (should-error
   (simple-swap '(("one")))
   :type 'user-error)
  (should-error
   (mb-smart-swap--other-directory default-directory '(invalid-rule))
   :type 'user-error))

(ert-deftest mb-smart-version-swap-preserves-relative-filename ()
  (let* ((root (make-temp-file "version-swap-" t))
         (first-directory (expand-file-name "first" root))
         (second-directory (expand-file-name "second" root))
         (source (expand-file-name "module.m" first-directory))
         visited)
    (unwind-protect
        (progn
          (make-directory first-directory)
          (make-directory second-directory)
          (mb-smart-swap-test--touch source)
          (let ((buffer-file-name source))
            (cl-letf (((symbol-function 'find-file)
                       (lambda (file) (setq visited file) 'visited)))
              (should
               (eq (version-swap
                    `((,first-directory ,second-directory)))
                   'visited))))
          (should
           (equal visited
                  (expand-file-name "module.m" second-directory))))
      (delete-directory root t))))

(ert-deftest mb-smart-elisp-swap-is-mode-specific ()
  (let (called)
    (cl-letf (((symbol-function 'mb-swap-ert-defun)
               (lambda () (setq called t) 'swapped)))
      (let ((major-mode 'text-mode))
        (should-not (elisp-swap))
        (should-not called))
      (let ((major-mode 'emacs-lisp-mode))
        (should (eq (elisp-swap) 'swapped))
        (should called)))))

(ert-deftest mb-smart-regexp-swap-visits-existing-target ()
  (let* ((directory (make-temp-file "regexp-swap-" t))
         (target (expand-file-name "result.pdf" directory))
         (default-directory directory)
         visited)
    (unwind-protect
        (progn
          (mb-smart-swap-test--touch target)
          (with-temp-buffer
            (setq default-directory directory)
            (insert "file=result.pdf")
            (cl-letf (((symbol-function 'find-file)
                       (lambda (file) (setq visited file) 'visited)))
              (should
               (eq (regexp-swap '("file=\\(.+\\)" 1))
                   'visited))))
          (should (equal visited target)))
      (delete-directory directory t))))

(ert-deftest mb-smart-swap-obeys-dispatch-priority ()
  (let (calls)
    (cl-letf (((symbol-function 'simple-swap)
               (lambda (&optional _) (push 'simple calls) 'done))
              ((symbol-function 'elisp-swap)
               (lambda () (push 'elisp calls)))
              ((symbol-function 'version-swap)
               (lambda (&optional _) (push 'version calls))))
      (should (eq (smart-swap) 'done))
      (should (equal calls '(simple))))))

(ert-deftest mb-smart-swap-compatibility-aliases ()
  (should
   (eq (indirect-function 'substitute-extension)
       (indirect-function 'mb-smart-swap-substitute-extension)))
  (should
   (eq (indirect-function 'expand-swap-target)
       (indirect-function 'mb-smart-swap-expand-target)))
  (should
   (eq (indirect-function 'swap-target)
       (indirect-function 'mb-smart-swap-target)))
  (should
   (eq (indirect-function 'match-current-buffer-p)
       (indirect-function 'mb-smart-swap-match-current-buffer-p)))
  (should
   (eq (indirect-function 'smart-swap-find-target)
       (indirect-function 'mb-smart-swap-find-target))))

(provide 'test-smart-swap)

;;; test-smart-swap.el ends here
