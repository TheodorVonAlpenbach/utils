;;; test-mb-grep.el --- Tests for mb-grep -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mb-grep)

(defun mb-grep-test--write-file (file)
  "Create FILE and its parent directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert "needle\n")))

(ert-deftest mb-grep-filter-dirs-applies-both-regexps ()
  (should
   (equal (mb-grep-filter-dirs
           '("/src/elisp/" "/src/elisp/old/" "/src/lisp/")
           "/elisp/" "/old/")
          '("/src/elisp/"))))

(ert-deftest mb-grep-files-support-types-wildcards-and-braces ()
  (let ((directory (make-temp-file "mb-grep-files-" t)))
    (unwind-protect
        (progn
          (dolist (name '("one.el" "two.el.backup" "three.js" ".emacs"))
            (mb-grep-test--write-file (expand-file-name name directory)))
          (should
           (equal (mapcar #'file-name-nondirectory
                          (mb-grep--files (list directory) "el"))
                  '("one.el")))
          (should
           (equal (mapcar #'file-name-nondirectory
                          (mb-grep--files (list directory) "{el,js}"))
                  '("one.el" "three.js")))
          (should
           (equal (mapcar #'file-name-nondirectory
                          (mb-grep--files (list directory) ".emacs"))
                  '(".emacs")))
          (should
           (equal (mapcar #'file-name-nondirectory
                          (mb-grep--files (list directory) "*.backup"))
                  '("two.el.backup"))))
      (delete-directory directory t))))

(ert-deftest mb-grep-build-command-quotes-target-and-paths ()
  (let* ((directory (make-temp-file "mb grep files-" t))
         (file (expand-file-name "two words.el" directory))
         (mb-grep-program "grep"))
    (unwind-protect
        (progn
          (mb-grep-test--write-file file)
          (should
           (equal (mb-grep-build-command "it's here" (list directory) "el")
                  (mapconcat
                   #'shell-quote-argument
                   (list "grep" "-nHs" "-E" "--" "it's here" file)
                   " "))))
      (delete-directory directory t))))

(ert-deftest mb-grep-build-command-rejects-invalid-searches ()
  (let ((directory (make-temp-file "mb-grep-empty-" t)))
    (unwind-protect
        (progn
          (should-error
           (mb-grep-build-command "" (list directory))
           :type 'user-error)
          (should-error
           (mb-grep-build-command "needle" (list directory) "el")
           :type 'user-error))
      (delete-directory directory t))))

(ert-deftest mb-grep-find-directories-obeys-depth ()
  (let ((directory (make-temp-file "mb-grep-dirs-" t)))
    (unwind-protect
        (let* ((child (expand-file-name "child" directory))
               (grandchild (expand-file-name "grandchild" child)))
          (make-directory grandchild t)
          (should
           (equal (mb-grep-find-directories directory 0)
                  (list (file-name-as-directory directory))))
          (should
           (equal (mb-grep-find-directories directory 1)
                  (mapcar #'file-name-as-directory
                          (list directory child))))
          (should
           (equal (mb-grep-find-directories directory)
                  (mapcar #'file-name-as-directory
                          (list directory child grandchild)))))
      (delete-directory directory t))))

(ert-deftest mb-grep-directory-compatibility-aliases ()
  (should (eq (indirect-function 'finddirs)
              (indirect-function 'mb-grep-find-directories)))
  (should (eq (indirect-function 'subdirs)
              (indirect-function 'mb-grep-find-directories))))

(ert-deftest mb-grep-dirs-decodes-prefix-values ()
  (let (arguments)
    (cl-letf (((symbol-function 'mb-grep-dirs-1)
               (lambda (up depth directory)
                 (setq arguments (list up depth directory)))))
      (mb-grep-dirs nil "root")
      (should (equal arguments '(0 nil "root")))
      (mb-grep-dirs 3 "root")
      (should (equal arguments '(3 nil "root")))
      (mb-grep-dirs 35 "root")
      (should (equal arguments '(3 5 "root")))
      (mb-grep-dirs -5 "root")
      (should (equal arguments '(0 5 "root")))
      (mb-grep-dirs -14 "root")
      (should (equal arguments '(4 1 "root")))
      (mb-grep-dirs '- "root")
      (should (equal arguments '(0 0 "root"))))))

(ert-deftest mb-grep-dirs-normalizes-raw-prefixes ()
  (let (arguments)
    (cl-letf (((symbol-function 'mb-grep-dirs-1)
               (lambda (up depth directory)
                 (setq arguments (list up depth directory)))))
      (mb-grep-dirs '(4) "root")
      (should (equal arguments '(4 nil "root"))))))

(ert-deftest mb-grep-basic-starts-grep-compilation ()
  (let* ((directory (make-temp-file "mb-grep-basic-" t))
         (file (expand-file-name "source.el" directory))
         command mode)
    (unwind-protect
        (progn
          (mb-grep-test--write-file file)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_program) "/usr/bin/grep"))
                    ((symbol-function 'compilation-start)
                     (lambda (value &optional mode-value &rest _)
                       (setq command value
                             mode mode-value)
                       'compilation-buffer)))
            (should
             (eq (mb-grep-basic
                  :target "needle"
                  :directories (list directory)
                  :types "el")
                 'compilation-buffer)))
          (should (eq mode 'grep-mode))
          (should (string-match-p
                   (regexp-quote (shell-quote-argument file))
                   command)))
      (delete-directory directory t))))

(ert-deftest mb-grep-basic-reports-missing-program ()
  (let ((mb-grep-program "missing-grep"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) nil)))
      (should-error
       (mb-grep-basic :target "needle" :directories (list default-directory))
       :type 'user-error))))

(ert-deftest mb-grep-dispatches-by-major-mode ()
  (let (called)
    (cl-letf (((symbol-function 'mb-elisp-grep)
               (lambda () (setq called 'elisp)))
              ((symbol-function 'mb-scilab-grep)
               (lambda () (setq called 'scilab)))
              ((symbol-function 'mb-gen-grep)
               (lambda () (setq called 'general))))
      (let ((major-mode 'emacs-lisp-mode))
        (mb-grep)
        (should (eq called 'elisp)))
      (let ((major-mode 'mbscilab-mode))
        (mb-grep)
        (should (eq called 'scilab)))
      (let ((major-mode 'text-mode))
        (mb-grep)
        (should (eq called 'general))))))

(ert-deftest mb-grep-cpp-commands-select-intended-types ()
  (let (types)
    (cl-letf (((symbol-function 'mb-grep-basic)
               (lambda (&rest arguments)
                 (setq types (plist-get arguments :types)))))
      (mb-h-grep)
      (should (equal types mb-grep-cpp-header-types))
      (mb-cpp-grep)
      (should (equal types mb-grep-cpp-source-types))
      (mb-c++-grep)
      (should
       (equal types
              (append mb-grep-cpp-header-types
                      mb-grep-cpp-source-types)))
      (mb-all-grep)
      (should-not types))))

(provide 'test-mb-grep)

;;; test-mb-grep.el ends here
