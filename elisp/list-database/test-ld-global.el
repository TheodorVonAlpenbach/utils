;;; test-ld-global.el --- Tests for list-database globals -*- lexical-binding: t; -*-

(require 'ert)
(require 'ld-global)

(defvar *local-data-dir*)

(ert-deftest ld-global-default-repository-uses-user-emacs-directory ()
  (let ((user-emacs-directory "/tmp/emacs-home/")
        (was-bound (boundp '*local-data-dir*))
        (old-value (and (boundp '*local-data-dir*)
                        *local-data-dir*)))
    (unwind-protect
        (progn
          (makunbound '*local-data-dir*)
          (should
           (equal (ld-default-repository-directory)
                  "/tmp/emacs-home/ld")))
      (when was-bound
        (setq *local-data-dir* old-value)))))

(ert-deftest ld-global-default-repository-preserves-legacy-data-directory ()
  (let ((*local-data-dir* "/tmp/local-data/"))
    (should
     (equal (ld-default-repository-directory)
            "/tmp/local-data/ld"))))

(ert-deftest ld-global-retains-repository-compatibility-name ()
  (let ((ld-repository-directory "/tmp/databases"))
    (should (equal +ld-repository+ "/tmp/databases"))))

(ert-deftest ld-global-initializes-placeholder-database ()
  (should (ld-database-p *current-database*))
  (should (equal (ld-database-identifier *current-database*)
                 '(:empty-db)))
  (should-not (ld-database-tables *current-database*)))

(ert-deftest ld-global-has-safe-operation-defaults ()
  (should *ld-check-data-p*)
  (should (= *ld-autogenerate-base* 1))
  (should (= *ld-autogenerate-increment* 1)))

(provide 'test-ld-global)

;;; test-ld-global.el ends here
