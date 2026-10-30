;;; test-mb-lisp.el --- Tests for mb-lisp -*- lexical-binding: t; -*-

;;; Commentary:

;; Deterministic tests for `mb-lisp'.  Nothing here starts an inferior Lisp
;; process; the REPL driver is exercised only through its pure helpers.

;;; Code:

(require 'ert)
(require 'mb-lisp)

(defmacro test-mb-lisp-with-setup (&rest body)
  "Run BODY with `mb-lisp-setup' active, then tear it down."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (mb-lisp-setup) ,@body)
     (mb-lisp-teardown)))

;;; Loading is free of global side effects

(ert-deftest test-mb-lisp-load-has-no-global-effects ()
  "Requiring the library must not advise or hook anything by itself."
  ;; The library is already loaded; a fresh teardown must leave no traces.
  (mb-lisp-teardown)
  (should-not (advice-member-p 'restrict-to-lisp-doc 'forward-sentence))
  (should-not (memq 'mb-inferior-lisp-set-locals inferior-lisp-mode-hook)))

(ert-deftest test-mb-lisp-setup-is-idempotent ()
  "`mb-lisp-setup' installs bindings and may be called repeatedly."
  (test-mb-lisp-with-setup
    (mb-lisp-setup)
    (should (eq (lookup-key mb-lisp-mode-map (kbd "C-M-x")) 'mb-lisp-eval-defun))
    (should (eq (lookup-key mb-lisp-mode-map (kbd "C-c z"))
                'mb-lisp-change-in-package))
    (should (equal (cl-remove 'mb-inferior-lisp-set-locals
                              inferior-lisp-mode-hook :test-not #'eq)
                   '(mb-inferior-lisp-set-locals)))
    (should (advice-member-p 'restrict-to-lisp-doc 'forward-sentence)))
  (should-not (advice-member-p 'restrict-to-lisp-doc 'forward-sentence)))

;;; Regression: forward-sentence advice must tolerate missing docstrings

(ert-deftest test-mb-lisp-forward-sentence-without-docstring ()
  "`forward-sentence' must not fail in a defun that has no docstring."
  (test-mb-lisp-with-setup
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun f (x)\n  (+ x 1))\n")
      (goto-char (point-min))
      (forward-line 1)
      ;; Before the fix this signalled a wrong-number-of-arguments error
      ;; from cl-destructuring-bind on a nil region.
      (should (progn (forward-sentence 1) t)))))

(ert-deftest test-mb-lisp-forward-sentence-stays-in-docstring ()
  "`forward-sentence' is restricted to the docstring of the defun at point."
  (test-mb-lisp-with-setup
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun f (x)\n  \"One.  Two.\"\n  (+ x 1))\n")
      (goto-char (point-min))
      (forward-line 1)
      (forward-char 3)
      (forward-sentence 1)
      (should (string= (buffer-substring-no-properties
                        (line-beginning-position) (point))
                       "  \"One."))
      ;; Narrowing keeps motion inside the docstring instead of running on
      ;; into the body of the defun.
      (forward-sentence 1)
      (should (string= (buffer-substring-no-properties
                        (line-beginning-position) (point))
                       "  \"One.  Two.")))))

(ert-deftest test-mb-lisp-forward-sentence-outside-lisp ()
  "Outside Lisp buffers `forward-sentence' is left untouched."
  (test-mb-lisp-with-setup
    (with-temp-buffer
      (text-mode)
      (insert "One.  Two.")
      (goto-char (point-min))
      (forward-sentence 1)
      (should (= (point) 5)))))

(ert-deftest test-mb-lisp-doc-region ()
  "`lisp-doc-region' finds a docstring and returns nil when there is none."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun f (x)\n  \"Doc.\"\n  x)\n")
    (goto-char (point-max))
    (should (lisp-doc-region))
    (erase-buffer)
    (insert "(defun g (x)\n  x)\n")
    (goto-char (point-max))
    (should-not (lisp-doc-region))))

;;; Reader

(ert-deftest test-mb-lisp-eread ()
  "`eread' converts Common Lisp print syntax into Elisp objects."
  (should (equal (eread "#2A((1 2) (3 4))") [[1 2] [3 4]]))
  (should (equal (eread "#<PACKAGE EGINA>") "(PACKAGE EGINA)"))
  (should (equal (eread "1.5L0") 1.5))
  (should (equal (eread "42") 42))
  (should-not (eread ""))
  (should-not (eread nil))
  (should-not (eread "(unbalanced")))

;;; Nested vector helpers; these used the undefined `map' before the brush-up

(ert-deftest test-mb-lisp-mvec-roundtrip ()
  "`tree->mvec' and `mvec->tree' are inverse conversions."
  (should (equal (tree->mvec '((1 2) (3 4))) [[1 2] [3 4]]))
  (should (equal (mvec->tree [[1 2] [3 4]]) '((1 2) (3 4))))
  (should (equal (mvec->tree (tree->mvec '((1 2) (3 4)))) '((1 2) (3 4))))
  (should (= (mvec-size [[1 2 3] [1 2 3]]) 6))
  (should (equal (tree-dimensions '((1 2) (3 4))) '(2 2)))
  (should (equal (mvec-dimensions [[1 2] [3 4]]) '(2 2))))

;;; Buffer helpers

(ert-deftest test-mb-lisp-buffer-package-name ()
  "`buffer-package-name' reads the preceding IN-PACKAGE form."
  (with-temp-buffer
    (insert "(in-package :utils)\n(defun f () 1)\n")
    (goto-char (point-max))
    (should (string= (buffer-package-name) "utils")))
  (with-temp-buffer
    (insert "(defun f () 1)\n")
    (goto-char (point-max))
    (should (string= (buffer-package-name) "COMMON-LISP-USER"))))

(ert-deftest test-mb-lisp-previous-sexp-handles-cl-syntax ()
  "`mb-previous-sexp' returns source text, so CL-only syntax survives."
  (with-temp-buffer
    (insert "(foo #(1 2 3))")
    (goto-char (point-max))
    (should (string= (mb-previous-sexp) "(foo #(1 2 3))"))))

(ert-deftest test-mb-lisp-search-symbol ()
  "`search-symbol' moves past the named symbol.
It used to call its argument as a function instead of using it."
  (with-temp-buffer
    (insert "(alpha beta)\n")
    (goto-char (point-min))
    (should (search-symbol 'beta))
    (should (= (point) (- (point-max) 2)))
    (goto-char (point-min))
    (should-not (search-symbol 'gamma))))

(ert-deftest test-mb-lisp-search-form ()
  "`search-form' moves point to the start of the named form."
  (with-temp-buffer
    (insert ";; preamble\n(defun f () 1)\n")
    (goto-char (point-min))
    (should (search-form 'defun))
    (should (looking-at "(defun f"))
    (goto-char (point-min))
    (should-not (search-form 'defmacro))))

;;; Pathname mangling

(ert-deftest test-mb-lisp-string-to-valid-pathname ()
  "`string-to-valid-pathname' replaces characters invalid in a file name."
  (should (string= (string-to-valid-pathname "qwe (a)+") "qwe_LaRP"))
  (should-not (string-match-p "'" (string-to-valid-pathname "a'b"))))

(ert-deftest test-mb-lisp-multi-substitute ()
  "`multi-substitute' applies every (NEW . OLD) pair in the map."
  (should (equal (multi-substitute '((?a . ?b) (?n . ?c)) "abc") "aan")))

;;; REPL waiting honours its argument

(ert-deftest test-mb-lisp-repl-wait-for-output-honours-timeout ()
  "`repl-wait-for-output' uses its argument rather than the global default.
The original ignored MAX-MILLISECONDS and always waited the global time."
  (let ((*repl-output* nil)
        (*repl-error* nil)
        (*repl-last-output-time* nil)
        (*repl-max-waiting-time* (* 1000 600)))
    (let ((start (current-time)))
      (should-not (repl-wait-for-output 1))
      (should (< (float-time (time-subtract (current-time) start)) 5)))))

(ert-deftest test-mb-lisp-repl-wait-for-output-returns-existing-output ()
  "`repl-wait-for-output' returns immediately when output is already present."
  (let ((*repl-output* '("done"))
        (*repl-error* nil)
        (*repl-last-output-time* nil))
    (should (equal (repl-wait-for-output 1) '("done")))))

(ert-deftest test-mb-lisp-repl-wait-for-output-alias ()
  "The misspelled original name still works."
  (should (eq (symbol-function 'repl-wait-for-ouput) #'repl-wait-for-output)))

;;; Output collection

(ert-deftest test-mb-lisp-process-output ()
  "`mb-lisp-process-output' parses batches once a prompt has arrived."
  (let ((*repl-comint-output-list* nil)
        (*repl-output* nil))
    (setq *repl-comint-output-list* (list "[1]> " "42 ;\n\n"))
    (mb-lisp-process-output)
    (should (member 42 *repl-output*))
    (should-not *repl-comint-output-list*)))

(ert-deftest test-mb-lisp-on-output-respects-discard-flag ()
  "`mb-lisp-on-output' passes output through unless it is discarded."
  (let ((*repl-comint-output-list* nil)
        (*repl-output* nil)
        (*repl-error* nil)
        (*repl-last-output-time* nil)
        (*repl-discard-output* nil))
    (should (string= (mb-lisp-on-output "hello") "hello")))
  (let ((*repl-comint-output-list* nil)
        (*repl-output* nil)
        (*repl-error* nil)
        (*repl-last-output-time* nil)
        (*repl-discard-output* t))
    (should (string= (mb-lisp-on-output "hello") ""))))

(ert-deftest test-mb-lisp-repl-error-p ()
  "`repl-error-p' recognises CLISP error reports."
  (should (repl-error-p "*** - foo"))
  (should (repl-error-p "** - Continuable Error"))
  (should-not (repl-error-p "42")))

;;; Modes

(ert-deftest test-mb-lisp-mode-sets-paragraph-start ()
  "`mb-lisp-mode' derives from `lisp-mode' and sets `paragraph-start' locally.
The setting used to be a top-level `setq-local', which leaked into
whichever buffer happened to load the library."
  (with-temp-buffer
    (mb-lisp-mode)
    (should (derived-mode-p 'lisp-mode))
    (should (string= paragraph-start "\"\\|\f\\|[ \t]*$")))
  (with-temp-buffer
    (should-not (string= paragraph-start "\"\\|\f\\|[ \t]*$"))))

(ert-deftest test-mb-lisp-repl-mode-map ()
  "The REPL keymap is populated and reachable through its legacy name."
  (should (keymapp mb-lisp-repl-mode-map))
  (should (eq +mb-lisp-repl-mode-map+ mb-lisp-repl-mode-map))
  (should (eq (lookup-key mb-lisp-repl-mode-map "Q") 'mb-lisp-repl-quit))
  (should (eq (lookup-key mb-lisp-repl-mode-map "S")
              'mb-lisp-repl-eval-previous-sexp))
  (should (commandp (lookup-key mb-lisp-repl-mode-map "c")))
  (should (commandp (lookup-key mb-lisp-repl-mode-map "1"))))

;;; Error boundaries

(ert-deftest test-mb-lisp-load-buffer-requires-a-file ()
  "`mb-lisp-load-buffer' reports a clear error in a buffer without a file."
  (with-temp-buffer
    (should-error (mb-lisp-load-buffer nil) :type 'user-error)))

(ert-deftest test-mb-lisp-eval-defun-requires-a-defun ()
  "`mb-lisp-eval-defun' reports a clear error outside a defun."
  (with-temp-buffer
    (lisp-mode)
    (should-error (mb-lisp-eval-defun nil) :type 'user-error)))

(provide 'test-mb-lisp)

;;; test-mb-lisp.el ends here
