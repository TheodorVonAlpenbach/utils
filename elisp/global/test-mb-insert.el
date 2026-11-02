;;; test-mb-insert.el --- Tests for mb-insert -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mb-insert)

(ert-deftest test-mb-insert-parentheses-empty-with-spacing ()
  "An empty pair is inserted at point with requested surrounding space."
  (with-temp-buffer
    (insert "ab")
    (goto-char 2)
    (insert-parentheses* nil)
    (should (string= (buffer-string) "a () b"))
    (should (= (point) 4))))

(ert-deftest test-mb-insert-parentheses-positive-and-negative ()
  "Positive and negative counts wrap following and preceding sexps."
  (with-temp-buffer
    (insert "alpha beta")
    (goto-char (point-min))
    (insert-double-quotes 1)
    (should (string= (buffer-string) "\"alpha\" beta")))
  (with-temp-buffer
    (insert "alpha beta")
    (goto-char (point-max))
    (insert-square-brackets -1)
    (should (string= (buffer-string) "alpha [beta]"))))

(ert-deftest test-mb-insert-parentheses-normalizes-raw-prefix ()
  "A raw universal prefix is converted to a numeric sexp count."
  (with-temp-buffer
    (insert "a b c d")
    (goto-char (point-min))
    (insert-parentheses* '(4) :ensure-space :none)
    (should (string= (buffer-string) "(a b c d)"))))

(ert-deftest test-mb-insert-date-commands ()
  "Date commands dispatch their prefix arguments deterministically."
  (cl-letf (((symbol-function 'iso-date)
             (lambda (&rest _) "DATE"))
            ((symbol-function 'iso-time)
             (lambda (&key with-seconds)
               (if with-seconds "TIME-SECONDS" "TIME")))
            ((symbol-function 'iso-date-and-time)
             (lambda (&key with-seconds)
               (if with-seconds "DATETIME-SECONDS" "DATETIME")))
            ((symbol-function 'full-date)
             (lambda (&rest _) "FULL-DATE")))
    (with-temp-buffer
      (insert-date nil)
      (insert "|")
      (insert-date t)
      (insert "|")
      (insert-date 3)
      (insert "|")
      (insert-time nil)
      (insert "|")
      (insert-time 3)
      (insert "|")
      (insert-date-and-time)
      (insert "|")
      (insert-date-and-time 3)
      (insert "|")
      (insert-full-date)
      (should
       (string=
        (buffer-string)
        (concat "DATE|DATETIME|DATETIME-SECONDS|TIME|TIME-SECONDS|"
                "DATETIME|DATETIME-SECONDS|FULL-DATE"))))))

(ert-deftest test-mb-insert-date-and-time-is-interactive ()
  "`insert-date-and-time' accepts its formerly missing interactive argument."
  (cl-letf (((symbol-function 'iso-date-and-time)
             (lambda (&rest _) "DATETIME")))
    (with-temp-buffer
      (call-interactively #'insert-date-and-time)
      (should (string= (buffer-string) "DATETIME")))))

(ert-deftest test-mb-insert-iso-to-short-date ()
  "ISO date conversion replaces the detected bounds or reports their absence."
  (with-temp-buffer
    (insert "2026-11-02")
    (cl-letf (((symbol-function 'date-at-point)
               (lambda (&optional _) 'parsed-date))
              ((symbol-function 'thing-at-point-bounds-of-date)
               (lambda (&optional _) '(1 11)))
              ((symbol-function 'short-date)
               (lambda (&optional _) "SHORT")))
      (iso-to-short-date-at-point)
      (should (string= (buffer-string) "SHORT"))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'date-at-point)
               (lambda (&optional _) nil))
              ((symbol-function 'thing-at-point-bounds-of-date)
               (lambda (&optional _) nil)))
      (should-error (iso-to-short-date-at-point) :type 'user-error))))

(ert-deftest test-mb-insert-kill-sexp-returns-text ()
  "`kill-sexp*' returns the text it killed."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(alpha beta)")
    (goto-char (point-max))
    (should (string= (kill-sexp* 1) "(alpha beta)"))
    (should (string-empty-p (buffer-string)))))

(ert-deftest test-mb-insert-repeat-command ()
  "`insert-n' evaluates its object once and returns a repeatable command."
  (let ((evaluations 0)
        command)
    (setq command (insert-n (progn (cl-incf evaluations) 'x)))
    (with-temp-buffer
      (funcall command 3)
      (should (string= (buffer-string) "xxx")))
    (should (= evaluations 1))))

(ert-deftest test-mb-insert-skip-quote-honors-count ()
  "Omission markers honor their formerly ignored count."
  (with-temp-buffer
    (insert-skip-quote 3)
    (should (string= (buffer-string) "[...][...][...]"))))

(ert-deftest test-mb-insert-unicode-symbols ()
  "Unicode symbol helpers retain their intended Latin-1 characters."
  (with-temp-buffer
    (insert-symbol-sup1 1)
    (insert-symbol-sup2 1)
    (insert-symbol-sup3 1)
    (insert-symbol-deg 1)
    (insert-symbol-ordm 1)
    (insert-symbol-ordf 1)
    (insert-symbol-macr 1)
    (insert-german-double-s 1)
    (insert-c-cedilla 1)
    (insert-1/2 1)
    (should (string= (buffer-string) "¹²³°ºª¯ßç½")))
  (with-temp-buffer
    (insert "word")
    (goto-char (point-min))
    (insert-guillemets 1)
    (should (string= (buffer-string) "«word»"))))

(ert-deftest test-mb-insert-bullet-spacing ()
  "Bullet insertion honors the optional no-spacing flag."
  (with-temp-buffer
    (insert-symbol-bullet 2)
    (should (string= (buffer-string) "·· ")))
  (with-temp-buffer
    (insert-symbol-bullet 2 t)
    (should (string= (buffer-string) "··"))))

(ert-deftest test-mb-insert-prefix-region ()
  "Every line intersecting the region receives the requested prefix."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (insert-prefix-region (point-min) (point-max) "| ")
    (should (string= (buffer-string) "| one\n| two\n| three\n"))))

(ert-deftest test-mb-insert-underline ()
  "Underlines match the current line length and repeat the pattern."
  (with-temp-buffer
    (insert "hello")
    (insert-underline "-=")
    (should (string= (buffer-string) "hello\n-=-=-")))
  (with-temp-buffer
    (insert "abc")
    (insert-underline "")
    (should (string= (buffer-string) "abc\n==="))))

(ert-deftest test-mb-insert-objects ()
  "`insert-object' and `ins' use the %s representation."
  (with-temp-buffer
    (insert-object 'alpha)
    (ins ":" 42)
    (should (string= (buffer-string) "alpha:42"))))

(ert-deftest test-mb-insert-provide ()
  "A provide form is derived from the visited file name."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/example-feature.el")
    (insert ";;; code\n\n")
    (insert-provide)
    (should (string-suffix-p "(provide 'example-feature)\n"
                             (buffer-string)))))

(ert-deftest test-mb-insert-todo-item ()
  "A todo item uses the short date and leaves point before the newline."
  (cl-letf (((symbol-function 'short-date)
             (lambda (&optional _) "02.11.2026")))
    (with-temp-buffer
      (insert "existing")
      (goto-char (point-min))
      (insert-todo-item)
      (should (string= (buffer-string) "* 02.11.2026 \nexisting"))
      (should (= (point) 14)))))

(ert-deftest test-mb-insert-keeps-canonical-alphabet-command ()
  "Loading mb-insert must not replace the tested utility alphabet command."
  (should (string-suffix-p "/utils/mb-utils-buffer.el"
                           (or (symbol-file 'insert-alphabet) ""))))

(provide 'test-mb-insert)

;;; test-mb-insert.el ends here
