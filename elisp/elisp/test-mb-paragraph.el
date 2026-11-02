;;; test-mb-paragraph.el --- Tests for mb-paragraph -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'mb-paragraph)

(ert-deftest test-mb-paragraph-keeps-standard-forward-sentence ()
  "Loading the library must not replace Emacs's core sentence command."
  (should-not (eq (symbol-function 'forward-sentence)
                  (symbol-function 'mb-paragraph-forward-sentence)))
  (should (string-match-p "/paragraphs\\."
                          (or (symbol-file 'forward-sentence) ""))))

(ert-deftest test-mb-paragraph-legacy-time-regexp-alias ()
  "The generic legacy variable remains an alias for the namespaced constant."
  (should (eq (indirect-variable 'time-regexp)
              'mb-paragraph-time-regexp))
  (should (string= time-regexp mb-paragraph-time-regexp))
  (should (string-match-p time-regexp "09:16")))

(ert-deftest test-mb-paragraph-kill-region-returns-text ()
  "`kill-region*' returns killed text, optionally retaining properties."
  (with-temp-buffer
    (insert (propertize "abc" 'face 'bold))
    (let ((text (kill-region* 1 4 t)))
      (should (string= text "abc"))
      (should (eq (get-text-property 0 'face text) 'bold))
      (should (string-empty-p (buffer-string))))))

(ert-deftest test-mb-paragraph-kill-schedule-paragraph ()
  "Schedule paragraphs are split into timestamp and text."
  (with-temp-buffer
    (insert "09:16  This is a paragraph.\n")
    (goto-char 10)
    (should (equal (kill-current-schedule-paragraph)
                   '("09:16" "  This is a paragraph.\n")))
    (should (string-empty-p (buffer-string))))
  (with-temp-buffer
    (insert "This paragraph has no timestamp.\n")
    (goto-char 10)
    (should (equal (kill-current-schedule-paragraph)
                   '(nil "This paragraph has no timestamp.\n")))))

(ert-deftest test-mb-paragraph-schedule-fill ()
  "Schedule filling preserves the timestamp, fill column, and hanging indent."
  (with-temp-buffer
    (setq fill-column 28)
    (insert
     "09:16  This is a fairly long schedule paragraph that should wrap over several lines.\n")
    (goto-char 12)
    (schedule-fill-paragraph)
    (should
     (string=
      (buffer-string)
      (concat "09:16  This is a fairly long\n"
              "       schedule paragraph\n"
              "       that should wrap over\n"
              "       several lines.\n")))))

(ert-deftest test-mb-paragraph-schedule-fill-skips-date-heading ()
  "An ISO-date heading is not treated as a schedule paragraph."
  (with-temp-buffer
    (insert "2026-11-02\nHeading\n")
    (goto-char (point-min))
    (let ((before (buffer-string)))
      (schedule-fill-paragraph)
      (should (string= (buffer-string) before)))))

(ert-deftest test-mb-paragraph-set-locals ()
  "Schedule locals are installed only in the requested buffer."
  (let ((target (generate-new-buffer " *mb-paragraph-locals*")))
    (unwind-protect
        (progn
          (set-locals-arbeidslog target)
          (with-current-buffer target
            (should (local-variable-p 'paragraph-start))
            (should (string= paragraph-start schedule-paragraph-start))
            (should (string= fill-prefix "       "))
            (should (eq fill-paragraph-function
                        #'schedule-fill-paragraph))))
      (kill-buffer target))))

(ert-deftest test-mb-paragraph-marking-commands ()
  "Whole-word and whole-sexp commands activate the expected regions."
  (with-temp-buffer
    (insert "alpha beta")
    (goto-char 3)
    (mark-whole-word)
    (should (string= (buffer-substring (region-beginning) (region-end))
                     "alpha")))
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(alpha beta)")
    (goto-char 3)
    (mark-whole-sexp)
    (should (string= (buffer-substring (region-beginning) (region-end))
                     "alpha"))))

(ert-deftest test-mb-paragraph-forward-sentence ()
  "Personal sentence motion accepts single spaces and respects case."
  (with-temp-buffer
    (insert "One. Two. Three.")
    (goto-char (point-min))
    (mb-paragraph-forward-sentence 1)
    (should (= (point) 7))
    (mb-paragraph-forward-sentence 1)
    (should (= (point) 12))
    (mb-paragraph-forward-sentence -1)
    (should (= (point) 7)))
  (with-temp-buffer
    (insert "One. two. Three.")
    (goto-char (point-min))
    (mb-paragraph-forward-sentence 1)
    (should (= (point) 12))))

(provide 'test-mb-paragraph)

;;; test-mb-paragraph.el ends here
