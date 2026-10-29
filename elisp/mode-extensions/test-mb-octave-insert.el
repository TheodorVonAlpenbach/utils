;;; test-mb-octave-insert.el --- Tests for mb-octave-insert -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'mb-octave-insert)

(ert-deftest mb-octave-insert-map-and-defun ()
  (let ((map (mb-octave-insert-map))
        called)
    (should (eq (lookup-key map "d") #'mb-octave-insert-defun))
    (should (eq (lookup-key map "D") #'mb-octave-insert-doc))
    (should (eq (lookup-key map "i") #'mb-octave-insert-if))
    (should (eq (lookup-key map "c") #'mb-octave-insert-case))
    (should (eq (lookup-key map "f") #'mb-octave-insert-for))
    (cl-letf (((symbol-function 'octave-insert-defun)
               (lambda ()
                 (setq called t))))
      (mb-octave-insert-defun)
      (should called))))

(ert-deftest mb-octave-insert-if-skeleton ()
  (with-temp-buffer
    (insert "before")
    (cl-letf (((symbol-function 'indent-region) #'ignore))
      (mb-octave-insert-if))
    (should (equal (buffer-string)
                   "before\nif ()\nelse\nendif\n\n"))
    (should (eq (char-before) ?\())
    (should (eq (char-after) ?\)))))

(ert-deftest mb-octave-insert-for-skeleton ()
  (with-temp-buffer
    (octave-mode)
    (insert "function y = f (x)")
    (mb-octave-insert-for)
    (should (equal (buffer-string)
                   "function y = f (x)\n  for \n  endfor\n\n"))
    (should (equal (buffer-substring (line-beginning-position) (point))
                   "  for "))))

(ert-deftest mb-octave-insert-case-signals ()
  (should-error (mb-octave-insert-case) :type 'user-error))

(ert-deftest mb-octave-insert-doc-expands-template ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/calculate_total.m")
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (format &optional _time _zone)
                 (pcase format
                   ("%Y" "2026")
                   ("%Y-%m-%d" "2026-10-28")))))
      (mb-octave-insert-doc))
    (should (string-match-p
             "Copyright (C) 2026 Light Structures" (buffer-string)))
    (should (string-match-p
             "calculate_total (@var{x})" (buffer-string)))
    (should (string-match-p
             "Created: 2026-10-28" (buffer-string)))
    (should-not (string-match-p "%this" (buffer-string)))))

(provide 'test-mb-octave-insert)

;;; test-mb-octave-insert.el ends here
