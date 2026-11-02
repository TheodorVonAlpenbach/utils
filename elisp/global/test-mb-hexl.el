;;; test-mb-hexl.el --- Tests for mb-hexl -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mb-hexl)

(ert-deftest mb-hexl-converts-characters-to-integers ()
  (should (= (mb-hexl-character-to-integer ?0) 0))
  (should (= (mb-hexl-character-to-integer ?a) 10))
  (should (= (mb-hexl-character-to-integer ?F) 15))
  (should-error
   (mb-hexl-character-to-integer ?g)
   :type 'user-error))

(ert-deftest mb-hexl-converts-integers-to-characters ()
  (should (= (mb-hexl-integer-to-character 0) ?0))
  (should (= (mb-hexl-integer-to-character 15) ?f))
  (should-error
   (mb-hexl-integer-to-character 16)
   :type 'user-error))

(ert-deftest mb-hexl-indexes-both-byte-orders ()
  (should (= (mb-hexl-ith-character-big-endian "01234567" 0) ?7))
  (should (= (mb-hexl-ith-character-big-endian "01234567" 7) ?0))
  (should (= (mb-hexl-ith-character-little-endian "01234567" 0) ?1))
  (should (= (mb-hexl-ith-character-little-endian "01234567" 7) ?6)))

(ert-deftest mb-hexl-indexing-validates-input ()
  (should-error
   (mb-hexl-ith-character-big-endian "0123" 0)
   :type 'user-error)
  (should-error
   (mb-hexl-ith-character-little-endian "01234567" 8)
   :type 'user-error))

(ert-deftest mb-hexl-converts-byte-strings ()
  (should (= (mb-hexl-byte-to-integer "00") 0))
  (should (= (mb-hexl-byte-to-integer "aF") 175))
  (should-error
   (mb-hexl-byte-to-integer "f")
   :type 'user-error)
  (should-error
   (mb-hexl-byte-to-integer "gg")
   :type 'user-error))

(ert-deftest mb-hexl-splits-natural-byte-order ()
  (should
   (equal (mb-hexl-split-to-bytes "01 02\nff")
          '(1 2 255)))
  (should-error
   (mb-hexl-split-to-bytes "123")
   :type 'user-error))

(ert-deftest mb-hexl-converts-big-endian-integers ()
  (should (= (mb-hexl-string-to-integer "01 02") 258))
  (should (= (mb-hexl-string-to-integer "0000 0030" :big) 48)))

(ert-deftest mb-hexl-converts-little-endian-integers ()
  (should (= (mb-hexl-string-to-integer "01 02" :little) 513))
  (should (= (mb-hexl-string-to-integer "03 00 00 00" "le") 3)))

(ert-deftest mb-hexl-rejects-unknown-byte-order ()
  (should-error
   (mb-hexl-string-to-integer "01" :middle)
   :type 'user-error))

(ert-deftest mb-hexl-region-commands-return-and-display-values ()
  (with-temp-buffer
    (insert "01 02")
    (let (displayed)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (setq displayed
                         (apply #'format format-string arguments)))))
        (should (= (hexl-region-to-int-be (point-min) (point-max))
                   258))
        (should (equal displayed "258"))
        (should (= (hexl-region-to-int-le (point-min) (point-max))
                   513))
        (should (equal displayed "513"))))))

(ert-deftest mb-hexl-formats-nonnegative-integers ()
  (should (equal (mb-hexl-integer-to-string 100) "64"))
  (cl-letf (((symbol-function 'message) #'ignore))
    (should (equal (hexl-int-to-hexl 15) "f")))
  (should-error
   (mb-hexl-integer-to-string -1)
   :type 'user-error))

(ert-deftest mb-hexl-retains-mode-bindings ()
  (should (eq (lookup-key hexl-mode-map (kbd "C-c l"))
              #'hexl-region-to-int-le))
  (should (eq (lookup-key hexl-mode-map (kbd "C-c b"))
              #'hexl-region-to-int-be))
  (should (eq (lookup-key hexl-mode-map (kbd "C-t"))
              #'hexl-int-to-hexl)))

(ert-deftest mb-hexl-compatibility-aliases ()
  (dolist (pair
           '((hexl-char-to-int . mb-hexl-character-to-integer)
             (hexl-int-to-char . mb-hexl-integer-to-character)
             (hexl-ith-char-be . mb-hexl-ith-character-big-endian)
             (hexl-ith-char-le . mb-hexl-ith-character-little-endian)
             (hexl-to-int-byte . mb-hexl-byte-to-integer)
             (remove-spaces . mb-hexl-remove-whitespace)
             (hexl-split-to-bytes . mb-hexl-split-to-bytes)
             (hexl-string-to-int . mb-hexl-string-to-integer)))
    (should
     (eq (indirect-function (car pair))
         (indirect-function (cdr pair))))))

(provide 'test-mb-hexl)

;;; test-mb-hexl.el ends here
