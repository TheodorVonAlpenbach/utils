;;; test-mb-locale.el --- Tests for mb-locale -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'mb-locale)

(ert-deftest mb-locale-transliterates-default-latin1 ()
  (should
   (equal (mb-locale-string-to-ascii "Blåbærgrøt, Straße")
          "Blaabaergroet, Strasse")))

(ert-deftest mb-locale-preserves-unmapped-characters ()
  (should
   (equal (mb-locale-string-to-ascii "ASCII Ελληνικά")
          "ASCII Ελληνικά")))

(ert-deftest mb-locale-uses-caller-supplied-encoding ()
  (let ((encoding
         '(("å" "aa" "%E5" 229 "Ã¥")
           ("æ" "ae" "%E6" 230 "Ã¦")
           ("ø" "oe" "%F8" 248 "Ã¸"))))
    (should
     (equal (mb-locale-string-to-ascii "Blåbærgrøt" encoding)
            "Blaabaergroet"))
    (should
     (equal (mb-locale-character-to-ascii ?æ nil encoding)
            "ae"))))

(ert-deftest mb-locale-character-errors-when-requested ()
  (should-error
   (mb-locale-character-to-ascii ?x t '(("y" "z")))
   :type 'user-error)
  (should-error
   (mb-locale-character-to-ascii "x")
   :type 'user-error))

(ert-deftest mb-locale-replaces-literal-region-text ()
  (with-temp-buffer
    (insert "[a] [b]")
    (mb-locale-map-string-region "[" "<<" (point-min) (point-max))
    (should (equal (buffer-string) "<<a] <<b]"))))

(ert-deftest mb-locale-region-marker-tracks-length-changes ()
  (with-temp-buffer
    (insert "åå outside")
    (mb-locale-map-string-region "å" "aa" (point-min) 3)
    (should (equal (buffer-string) "aaaa outside"))))

(ert-deftest mb-locale-maps-selected-columns ()
  (with-temp-buffer
    (insert "å æ")
    (mb-locale-map-strings-region
     (point-min) (point-max)
     '(("å" "aa" "A") ("æ" "ae" "E"))
     0 2)
    (should (equal (buffer-string) "A E"))))

(ert-deftest mb-locale-region-command-transliterates ()
  (with-temp-buffer
    (insert "blåbær")
    (mb-locale-replace-iso-latin1-region (point-min) (point-max))
    (should (equal (buffer-string) "blaabaer"))))

(ert-deftest mb-locale-skips-incomplete-mapping-rows ()
  (with-temp-buffer
    (insert "abc")
    (mb-locale-map-strings-region
     (point-min) (point-max)
     '(("a" nil) (nil "x") ("b" "B")))
    (should (equal (buffer-string) "aBc"))))

(ert-deftest mb-locale-rejects-empty-replacement-source ()
  (with-temp-buffer
    (should-error
     (mb-locale-map-string-region "" "x" (point-min) (point-max))
     :type 'user-error)))

(ert-deftest mb-locale-table-retains-consumer-columns ()
  (let ((entry (assoc "ø" *iso-latin1-encoding*)))
    (should (equal entry '("ø" "oe" "%F8" 248)))))

(ert-deftest mb-locale-compatibility-aliases ()
  (dolist (pair
           '((map-string-region . mb-locale-map-string-region)
             (map-strings-region . mb-locale-map-strings-region)
             (replace-iso-latin1-with-7bit-region
              . mb-locale-replace-iso-latin1-region)
             (iso-latin1-2-7bit-char
              . mb-locale-character-to-ascii)
             (iso-latin1-2-7bit . mb-locale-string-to-ascii)))
    (should
     (eq (indirect-function (car pair))
         (indirect-function (cdr pair))))))

(provide 'test-mb-locale)

;;; test-mb-locale.el ends here
