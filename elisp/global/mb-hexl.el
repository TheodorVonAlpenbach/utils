;;; mb-hexl.el --- Hexadecimal conversion helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'hexl)
(require 'subr-x)

(defconst hex-to-int-list
  '((?0 . 0) (?1 . 1) (?2 . 2) (?3 . 3) (?4 . 4)
    (?5 . 5) (?6 . 6) (?7 . 7) (?8 . 8) (?9 . 9)
    (?a . 10) (?b . 11) (?c . 12)
    (?d . 13) (?e . 14) (?f . 15))
  "Compatibility table mapping lowercase hexadecimal digits to values.")

(defconst hexl-be-byte-map-list
  '((0 . 7) (1 . 6) (2 . 5) (3 . 4)
    (4 . 3) (5 . 2) (6 . 1) (7 . 0))
  "Character index map for reversed big-endian display order.")

(defconst hexl-le-byte-map-list
  '((0 . 1) (1 . 0) (2 . 3) (3 . 2)
    (4 . 5) (5 . 4) (6 . 7) (7 . 6))
  "Character index map for little-endian byte display order.")

(defun mb-hexl-character-to-integer (character)
  "Return the integer represented by hexadecimal CHARACTER."
  (unless (and (characterp character)
               (string-match-p
                "\\`[[:xdigit:]]\\'"
                (char-to-string character)))
    (user-error "Not a hexadecimal digit: %S" character))
  (hexl-hex-string-to-integer (char-to-string character)))

(defalias 'hexl-char-to-int #'mb-hexl-character-to-integer)

(defun mb-hexl-integer-to-character (integer)
  "Return the lowercase hexadecimal character for INTEGER."
  (unless (and (integerp integer) (<= 0 integer) (< integer 16))
    (user-error "Hexadecimal digit value must be between 0 and 15"))
  (aref (format "%x" integer) 0))

(defalias 'hexl-int-to-char #'mb-hexl-integer-to-character)

(defun mb-hexl-indexed-character (string index index-map)
  "Return character from STRING at INDEX translated by INDEX-MAP."
  (unless (= (length string) 8)
    (user-error "Indexed hexadecimal string must contain eight characters"))
  (unless (and (integerp index) (<= 0 index) (< index 8))
    (user-error "Character index must be between 0 and 7"))
  (aref string (alist-get index index-map)))

(defun mb-hexl-ith-character-big-endian (string index)
  "Return INDEXed character of STRING in reversed big-endian order."
  (mb-hexl-indexed-character string index hexl-be-byte-map-list))

(defalias 'hexl-ith-char-be
  #'mb-hexl-ith-character-big-endian)

(defun mb-hexl-ith-character-little-endian (string index)
  "Return INDEXed character of STRING in little-endian byte order."
  (mb-hexl-indexed-character string index hexl-le-byte-map-list))

(defalias 'hexl-ith-char-le
  #'mb-hexl-ith-character-little-endian)

(defun mb-hexl-byte-to-integer (hex)
  "Return the integer represented by the two-character string HEX."
  (unless (and (stringp hex)
               (string-match-p "\\`[[:xdigit:]]\\{2\\}\\'" hex))
    (user-error "Hexadecimal byte must contain exactly two digits"))
  (hexl-hex-string-to-integer hex))

(defalias 'hexl-to-int-byte #'mb-hexl-byte-to-integer)

(defun mb-hexl-remove-whitespace (string)
  "Return STRING without whitespace characters."
  (replace-regexp-in-string "[[:space:]]+" "" string))

(defalias 'remove-spaces #'mb-hexl-remove-whitespace)

(defun mb-hexl-split-to-bytes (string)
  "Convert hexadecimal STRING to a list of byte values."
  (let ((hex (mb-hexl-remove-whitespace string)))
    (unless (zerop (% (length hex) 2))
      (user-error "Hexadecimal string must contain complete bytes"))
    (let ((offset 0)
          bytes)
      (while (< offset (length hex))
        (push
         (mb-hexl-byte-to-integer
          (substring hex offset (+ offset 2)))
         bytes)
        (setq offset (+ offset 2)))
      (nreverse bytes))))

(defalias 'hexl-split-to-bytes #'mb-hexl-split-to-bytes)

(defun mb-hexl--little-endian-p (endian)
  "Return non-nil when ENDIAN denotes little-endian order."
  (cond
   ((memq endian '(le :le little :little)) t)
   ((member endian '("le" "little")) t)
   ((or (null endian)
        (memq endian '(be :be big :big))
        (member endian '("be" "big")))
    nil)
   (t
    (user-error "Unknown byte order: %S" endian))))

(defun mb-hexl-string-to-integer (string &optional endian)
  "Convert hexadecimal STRING to an integer using ENDIAN byte order.
ENDIAN defaults to big-endian and accepts `be', `big', `le', and
`little' as symbols, keywords, or strings."
  (let ((bytes (mb-hexl-split-to-bytes string))
        (little-endian (mb-hexl--little-endian-p endian))
        (result 0))
    (when little-endian
      (setq bytes (reverse bytes)))
    (dolist (byte bytes result)
      (setq result (+ (* result 256) byte)))))

(defalias 'hexl-string-to-int #'mb-hexl-string-to-integer)

(defun mb-hexl-region-to-integer (beginning end endian)
  "Interpret region from BEGINNING to END using ENDIAN byte order."
  (let ((value
         (mb-hexl-string-to-integer
          (buffer-substring-no-properties beginning end)
          endian)))
    (message "%d" value)
    value))

(defun hexl-region-to-int-le (beginning end)
  "Interpret the region as a little-endian hexadecimal integer."
  (interactive "r")
  (mb-hexl-region-to-integer beginning end :little))

(defun hexl-region-to-int-be (beginning end)
  "Interpret the region as a big-endian hexadecimal integer."
  (interactive "r")
  (mb-hexl-region-to-integer beginning end :big))

(defun mb-hexl-integer-to-string (integer)
  "Return nonnegative INTEGER as a lowercase hexadecimal string."
  (unless (and (integerp integer) (>= integer 0))
    (user-error "Hexadecimal conversion requires a nonnegative integer"))
  (format "%x" integer))

(defun hexl-int-to-hexl (integer)
  "Display and return INTEGER as a lowercase hexadecimal string."
  (interactive "nInteger: ")
  (let ((hex (mb-hexl-integer-to-string integer)))
    (message "%s" hex)
    hex))

(define-key hexl-mode-map (kbd "C-c l") #'hexl-region-to-int-le)
(define-key hexl-mode-map (kbd "C-c b") #'hexl-region-to-int-be)
(define-key hexl-mode-map (kbd "C-t") #'hexl-int-to-hexl)

(provide 'mb-hexl)

;;; mb-hexl.el ends here
