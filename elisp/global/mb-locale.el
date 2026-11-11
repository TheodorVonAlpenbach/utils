;;; mb-locale.el --- Latin-1 transliteration helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst *iso-latin1-encoding*
  '((" " " " "%20" 32)
    ("[" "[" "%5B" 91)
    ("]" "]" "%5D" 93)
    ("'" "'" "%27" 39)
    ("+" "+" "%2B" 43)
    ("À" "A" "%C0" 192)
    ("Á" "A" "%C1" 193)
    ("Â" "A" "%C2" 194)
    ("Ã" "A" "%C3" 195)
    ("Ä" "Ae" "%C4" 196)
    ("Å" "Aa" "%C5" 197)
    ("Æ" "Ae" "%C6" 198)
    ("Ç" "C" "%C7" 199)
    ("È" "E" "%C8" 200)
    ("É" "E" "%C9" 201)
    ("Ê" "E" "%CA" 202)
    ("Ë" "E" "%CB" 203)
    ("Ì" "I" "%CC" 204)
    ("Í" "I" "%CD" 205)
    ("Î" "I" "%CE" 206)
    ("Ï" "I" "%CF" 207)
    ("Ð" "D" "%D0" 208)
    ("Ñ" "N" "%D1" 209)
    ("Ò" "O" "%D2" 210)
    ("Ó" "O" "%D3" 211)
    ("Ô" "O" "%D4" 212)
    ("Õ" "O" "%D5" 213)
    ("Ö" "Oe" "%D6" 214)
    ("×" "x" "%D7" 215)
    ("Ø" "Oe" "%D8" 216)
    ("Ù" "U" "%D9" 217)
    ("Ú" "U" "%DA" 218)
    ("Û" "U" "%DB" 219)
    ("Ü" "Ue" "%DC" 220)
    ("Ý" "Y" "%DD" 221)
    ("Þ" "Th" "%DE" 222)
    ("ß" "ss" "%DF" 223)
    ("à" "a" "%E0" 224)
    ("á" "a" "%E1" 225)
    ("â" "a" "%E2" 226)
    ("ã" "a" "%E3" 227)
    ("ä" "ae" "%E4" 228)
    ("å" "aa" "%E5" 229)
    ("æ" "ae" "%E6" 230)
    ("ç" "c" "%E7" 231)
    ("è" "e" "%E8" 232)
    ("é" "e" "%E9" 233)
    ("ê" "e" "%EA" 234)
    ("ë" "e" "%EB" 235)
    ("ì" "i" "%EC" 236)
    ("í" "i" "%ED" 237)
    ("î" "i" "%EE" 238)
    ("ï" "i" "%EF" 239)
    ("ð" "d" "%F0" 240)
    ("ñ" "n" "%F1" 241)
    ("ò" "o" "%F2" 242)
    ("ó" "o" "%F3" 243)
    ("ô" "o" "%F4" 244)
    ("õ" "o" "%F5" 245)
    ("ö" "oe" "%F6" 246)
    ("÷" "/" "%F7" 247)
    ("ø" "oe" "%F8" 248)
    ("ù" "u" "%F9" 249)
    ("ú" "u" "%FA" 250)
    ("û" "u" "%FB" 251)
    ("ü" "ue" "%FC" 252)
    ("ý" "y" "%FD" 253)
    ("þ" "th" "%FE" 254)
    ("ÿ" "y" "%FF" 255))
  "Latin-1 mappings.
Columns contain the source string, ASCII transliteration,
percent-encoded byte, and character code.")

(defun mb-locale-map-string-region (from to beginning end)
  "Replace literal FROM with TO between BEGINNING and END."
  (unless (and (stringp from) (not (string-empty-p from)))
    (user-error "Replacement source must be a nonempty string"))
  (let ((case-fold-search nil)
        (end-marker (copy-marker end t)))
    (unwind-protect
        (save-excursion
          (goto-char beginning)
          (while (search-forward from end-marker t)
            (replace-match to t t)))
      (set-marker end-marker nil))))

(defalias 'map-string-region #'mb-locale-map-string-region)

(cl-defun mb-locale-map-strings-region
    (beginning end
     &optional
     (map *iso-latin1-encoding*)
     (from-column 0)
     (to-column 1))
  "Apply MAP replacements between BEGINNING and END.
FROM-COLUMN and TO-COLUMN select source and replacement values
from each mapping row."
  (dolist (row map)
    (let ((from (nth from-column row))
          (to (nth to-column row)))
      (when (and from to)
        (mb-locale-map-string-region from to beginning end)))))

(defalias 'map-strings-region #'mb-locale-map-strings-region)

(defun mb-locale-replace-iso-latin1-region (beginning end)
  "Transliterate Latin-1 characters between BEGINNING and END."
  (interactive "*r")
  (mb-locale-map-strings-region beginning end))

(defalias 'replace-iso-latin1-with-7bit-region
  #'mb-locale-replace-iso-latin1-region)

(defun mb-locale--encoding-entry (character encoding)
  "Return CHARACTER's row in ENCODING."
  (assoc (char-to-string character) encoding))

(defun mb-locale-character-to-ascii
    (character &optional check-error-p encoding)
  "Transliterate CHARACTER using ENCODING.
ENCODING defaults to `*iso-latin1-encoding*'.  Return unchanged
characters as one-character strings.  With CHECK-ERROR-P non-nil,
report an error when CHARACTER has no mapping."
  (unless (characterp character)
    (user-error "Expected a character: %S" character))
  (let* ((table (or encoding *iso-latin1-encoding*))
         (entry (mb-locale--encoding-entry character table)))
    (cond (entry (cadr entry))
          (check-error-p
           (user-error "No transliteration for character: %c" character))
          (t (char-to-string character)))))

(defalias 'iso-latin1-2-7bit-char
  #'mb-locale-character-to-ascii)

(defun mb-locale-string-to-ascii (string &optional encoding)
  "Transliterate STRING using ENCODING."
  (mapconcat
   (lambda (character)
     (mb-locale-character-to-ascii character nil encoding))
   string ""))

(defalias 'iso-latin1-2-7bit #'mb-locale-string-to-ascii)

(provide 'mb-locale)

;;; mb-locale.el ends here
