(provide 'mb-hexl)

(require 'cl)
(require 'hexl)

(defvar hex-to-int-list 
  '((?0 . 0)
    (?1 . 1)
    (?2 . 2)
    (?3 . 3)
    (?4 . 4)
    (?5 . 5)
    (?6 . 6)
    (?7 . 7)
    (?8 . 8)
    (?9 . 9)
    (?a . 10)
    (?b . 11)
    (?c . 12)
    (?d . 13)
    (?e . 14)
    (?f . 15)))

(defun hexl-char-to-int (char) "" (cdr (assoc char hex-to-int-list)))
(defun hexl-int-to-char (int) "" (car (rassoc char hex-to-int-list)))

(defvar hexl-be-byte-map-list
  '((0 . 7) (1 . 6) (2 . 5) (3 . 4) (4 . 3) (5 . 2) (6 . 1) (7 . 0)))

(defvar hexl-le-byte-map-list
  '((0 . 1) (1 . 0) (2 . 3) (3 . 2) (4 . 5) (5 . 4) (6 . 7) (7 . 6)))

(defun hexl-ith-char-be (s i) "returns char I of S in BE"
  (aref s (cdr (assoc i hexl-be-byte-map-list))))

(defun hexl-ith-char-be (s i) "returns char I of S in LE"
  (aref s (cdr (assoc i hexl-le-byte-map-list))))

(defun 2* (number) "Return NUMBER * 2. Should become macro" (* 2 number))
(defun 2+ (number) "Return NUMBER + 2. Should become macro" (+ 2 number))

(defun hexl-to-int-byte (hex)
  "Returns HEX, which must be a string of two characters, as int."
  (+ (* 16 (hexl-char-to-int (aref hex 0)))
     (hexl-char-to-int (aref hex 1))))

(defun hexl-split-to-bytes (s) "Splits abcd to '((ab) (bc))."
  (let ((bytes) (len (/ (length s) 2)))
    (dotimes (i len bytes)
      (setq bytes (cons (hexl-to-int-byte
			 (substring s (2* i) (2+ (2* i)))) bytes)))))

(defun hexl-string-to-int (string endian)
  "Converts little-endian 4 B hexadecimal integer string to int.
Requires thus that string is 8 characters long
Ex: \"abcd efgh\" equals integer badcfehg with base 16"
  (let ((bytes (hexl-split-to-bytes (remove-spaces string))) (sum 0))
    (if (string= endian "le") (setq bytes (reverse bytes)))
    (dotimes (i (length bytes) sum)
      (setq sum (+ sum (* (expt 256 i) (elt bytes i)))))))

;(hexl-string-to-int  "0000 0030")

(defun remove-spaces (string)
  "Returns copy of STRING with space characters removed."
  (remove 32 string))

(defun hexl-region-to-int-le (beg end)
  "Interprets region as hexl-string and converts it to LE integer"
  (interactive "r")
  (let ((s (buffer-substring beg end)))
    (message "%d" (hexl-string-to-int s "le"))))

(defun hexl-region-to-int-be (beg end)
  "Interprets region as hexl-string and converts it to BE integer"
  (interactive "r")
  (let ((s (buffer-substring beg end)))
    (message "%d" (hexl-string-to-int s "be"))))

(define-key hexl-mode-map "\C-cl" 'hexl-region-to-int-le)
(define-key hexl-mode-map "\C-cb" 'hexl-region-to-int-be)

;other stuff

(defun mb-point () "" (interactive) (message "%d" (point)))
(defun mb-buffer-substring (beg end) "" (interactive "r") 
  (message "%s" (buffer-substring beg end)))

;;(hexl-string-to-int (remove 32 "0000 270a") "be")
;;(hexl-string-to-int "4dd6" "le")

;;(setq idar "idar")
;;(append idar nil)

(defun hexl-int-to-hexl (int) "Converts int to hex string"
  (interactive "ninteger: ")
  (message "%x" int))

(define-key hexl-mode-map "\C-t" 'hexl-int-to-hexl)

;(hexl-int-to-hexl 100) => "64"
;(hexl-int-to-hexl 10) => "a"
    
;(hexl-string-to-int-le "0000270a") => 7471104
;(hexl-string-to-int-be "000389b4") => 231860
;(hexl-string-to-int-be "0071")
;(hexl-string-to-int-be "0077")
;(hexl-string-to-int-be "0065")
;(hexl-string-to-int-le "1a") => 161
;(hexl-string-to-int-be "1a") => 26
;(hexl-string-to-int-be "0001c4da") => 115930 (* 115930 2) => 231860

;(debug-on-entry 'hexl-string-to-int-be)
;(hexl-string-to-int-le "03000000") 3+0+0+0
;(hexl-string-to-int-be "0100") 256 + 0
;(hexl-split-to-bytes "0102")
;(reverse (hexl-split-to-bytes "0102"))
;(hexl-to-int-byte "01")
;(hexl-char-to-int (aref "0a" 1))
;(hexl-string-to-int "03 00 00 00" nil) 3+0+0+0
