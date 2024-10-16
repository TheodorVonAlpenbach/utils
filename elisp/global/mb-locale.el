(cl-defun map-string-region (from to beg end)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (search-forward from end t)
	(replace-match to t nil)))))
;;(map-string-region "ø" "oe" 1 10)

(cl-defun map-strings-region (beg end &optional
				  (map *iso-latin1-encoding*)
				  (from-column 0) (to-column 1))
  (interactive "*r")
  (let ((case-fold-search nil))
   (dolist (curr (project-sequence map (list from-column to-column)))
     (awhen (first curr)
       (map-string-region it (second curr) beg (min end (point-max)))))))
;;(map-strings-region 1 100 *iso-latin1-encoding* 4 0)
;;(map-strings-region (point-min) (point-max) *iso-latin1-encoding* 4 0)

(cl-defun replace-iso-latin1-with-7bit-region (beg end) (interactive "*r")
  (map-strings-region beg end map-string-iso-latin1-to-7bit))

(cl-defun replace-iso-latin1-with-7bit-region (beg end)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((table replace-iso-latin1-with-7bit-table))
        (while table
          (let ((current (car table)))
            (goto-char (point-min))
            (while (search-forward (car current) nil t)
              (replace-match (car (cdr current)) t nil)))
          (setq table (cdr table)))))))

(cl-defun iso-latin1-2-7bit-char (iso-latin-char &optional check-error-p encoding)
  (let ((string (char-to-string iso-latin-char)))
    (aif (assoc string (or encoding *iso-latin1-encoding*))
      (second it)
      (if check-error-p
	(error "Unknown ISO-LATIN-STRING %c." iso-latin-char)
	string))))
;;(mapcar #'iso-latin1-2-7bit-char (list ?æ ?a))

(cl-defun iso-latin1-2-7bit (iso-latin-string &optional encoding)
  (cl-loop for c across iso-latin-string
	   concat (iso-latin1-2-7bit-char c)))
;;(iso-latin1-2-7bit "ææ")

(defconst *iso-latin1-encoding*
  '((" " " " "%20" -1)
    ("\\[" "[" "%5B" -1)
    ("\\]" "]" "%5D" -1)
    ("'" "'" "%20" 39)
    ("'" "'" "%20" 39 "ÃÂ´")
    ("+" "+" "%20" 192)
    ("À" "A" "%C0" 192)
    ("Á" "A" "%C1" 193)
    ("Â" "A" "%C2" 194)
    ("Ã" "A" "%C3" 195)
    ("Ä" "Ae" "%C4" 196)
    ("Å" "Aa" "%C5" 197 "Ã")
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
    ("Ø" "Oe" "%D8" 216 "ÃÂ")
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
    ("å" "aa" "%E5" 229 "Ã¥")
    ("å" "aa" "%E5" 229 "ÃÂ¥")
    ("æ" "ae" "%E6" 230 "Ã¦")
    ("æ" "ae" "%E6" 230 "ÃÂ¦")
    ("ç" "c" "%E7" 231)
    ("è" "e" "%E8" 232)
    ("é" "e" "%E9" 233 "ÃÂ©")
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
    ("ö" "oe" "%F6" 246 "Ã¶")
    ("÷" "%" "%F7" 247)
    ("ø" "oe" "%F8" 248 "Ã¸")
    ("ø" "oe" "%F8" 248 "ÃÂ¸")
    ("ù" "u" "%F9" 249)
    ("ú" "u" "%FA" 250)
    ("û" "u" "%FB" 251)
    ("ü" "ue" "%FC" 252 "ÃÂ¼")
    ("ý" "y" "%FD" 253)
    ("þ" "th" "%FE" 254)
    ("ÿ" "y" "%FF" 255)
    ("..." "..." nil nil "Ã¢ÂÂ¦")
    ("..." "..." nil nil "â¦")))

(provide 'mb-locale)
