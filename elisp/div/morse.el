(defconst +morse-unit-length+ 300 "milliseconds")
(defconst +morse-dit+ (* 1 +morse-unit-length+))
(defconst +morse-dat+ (* 3 +morse-unit-length+))
(defconst +morse-sign-space+ (* 1 +morse-unit-length+))
(defconst +morse-char-space+ (* 3 +morse-unit-length+))
(defconst +morse-word-space+ (* 7 +morse-unit-length+))

(defconst +morse-code-international+
  '((?a ".-") (?b "-...") (?c "-.-.") (?d "-..") (?e ".") (?f "..-.") (?g "--.")
    (?h "....") (?i "..") (?j ".---") (?k "-.-") (?l ".-..") (?m "--")
    (?n "-.") (?o "---") (?p ".--.") (?q "--.-") (?r ".-.") (?s "...") (?t "-")
    (?u "..-") (?v "...-") (?w ".--") (?x "-..-") (?y "-.--") (?z "....")
    (?0 "-----") (?1 ".----") (?2 "..---") (?3 "...--") (?4 "....-")
    (?5 ".....") (?6 "-....") (?7 "--...") (?8 "---..") (?9 "----.")))

(defun sign-to-morse (sign)
  (case (ssymbol sign)
    (?. dit-length)
    (?- dash-length)
    (otherwise (error "Unknown morse sign %c" sign))))
;;(mapcar #'sign-to-morse '(?. ?-))

(defun morse-char-to-sign (char)
  (second (cl-find char +morse-code-international+ :key #'car)))
;;(morse-char-to-sign ?a)

(defun char-to-morse (char)
  (infix-list
   (mapcar #'sign-to-morse (string-to-list (morse-char-to-sign char)))
   +morse-sign-space+))
;;(char-to-morse ?a)

(defun word-to-morse (word)
  (flatten
   (infix-list (mapcar #'char-to-morse word) (list +morse-char-space+))))
;;(word-to-morse "be")

(defun string-to-morse (string)
  (flatten (infix-list (mapcar #'word-to-morse (split-string string))
		       (list +morse-word-space+))))
;;(string-to-morse "ae  be ")

(defun morsebeep (morse-string)
  (let ((durations (mapcar #'sstring (string-to-morse morse-string))))
    (concat* (loop for (off on) in (cut (rest durations))
		   collect (format "-D %s -l %s" off on))
      :pre (format "beep -l %s " (car durations))
      :in " ")))
;;(morsebeep "ae  be ")
(loop for (a b) in '((1 2) (3 2)) collect a)

(provide 'morse)
