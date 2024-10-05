(require 'mb-utils-strings)

(cl-defun ly-string (symbol-or-string)
  (format "\"%s\"" (sstring symbol-or-string)))
;;(insert (ly-string 'qwe))

(cl-defun ly-scheme (string)
  (format "#%s" string))
;;(insert (ly-scheme 1))#1

(cl-defun ly-scheme-string (string)
  (ly-scheme (ly-string string)))
;;(insert (ly-scheme-string 'qwe))

(cl-defun ly-variable (variable-symbol-or-string)
  (format "\\%s" (sstring variable-symbol-or-string)))
;;(insert (ly-variable 'qwe))
;;(insert (ly-variable "qwe"))

(cl-defun ly-markup (markup)
  (format "%s {
%s
}" (ly-variable 'markup) markup))
;;(insert (ly-markup "string"))

(cl-defun ly-list (list &optional with-line-breaks)
  (concat* list 
	   :pre (if with-line-breaks "{\n" "{ ")
	   :in (if with-line-breaks "\n" " ")
	   :suf (if with-line-breaks "\n}" " }")))
;;(insert (ly-list (list "qwe" "qwe" "qwe") t))

(cl-defun ly-sequential-music* (list &optional with-line-breaks) 
  (ly-list list with-line-breaks))
;;(ly-sequential-music* (list "a" "b"))

(cl-defun ly-sequential-music (&rest musics) 
  (ly-sequential-music* musics))
;;(ly-sequential-music "a" "b")

(cl-defun ly-sequential-music-lines (&rest musics) 
  (ly-sequential-music* musics t))
;;(ly-sequential-music-lines "a" "b")


(cl-defun ly-simultaneous-music* (list &optional with-line-breaks)
  (concat* list 
	   :pre (if with-line-breaks "<<\n" "<< ")
	   :in (if with-line-breaks "\n" " ")
	   :suf (if with-line-breaks "\n>>" " >>")))
;;(insert (ly-simultaneous-music (list "qwe" "qwe" "qwe") t))

(cl-defun ly-simultaneous-music (&rest musics)
  (ly-simultaneous-music* musics))

(cl-defun ly-simultaneous-music-lines (&rest musics)
  (ly-simultaneous-music* musics t))
;;(ly-simultaneous-music-lines "a" "b")

(cl-defun ly-line (&rest markups)
  (concat "\\line " (ly-list markups)))
;;(insert (ly-line "qwe" "qwe" "qwe"))

(cl-defun ly-concat (&rest markups)
  (concat "\\concat " (ly-list markups)))
;;(insert (ly-concat "qwe" "qwe" "qwe"))

(cl-defun ly-column (&rest columns)
  (concat "\\column " (ly-list columns t)))
;;(insert (ly-column (ly-string "string") (ly-string "string")))

(defconst ly-flat (ly-variable 'flat))
(defconst ly-smaller (ly-variable 'smaller))
(defconst ly-include (ly-variable 'include))

(cl-defun ly-clef (clef-symbol)
  (format "\\clef %S" clef-symbol))
;;(insert (ly-clef 'bass))

(cl-defun ly-transpose (music from-note &optional (to-note "c"))
  (format "%s %s %s %s" (ly-variable 'transpose) from-note to-note music))
;;(insert (ly-transpose "\\hornStaff" "f,"))

(cl-defun ly-include (filename)
  (format "%s %s" ly-include (ly-string filename)))
;;(insert "\n\n" (ly-include "oboe.ily"))

(cl-defun ly-layout-block ()
  "Only empty layout object"
  (format "%s {}" (ly-variable 'layout)))
;;(ly-layout-block)
  
(cl-defun ly-midi-block ()
  "Only empty layout object"
  (format "%s {}" (ly-variable 'midi)))
;;(ly-midi-block)
  

(cl-defun ly-variable-definition (name value)
  (format "%s = %s" (sstring name) value))
;;(insert (ly-variable-definition 'qwe (ly-string "string")))

(cl-defun ly-with (variable-definitions)
  (concat "\\with " (ly-list variable-definitions t)))
;;(insert (ly-with (list (ly-string "string") (ly-string "string"))))

(cl-defun ly-new-staff (music &optional name with)
  "Optional NAME should be a string, WITH is a list of variable definitions"
  (format "\\new Staff%s%s\n%s"
    (if name (format " = %s" (ly-string name)) "")
    (if with (concat " " (ly-with with)) "")
    music))
;;(insert (ly-new-staff "{ c e g }" "Qwe StafF" (list (ly-variable-definition 'instrumentName "Trumpet") (ly-variable-definition 'instrumentName "Pipe"))))

(cl-defun ly-new-staff-group (staffs &optional name with)
  "Optional NAME should be a string, WITH is a list of variable definitions"
  (format "\\new StaffGroup%s%s\n%s"
    (if name (format " = %s" (ly-string name)) "")
    (if with (concat " " (ly-with with)) "")
    (ly-simultaneous-music* staffs t)))
;;(insert "\n\n" (ly-new-staff-group (list (ly-new-staff "{ c e g }" "Qwe StafF" (list (ly-variable-definition 'instrumentName "Trumpet") (ly-variable-definition 'instrumentName "Pipe"))) (ly-new-staff "{ c e g }" "Ewq StafF" (list (ly-variable-definition 'instrumentName "Horn") (ly-variable-definition 'instrumentName "Pipe")))) "Qwe StafF Group"))

(cl-defun ly-new-score (sequential-music)
  (format "%s %s" (ly-variable 'score) sequential-music))
;;(insert "\n\n" (ly-new-score (ly-sequential-music '("c" "e" "g") t)))

(provide 'ly-functions)