(defconst ly-source-directory (concat *shared-projects-dir* "UiO/Host-2012/2710/Eksamen/"))
(defconst ly-project-tag "-eksamen-H12")
(defconst ly-instrument-file-extension ".ily")
(defconst ly-score-file-extension ".ly")
(defconst ly-instrument-groups  '(winds brass strings))
(defconst ly-score-sections '(a b c d e))

(require 'ly-functions)

(defconst ly-instruments
  (list 
   ;;winds
   (list 'flauto
	 (ly-scheme-string "2 Flauti") 
	 (ly-scheme-string "2 Fl.")
	 (ly-scheme-string "flute")
	 'treble
	 'winds
	 nil
	 'fl)
   (list 'oboe
	 (ly-scheme-string "2 Oboi") 
	 (ly-scheme-string "2 Ob.")
	 (ly-scheme-string "oboe")
	 'treble
	 'winds
	 nil
	 'ob)
   (list 'clarinetto
	 (ly-markup (ly-column (ly-string "2 Clarinetti")
			       (ly-line (ly-string "in Si ") ly-smaller ly-flat)))
	 (ly-markup (ly-column (ly-string "2 Cl.")
			       (ly-concat (ly-string "(Si ") ly-smaller ly-flat (ly-string ")"))))
	 (ly-scheme-string "clarinet")
	 'treble
	 'winds
	 "bes,"
	 'cl)
   (list 'fagotto
	 (ly-scheme-string "2 Fagotti")
	 (ly-scheme-string "2 Fg.")
	 (ly-scheme-string "bassoon")
	 'bass
	 'winds
	 nil
	 'fg)

   ;;brass
   (list 'corno
	 (ly-scheme-string "2 Corni in Fa")
	 (ly-markup (ly-column (ly-string "2 Cor.")
			       (ly-string "(Fa)")))
	 (ly-scheme-string "french horn")
	 'treble
	 'brass
	 "f,"
	 'cor)

   ;;strings
   (list 'violinoI
	 (ly-scheme-string "Violino I")
	 (ly-scheme-string "Vl. I")
	 (ly-scheme-string "violin")
	 'treble
	 'strings
	 nil
	 'vnI)
   (list 'violinoII (ly-scheme-string "Violino II")
	 (ly-scheme-string "Vl. II")
	 (ly-scheme-string "violin")
	 'treble
	 'strings
	 nil
	 'vnII)
   (list 'viola (ly-scheme-string "Viola")
	 (ly-scheme-string "Vla")
	 (ly-scheme-string "viola")
	 'alto
	 'strings
	 nil
	 'vla)
   (list 'bassi 
	 (ly-markup (ly-column (ly-string "Violoncello e")
			       (ly-string "contrabasso")))
	 (ly-scheme-string "Bassi")
	 (ly-scheme-string "cello")
	 'bass
	 'strings
	 nil
	 'bassi)
   (list 'violoncello
	 (ly-scheme-string "Violoncello")
	 (ly-scheme-string "Vlc.")
	 (ly-scheme-string "cello")
	 'bass
	 'strings
	 nil
	 'vlc)
   (list 'contrabasso
	 (ly-scheme-string "Contrabasso")
	 (ly-scheme-string "Cb.")
	 (ly-scheme-string "contrabass")
	 'bass
	 'strings
	 nil
	 'cb))
  "List of instruments. Each instrument is represented by a list
\(symbol long-instrument-name short-instrument-name midi-instrument-name clef instrument-class)")

;;; property functions
(defun ly-instrument-symbol (instrument) (first instrument))
(defun ly-long-instrument-name (instrument) (second instrument))
(defun ly-short-instrument-name (instrument) (third instrument))
(defun ly-midi-instrument-name (instrument) (fourth instrument))
(defun ly-default-clef (instrument) (fifth instrument))
(defun ly-instrument-class (instrument) (sixth instrument))
(defun ly-instrument-key (instrument) (seventh instrument))
(defun ly-instrument-abbrevation-symobol (instrument) (eighth instrument))

(defun ly-find-instrument (instrument-symbol)
  (assoc instrument-symbol ly-instruments))
;;(ly-find-instrument 'oboa)

(defun ly-get-instrument (instrument-symbol)
  (aif (ly-find-instrument instrument-symbol)
    it (error "No such instrument as %S" instrument-symbol)))
;;(ly-get-instrument 'oboea)


(defun ly-get-long-instrument-name (instrument-symbol)
  (ly-long-instrument-name (ly-get-instrument instrument-symbol)))
;;(ly-long-instrument-name 'fagotto)

(defun ly-get-short-instrument-name (instrument-symbol)
  (ly-short-instrument-name (ly-get-instrument instrument-symbol)))

(defun ly-get-midi-instrument-name (instrument-symbol)
  (ly-midi-instrument-name (ly-get-instrument instrument-symbol)))

(defun ly-get-default-clef (&optional instrument-symbol)
  (if instrument-symbol
    (ly-default-clef (ly-get-instrument instrument-symbol))
    'treble))
;;(ly-default-clef 'fagotto)

(defun ly-get-instrument-class (instrument-symbol)
  (ly-instrument-class (ly-get-instrument instrument-symbol)))
;;(ly-get-instrument-class 'fagotto)

(defun* ly-get-instruments (instrument-class &optional (instruments ly-instruments))
  (copy-if (bind #'eq instrument-class) instruments :key #'ly-instrument-class))
;;(ly-get-instruments 'strings (nthcdr 7 ly-instruments))

(defun* ly-instrument-symbols (&optional (instruments ly-instruments))
  (mapcar #'ly-instrument-symbol instruments))
;;(ly-instrument-symbols)



(defun ly-instrument-filename (ly-instrument)
  (format "%s%s%s" (symbol-name ly-instrument) ly-project-tag ly-instrument-file-extension))
;;(ly-instrument-filename 'flauto)

(defun ly-score-filename ()
  (format "score%s%s" ly-project-tag ly-score-file-extension))
;;(ly-score-filename)

(defun ly-voices-sections-variable (instrument sections)
  (if (symbolp sections)
    (format "\\%sVoices%s"
      (ly-instrument-symbol instrument)
      (upcase (symbol-name sections)))
    (mapcar (bind #'ly-voices-sections-variable instrument 1) sections)))
;;(ly-voices-sections-variable (ly-get-instrument 'oboe) '(a b c))

(defun ly-with-staff-args (instrument)
  (list (ly-variable-definition 'instrumentName (ly-long-instrument-name instrument))
	(ly-variable-definition 'shortInstrumentName (ly-short-instrument-name instrument))
	(ly-variable-definition 'midiInstrument (ly-midi-instrument-name instrument))))
;;(ly-with-staff-args (ly-get-instrument 'clarinetto))

(defun ly-make-staff (instrument sections global-symbol)
  (ly-new-staff 
   (ly-sequential-music-lines
    (ly-clef (ly-default-clef instrument))
    (ly-simultaneous-music
     (ly-variable global-symbol)
     (ly-sequential-music*
      (ly-voices-sections-variable instrument sections))))
   (format "%S Staff" (ly-instrument-symbol instrument))
   (ly-with-staff-args instrument)))
;;(insert "\n\n" (ly-make-staff (ly-get-instrument 'fagotto) '(a b c) 'global))

(defun ly-staff-variable-name (instrument-symbol)
  (concat (symbol-name instrument-symbol) "Staff"))
;;(ly-staff-variable-name 'oboe)

(defun ly-staff-variable (instrument-symbol)
  (ly-variable (ly-staff-variable-name instrument-symbol)))
;;(ly-staff-variable 'oboe)

(defun ly-make-staff-variables (instrument-symbols sections global-symbol)
  (concat* 
   (loop for x in instrument-symbols
	 collect (ly-variable-definition 
		  (ly-staff-variable-name x)
		  (ly-make-staff (ly-get-instrument x) sections global-symbol)))
   :in "\n\n"))
;;(insert "\n\n" (ly-make-staff-variables '(flauto oboe) '(a b c) 'global))

(defun ly-make-staff-group (instrument-group instrument-symbols)
  "Returns empty string if instrument-symbols is nil"
  (when instrument-symbols
    (ly-new-staff-group 
     (loop for x in instrument-symbols
	   for key = (ly-instrument-key (ly-get-instrument x))
	   for staff-variable = (ly-staff-variable x)
	   collect (if key 
		     (ly-transpose staff-variable key)
		     staff-variable))
     (capitalize (sstring instrument-group)))))
;;(insert "\n\n" (ly-make-staff-group 'winds '(oboe clarinetto)))


(defun ly-make-staff-groups (instrument-symbols)
  (loop for group in ly-instrument-groups
	for instruments = (ly-get-instruments group (mapcar #'ly-get-instrument instrument-symbols))
	for staff-group = (ly-make-staff-group group (mapcar #'ly-instrument-symbol instruments))
	if staff-group collect staff-group))
;;(ly-make-staff-groups '(flauto oboe corno))

(defun ly-make-global-varible (sections global-symbol)
  (ly-variable-definition
   global-symbol
   (ly-sequential-music*
    (loop for x in sections
	  collect (ly-variable (concat (sstring global-symbol)
				       (capitalize (sstring x))))))))
;;(ly-make-global-varible '(a b c) 'global)

(defun ly-make-layout-score (instrument-symbols)
  (ly-new-score
   (ly-sequential-music-lines
    (ly-simultaneous-music* (ly-make-staff-groups instrument-symbols) t) 
    (ly-layout-block))))
;;(insert "\n" (ly-make-layout-score '(flauto oboe corno)))

(defun ly-make-midi-score (instrument-symbols)
  (ly-new-score 
   (ly-sequential-music-lines
    (ly-simultaneous-music* (mapcar #'ly-staff-variable instrument-symbols) t)
    (ly-midi-block))))
;;(insert "\n" (ly-make-midi-score '(flauto oboe corno violinoI)))

(defun ly-make-include (instrument-symbol)
  (ly-include (concat (symbol-name instrument-symbol)
		      ly-instrument-file-extension)))
;;(insert (ly-make-include 'oboe))

(defun ly-make-includes (instrument-symbols)
  (concat* (mapcar #'ly-make-include instrument-symbols) :in "\n"))
;;(insert "\n\n" (ly-make-includes '(oboe corno)))

(defun ly-expand-instrument-symbols (instrument-symbols)
  (if (symbolp instrument-symbols)
    (ly-expand-instrument-symbols (list instrument-symbols))
    (let ((all-symbols (ly-instrument-symbols)))
      (sort  
       (remove-duplicates 
	(loop for x in instrument-symbols
	      if (eq x 'tutti) return (mapcan #'ly-expand-instrument-symbols '(winds brass strings))
	      else if (member x ly-instrument-groups) append (mapcar #'ly-instrument-symbol (ly-get-instruments x))
	      else if (member x all-symbols) collect x))
       (explicit< all-symbols)))))
;;(ly-expand-instrument-symbols '(bassi winds strings brass tutti))
;;(ly-expand-instrument-symbols 'tutti)

(defun* ly-make-source (instrument-symbols sections &optional (global-symbol 'global))
  (let ((instrument-symbols* (ly-expand-instrument-symbols instrument-symbols)))
    (concat* 
     (list (ly-make-includes instrument-symbols*)
	   (ly-variable 'Header)
	   (ly-make-global-varible sections global-symbol)
	   (ly-make-staff-variables instrument-symbols* sections global-symbol)
	   (ly-make-layout-score instrument-symbols*)
	   (ly-make-midi-score instrument-symbols*)
	   (ly-variable 'Footer))
     :in "\n\n")))
;;;;(insert (ly-make-source 'violoncello '(c) 'global))
;;;;(insert (ly-make-source 'oboe '( e f) 'global))
;;;;(insert (ly-make-source '(violinoI violinoII) '(d e f) 'global))

(defun ly-find-part-at-buffer-position (instrument-name)
  (let ((regexp (format "%s.*Notes\\([^[[:space:]]]*\\)" instrument-name)))
    (save-excursion
      (re-search-backward regexp)
      (string-remove-props (match-string 1)))))

(defun ly-abbrevate-symbol (instrument-symbol)
  (let ((instrument (ly-find-instrument instrument-symbol)))
    (if instrument 
      (ly-instrument-abbrevation-symobol instrument)
      instrument-symbol)))
;;(mapcar #'ly-abbrevate-symbol '(flauto strings tutti))

(defun ly-make-buffer-name (instrument-symbols sections)
  "Nice to have: sort parts"
  (let ((instruments-part (concat* (mapcar #'ly-abbrevate-symbol (llist instrument-symbols))
				  :in "-" :key #'symbol-name))
	(sections-part (concat* (llist sections) :key (compose #'capitalize #'symbol-name))))
    (concat instruments-part "-" sections-part ".ly")))
;;(ly-make-buffer-name 'bassi 'a)
;;(ly-make-buffer-name '(flauto strings tutti) '(a b c))

(defun* ly-make-and-compile (instrument-symbols sections &optional (global-symbol 'global))
  (let* ((buffer-name (ly-make-buffer-name instrument-symbols sections))
	 (path (expand-file-name buffer-name ly-source-directory)))
    (find-file-noselect path)
    (switch-to-buffer buffer-name)
    (erase-buffer)
    (insert (ly-make-source (llist instrument-symbols) (llist sections) global-symbol))
    (indent-region (point-min) (point-max))
    (write-file path nil)
    (LilyPond-command-lilypond)))
;;    (switch-to-buffer (other-buffer))))
;;(ly-make-and-compile '(bassi) '(d e f))
;;(ly-make-and-compile '(strings) '(f))
;;(ly-make-and-compile 'tutti '(a d e f))
;;(ly-make-and-compile '(corno violinoI) 'c)
;;(ly-make-and-compile '(winds brass violinoI violinoII viola violoncello contrabasso) '(b c))
;;(ly-make-and-compile '(winds brass violinoI violinoII viola bassi) 'a)
;;(ly-make-and-compile '(winds brass violinoI violinoII viola violoncello contrabasso) '(a b c d e f))
;;(ly-make-and-compile '(violoncello contrabasso) '(a))
;;(ly-make-and-compile 'oboe '(a b c d))
;;(ly-make-and-compile 'violoncello '(a b c))

(defun ly-make-and-compile-part ()
  (let* ((instrument-name (file-name-sans-extension (buffer-name)))
	 (section (ly-find-part-at-buffer-position instrument-name)))
    (ly-make-and-compile (intern instrument-name) (intern section))))

(defun LilyPond-command-master ()
  "Run command on the current document."
  (interactive)
  (if (ly-ily-buffer-p)
    (ly-make-and-compile-part))
  (LilyPond-command-select-master)
  (LilyPond-command (LilyPond-command-query (LilyPond-get-master-file))
		    'LilyPond-get-master-file))

(defun ly-ily-buffer-p ()
  (string= (file-extension (buffer-name)) ly-instrument-file-extension))

(defun ly-make-and-compile ()
  (if (ly-ily-buffer-p)
    (ly-make-and-compile-part))) 

(provide 'make-source)

;;;; Optionally TODO

;;;; Auto start compiling
;;;; Kill pdf and midi process if already running
;;;; Auto start pdf and midi processes

