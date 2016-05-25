;;http://lilypond.org/doc/v2.12/Documentation/index.html
(require 'movement)

(defun lp-string (string) (format "\"%s\"" string))

(defun lp-variable (name)
  (format "\\%s" (if (symbolp name) (symbol-name name) name)))

(defun lp-fixed-arguments-function (name &rest lp-args)
  (format "\%s %s" (lp-variable name) (concat* lp-args)))

(defun lp-inline-function (name &rest lp-args)
  (format "\%s { %s }" (lp-variable name) (concat* lp-args)))

(defun lp-function (name &rest lp-args)
  (format "\%s {%s}" (lp-variable name) (concat* lp-args :pre "\n" :in "\n" :suf "\n")))

(defun* lp-version (&optional (version 2) (revision 12) (patch 4))
  (lp-fixed-arguments-function 'version 
			       (lp-string (format "%d.%d.%d" version revision patch))))
;;(lp-version)

(defun lp-fill-line (&rest lp-strings)
  (apply #'lp-function 'fill-line lp-strings))
;;(lp-fill-line "qwe" "qwe")

(defun lp-line (markup)
  (lp-fixed-arguments-function 'line markup))

(defun lp-smallcaps (&rest strings)
  (apply #'lp-inline-function 'smallCaps strings))
;;(lp-smallcaps "qwe")

(defun lp-markup (string)
  ;;(lp-simple-function 'markup string)
  (lp-function 'markup string))
;;(lp-markup (lp-fill-line "qwe" "qwe"))

(defun lp-mode (mode)
  (lp-variable (symbol-name mode)))

(defun lp-key (key)
  (let ((key* (or key (k-new))))
    (format "%s %s %s" 
      (lp-variable 'key)
      (chrome-to-string (k-root key*))
      (lp-mode (k-mode key*)))))

(defun lp-time (time-signature)
  (format "%s %d/%d" 
    (lp-variable 'time)
    (ts-length time-signature)
    (ts-unit time-signature)))

(defun lp-clef (clef)
  (format "%s %S" (lp-variable 'clef) clef))

(defun lp-relative (absolute-pitch music)
  (format "%s = %s {\n%s\n}" (lp-variable 'relative) (p-to-string pitch) music))

(defun lp-define-variable (name value)
  (format "%s = %s \n" name (if (listp value) (lp-list value) value)))

(defun lp-list (list)
  (concat* list :pre "{ " :in " " :suf " }"))
;;(lp-list '())

(defun lp-simultanous-music (&rest musics)
  (concat*  musics :pre "<< \n" :in " " :suf ">> \n"))

(defun lp-simultanous-music* (musics)
  (concat*  musics :pre "<< \n" :in "\n\\\\\n" :suf ">> \n"))

(defun lp-sequential-music (&rest musics)
  (concat* musics :pre "{ \n" :in " " :suf "\n}"))

(defun lp-staff-group (&rest staffs)
  (format "\\new StaffGroup %s" (apply #'lp-simultanous-music staffs)))

(defun lp-choir-staff (&rest staffs)
  (format "\\new ChoirStaff %s" (apply #'lp-simultanous-music staffs)))

(defun lp-staff (music &optional name)
  (format "\\new Staff %s" music))

(defun lp-bar (type)
  (format "%s \"%s\"" (lp-variable 'bar) type))

(defun lp-voice (music &optional name)
  (if name 
    (format "\\new Voice = \"%s\" { \n %s \n } \n" name music)
    (format "\\new Voice %s" music)))
;;(insert (lp-voice "qwe" "qwe"))

(defun* lp-upper-voice-context (&rest musics)
  (format "\\voiceOne %s" (concat*  musics :in " ")))

(defun* lp-lower-voice-context (&rest musics)
  (format "\\voiceTwo %s" (concat*  musics :in " ")))

(defun lp-score (score-body)
  (format "%s {\n %s }" (lp-variable 'score) score-body))
;;(lp-score "a")

(defun* lp-upbeat (upbeat)
  (when upbeat (format "%s %s" (lp-variable 'partial) (d-to-string upbeat 'lilypond))))
;;(lp-upbeat 1.0)

(defun collect-globals (movement)
  (let ((gl (list (lp-key (mvt-key movement))
		  (lp-time (mvt-time-signature movement)))))
    (awhen (mvt-upbeat movement) (push-back (lp-upbeat it) gl))))

(defun* mvt-lp-voice (mvt n &optional (with-repeats t))
  "Converts the Nth voice of MVT to lilyond format text, including repeat patterns if WITH-REPEATS is not nil.
Assumes only one repetition pattern in mvt"
  (let ((voice (nth n (vg-voices (mvt-voice-group mvt)))))
    (lp-sequential-music 
     (if (mvt-repeats mvt)
       (concat* (mapcar (compose #'notes-to-string #'v-notes)
			(v-split-at-mposition voice (mvt-bar-position mvt (second (first (mvt-repeats mvt))))))
		:in (format "\n%s\n" (lp-bar ':|)))
       (notes-to-string (v-notes voice)))
     (lp-bar '|.))))

(defun* v-to-lp-voice (voice repeats &optional (clef 'treble))
  "Converts the Nth voice of MVT to lilyond format text, including repeat patterns if WITH-REPEATS is not nil.
Assumes only one repetition pattern in mvt"
  (lp-sequential-music
   (lp-clef clef)
   (if repeats
     (concat* (mapcar (compose #'notes-to-string #'v-notes)
		      (v-split-at-mposition voice (v-bar-position voice (second (first repeats)))))
	      :in (format "\n%s\n" (lp-bar ':|)))
     (notes-to-string (v-notes voice)))
   (lp-bar '|.)))

(defun vg-to-lp-choir-staff (vg global repeats)
  (lp-choir-staff 
   (lp-staff (lp-simultanous-music*
	      (list (v-to-lp-voice (first (vg-voices vg)) repeats)
		    (v-to-lp-voice (second (vg-voices vg)) repeats)
		    global)))
   (lp-staff (lp-simultanous-music*
	      (list (v-to-lp-voice (third (vg-voices vg)) repeats)
		    (v-to-lp-voice (fourth (vg-voices vg)) repeats 'bass)
		    global)))))

(defun* movement-to-lilypond-string (movement title &optional (style 'lp-choral-style))
  "Converts MU MOVEMENT to lilypond format and writes result to FILE.
Currently only LP-CHORAL-STYLE style is supported"
  (let ((mu-default-print-style 'lilypond))
    (unless (eq style 'lp-choral-style)
      (error "Only style LP-CHORAL-STYLE is supported!"))
    (let* ((title-line (and title (lp-markup (lp-line (format "{%s}" title)))))
	   ;; (lp-markup (lp-fill-line (lp-line (lp-smallcaps title)))) 
	   (header-lines
	    (if title-line 
	      (list (lp-version) title-line)
	      (list (lp-version))))
	   (global (concat* (collect-globals movement) :in " "))
	   (header (concat* header-lines :in "\n"))
	   (vgs (mvt-voice-groups movement))
	   (staff-groups (loop for vg in vgs
			       collect (vg-to-lp-choir-staff vg global (mvt-repeats movement))))
	   (score (if (= (length vgs) 1)
		    (lp-score (first staff-groups))
		    (lp-score (apply #'lp-staff-group staff-groups)))))
      (concat header score))))
;;(movement-to-lilypond-string qwe "qwe")
;;(movement-to-lilypond qwe :title "qwe" :file "c:/Documents and Settings/matsb/My Documents/data/ly/qwe.ly")
;;(setq vgqwe (mvt-voice-group (mvt-submovement (mvt-test) 5 7)))
;;(setq qwe (mvt-create (list vgqwe vgqwe)))

(defun* movement-to-lilypond-string-old (movement title &optional (style 'lp-choral-style))
  "Converts MU MOVEMENT to lilypond format and writes result to FILE.
Currently only LP-CHORAL-STYLE style is supported"
  (let ((mu-default-print-style 'lilypond))
    (unless (eq style 'lp-choral-style)
      (error "Only style LP-CHORAL-STYLE is supported!"))
    (let ((l (list (lp-version)
		   ""
		   (lp-markup (lp-fill-line (lp-line (lp-smallcaps title))))
		   ""
		   (lp-define-variable "global" (collect-globals movement))
		   (lp-define-variable "sopranoMusic" (mvt-lp-voice movement 0))
		   (lp-define-variable "altoMusic" (mvt-lp-voice movement 1))
		   (lp-define-variable "tenorMusic" (mvt-lp-voice movement 2))
		   (lp-define-variable "bassMusic" (mvt-lp-voice movement 3))
		   (lp-score
		    (lp-choir-staff (lp-staff 
				     (lp-simultanous-music (lp-variable "global") 
							   (lp-voice (lp-upper-voice-context (lp-variable "sopranoMusic")
											     (lp-bar "|."))
								     "soprani")
							   (lp-voice (lp-lower-voice-context (lp-variable "altoMusic"))
								     "alti"))
				     "SA")
				    (lp-staff 
				     (lp-simultanous-music (lp-variable "global")
							   (lp-clef 'bass)
							   (lp-voice (lp-upper-voice-context (lp-variable "tenorMusic"))
								     "tenori")
							   (lp-voice (lp-lower-voice-context (lp-variable "bassMusic"))
								     "bassi"))
				     "TB"))))))
      (concat* l :in "\n"))))
;;(movement-to-lilypond-string (mvt-test) "qwe")

(setf compilation-exit-message-function
      (lambda (status code msg)
	;; If M-x compile exists with a 0
	(when (and (eq status 'exit) (zerop code))
	  ;; then bury the *compilation* buffer, so that C-x b doesn't go there
	  (bury-buffer "*compilation*")
	  ;; and return to whatever were looking at before
	  (replace-buffer-in-windows "*compilation*")
	  (pdf-view (string-replace ".ly" ".pdf")))
	;; Always return the anticipated result of compilation-exit-message-function
	(cons msg code)))

(require 'lilypond-mode)
(defun* movement-to-lilypond (movement &key 
				       (title nil) 
				       (file (format "%s%s.ly" temporary-file-directory (md5 (or title "Unknown"))))
				       (style 'lp-choral-style)
				       (start t)
				       (view start))
  "Converts MU MOVEMENT to lilypond format and writes result to FILE.
Currently only LP-CHORAL-STYLE style is supported
TODO: make a similar function that operates on temporary files"
  
  (with-temp-buffer 
    (cd (file-name-directory file))
    (insert (movement-to-lilypond-string movement title style))
    (indent-region (point-min) (point-max))
    (setq buffer-file-name file)
    (save-buffer)
    (LilyPond-mode)
    (when start
      (lexical-let ((file file))
	(setf compilation-exit-message-function
	      (lambda (status code msg)
		;; If M-x compile exists with a 0
		(when (and (eq status 'exit) (zerop code))
		  ;; then bury the *compilation* buffer, so that C-x b doesn't go there
		  (bury-buffer "*LilyPond-compile*")
		  ;; and return to whatever were looking at before
		  (replace-buffer-in-windows "*LilyPond-compile*")
		  (pdf-view (string-replace file ".ly$" ".pdf")))
		;; Always return the anticipated result of compilation-exit-message-function
		(cons msg code))))
      (LilyPond-command-lilypond))))

(require 'midi-conversion)
(provide 'lilypond-conversion)
;;(setq mats (midi-file-to-movement (test-midi)))
;;(movement-to-lilypond (mvt-test) "qwe" "c:/Documents and Settings/matsb/My Documents/projects/UiO/project/bach-chorals/elisp/qwe.ly")
;;(movement-to-lilypond (midi-file-to-movement (test-midi 1)) "qwe" "c:/Documents and Settings/matsb/My Documents/projects/UiO/project/bach-chorals/elisp/qwe.ly")
;;(movement-to-lilypond mats "qwe" "c:/Documents and Settings/matsb/My Documents/projects/UiO/project/bach-chorals/elisp/qwe.ly")
