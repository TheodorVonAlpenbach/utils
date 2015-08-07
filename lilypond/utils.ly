%%http://lilypond.org/doc/v2.15/Documentation/notation-big-page#creating-contexts

#(define-markup-command (vspace layout props amount) (number?)
  "This produces a invisible object taking vertical space."
  (let ((amount (* amount 3.0)))
    (if (> amount 0)
        (ly:make-stencil "" (cons -1 1) (cons 0 amount))
        (ly:make-stencil "" (cons -1 1) (cons amount amount)))))

#(define (trill-event-chord event-chord)
   "Adds a trill grace to EVENT-CHORD."
   (append! (ly:music-property event-chord 'elements)
	    (list (make-music 'ArticulationEvent 'articulation-type "trill"))))

#(define (slur-event-chord event-chord dir)
   "Adds a slur to EVENT-CHORD. DIR specifies direction, -1 for left, 1 for right."
   (append! (ly:music-property event-chord 'elements)
	    (list (make-music 'SlurEvent 'span-direction dir))))

#(define (slur-event-chord-list event-chords)
     "Adds a slur around EVENT-CHORDS. DIR specifies direction."
     (slur-event-chord (car event-chords) -1)
     (slur-event-chord (car (last-pair event-chords)) 1)
     event-chords)

#(define slur
   (define-music-function (parser location sequential-music) (ly:music?)
     "Adds a slur around SEQUENTIAL-MUSIC."
     (slur-event-chord-list (ly:music-property sequential-music 'elements))
     sequential-music))

%duration
#(define (set-duration-note! note log count)
   "Sets duration of note-event NOTE"
   (set! (ly:music-property note 'duration)
	 (ly:make-duration log count 1 1)))

#(define (set-duration-event-chord! event-chord log count)
   "Sets duration for all notes in EVENT-CHORD to LOG/COUNT"
   (set-duration-note! (car (ly:music-property event-chord 'elements)) log count))

#(define mbTr
   (define-music-function (parser location n1 n2 n3) (ly:music? ly:music? ly:music?)
     "Converts input event-chords n1 n2 n3 to sequential-music:
n1 8. \trill (  
n2 32  
n3 32 )"
     (set-duration-event-chord! n1 3 1)
     (trill-event-chord n1)
     (slur-event-chord n1 -1)
     (set-duration-event-chord! n2 5 0)
     (set-duration-event-chord! n3 5 0)
     (slur-event-chord n3 1)
     (make-music 'SequentialMusic 'elements (list n1 n2 n3))))

#(define mbFig
   (define-music-function (parser location n1 n2 n3) (ly:music? ly:music? ly:music?)
     "Same as 'mbTr', but without the trill"
     (set-duration-event-chord! n1 3 1)
     (slur-event-chord n1 -1)
     (set-duration-event-chord! n2 5 0)
     (set-duration-event-chord! n3 5 0)
     (slur-event-chord n3 1)
     (make-music 'SequentialMusic 'elements (list n1 n2 n3))))

#(define-markup-command (instrumentBox layout props text) (markup?)
  "Draw a double box around text."
  (interpret-markup layout props 
		    (markup #:left-column (#:vspace 1
					   #:override '(box-padding . 1)
					   #:box text
					   #:vspace 1))))

#(define-markup-command (dynamicsDescribed layout props dynamics description) (markup? markup?)
  "Makes dynamics with additional description, like 'p espressivo'."
  (interpret-markup layout props 
   (markup #:line
	  (#:normal-text
	   #:dynamic dynamics
	   #:fontsize 1.5 #:italic description))))

#(define-markup-command (dynamicsDescribedReversed layout props description dynamics) (markup? markup?)
  "Makes dynamics with additional description, like 'p espressivo'."
  (interpret-markup layout props 
   (markup #:line
	  (#:normal-text
	   #:fontsize 1.5 #:italic description
 	   #:dynamic dynamics))))

#(define stemDownOnce
   (define-music-function (parser location sequential-music) (ly:music?)
     "Turns off auto beaming for music argument"
    #{ \stemDown $sequential-music \stemNeutral #} ))

#(define-markup-command (myCaps layout props text) (markup?)
   "Shortcut for setting TEXT in DTLAlbertinaTCaps"
   (interpret-markup layout props
     (markup #:override '(font-name . "DTLAlbertinaTCaps") text)))

#(define-markup-command (myCapsTest layout props text) (markup?)
   "Shortcut for setting TEXT in DTLAlbertinaTCaps"
   (display (ly:output-def-lookup layout 'fonts))
   (display props)
   (interpret-markup layout props
     (markup #:override '(font-name . "DTLAlbertinaTCaps") text)))
