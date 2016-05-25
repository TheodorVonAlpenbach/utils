;;; Generation
(defun* sparsity-function (sparsity)
  (case sparsity
    (:normal (sparsity-function 1.1))
    (:dense (sparsity-function 1.05))
    (:very-dense (sparsity-function 1.03))
    (:spread (sparsity-function 2.0))
    (t (lexical-let ((c sparsity))
	 #'(lambda (i) (expt c i))))))

(defun format-float (number decimals)
  (format (format "%%.%df" decimals) number))
;;(loop for i in (0-n 4) collect (format-float pi i "m"))

(defun* required-decimals (x y &optional (maximum 10))
  (let ((res (loop for i to maximum
		   while (string= (format-float x i)
				  (format-float y i))
		   finally return i)))
    (if (> res maximum)
      (error "Too many required decimals")
      res)))
;;(required-decimals 1.106 1.11)

(defun* integer-alternatives (answer sparsity n)
  (assert (integerp answer) t)
  (let ((pos (min answer (random n))))
    (a-b (- answer pos) (- (+ answer n) 1 pos))))
;;(length (integer-alternatives 11 nil 10))

(defun* alternatives (answer sparsity &optional (n 10) (pos (random n)))
  (if (eql sparsity :integer)
    (integer-alternatives answer sparsity n)
    (loop for i from (- pos) below (- n pos)
	  collect (* (funcall (sparsity-function sparsity) i) answer))))
;;(alternatives 3 :integer)

(defun* compile-question (text answer unit &optional (minimum-decimals 1) (sparsity :normal))
  (let* ((alternatives (alternatives answer sparsity))
	 (decimals (max minimum-decimals 
			(required-decimals (first alternatives) (second alternatives)))))
    (list text
	  (mapcar (bind #'format-float decimals) alternatives)
	  (format-float answer decimals)
	  unit)))
;;(apply #'compile-question qwe)

(defun compile-all-questions ()
  (loop for q in +questions+ 
	collect (apply #'compile-question q)))
;;(compile-all-questions)

(defun add-unit (answer-string unit)
  (format "%s %s" answer-string unit))

(defun format-question (q)
  (destructuring-bind (text alternatives answer unit) q
    (flatten (list text 
		   (mapcar (bind #'add-unit unit) alternatives)
		   (add-unit answer unit)))))

(defun* questions-to-csv (&optional (header (flatten (list "Spørsmål" (split-string "ABCDEFGHIJ" "" t) "Svar"))))
  (let ((formatted-questions (mapcar #'format-question *compiled-questions*)))
    (awhen header (push it formatted-questions))
    (csv-string formatted-questions)))
;;(insert (questions-to-csv))

;;; Database
(defconst +questions+
  `(;;mal 1
    ("Et elektron befinner seg i ro fordi tyngden balanseres akkurat av det elektriske feltet fra et annet elektron rett under. Hva er da avstanden mellom elektronene? Vi antar at partiklene befinner seg i nærheten av jordoverflaten."
     ,(coulomb-distance +electron-charge+ +electron-charge+ (* +electron-mass+ +gravity+))
     "meter" 1 :spread)
    ("Et proton befinner seg i ro fordi tyngden balanseres akkurat av det elektriske feltet fra et annet proton rett under. Hva er da avstanden mellom partiklene? Vi antar at partiklene befinner seg i nærheten av jordoverflaten."
     ,(coulomb-distance +proton-charge+ +proton-charge+ (* +proton-mass+ +gravity+))
     "meter" 1 :spread)
    ("Et proton befinner seg i ro fordi tyngden balanseres akkurat av det elektriske feltet fra en alfapartikkel rett under. Hva er da avstanden mellom partiklene? Vi antar at partiklene befinner seg i nærheten av jordoverflaten."
     ,(coulomb-distance (* 2 +proton-charge+) +proton-charge+ (* +proton-mass+ +gravity+))
     "meter" 1 :spread)
    ("Et elektron befinner seg i ro fordi tyngden balanseres akkurat av det elektriske feltet fra en alfapartikkel rett over. Hva er da avstanden mellom partiklene? Vi antar at partiklene befinner seg i nærheten av jordoverflaten."
     ,(coulomb-distance +electron-charge+ (* 2 +proton-charge+) (- (* +electron-mass+ +gravity+)))
     "meter" 1 :spread)
    ("En alfapartikkel befinner seg i ro fordi tyngden balanseres akkurat av det elektriske feltet fra et elektron rett over. Hva er da avstanden mellom partiklene? Vi antar at partiklene befinner seg i nærheten av jordoverflaten."
     ,(coulomb-distance (* 2 +proton-charge+) +electron-charge+ (- (* 2 +proton-mass+ +gravity+)))
     "meter" 1 :spread)

    ;; mal 2
    ("En bil bruker 31 minutter på å kjøre en veistrekning på 44 kilometer. Hva er den minste toppfarten bilen kan ha hatt på denne strekningen?"
     ,(velocity '(44 (:kilo :meter)) '(31 :minute) '((:kilo :meter) :hour))
     "km/h" 1 :very-dense)
    ("En buss bruker 22 minutter på å kjøre en veistrekning på 19 kilometer. Hva er den minste toppfarten bussen kan ha hatt på denne strekningen?"
     ,(velocity '(19 (:kilo :meter)) '(22 :minute) '((:kilo :meter) :hour))
     "km/h" 1 :very-dense)
    ("En bil bruker 5 minutter på å kjøre en veistrekning på 9 kilometer. Hva er den minste toppfarten bilen kan ha hatt på denne strekningen?"
     ,(velocity '(9 (:kilo :meter)) '(5 :minute) '((:kilo :meter) :hour))
     "km/h" 1 :very-dense)
    ("En syklist bruker 22 minutter på å kjøre en veistrekning på 9 kilometer. Hva er den minste toppfarten syklisten kan ha hatt på denne strekningen?"
     ,(velocity '(9 (:kilo :meter)) '(22 :minute) '((:kilo :meter) :hour))
     "km/h" 1 :very-dense)
    ("Et vogntog bruker 4 minutter på å kjøre en veistrekning på 7 kilometer. Hva er den minste toppfarten vogntoget kan ha hatt på denne strekningen?"
     ,(velocity '(7 (:kilo :meter)) '(4 :minute) '((:kilo :meter) :hour))
     "km/h" 1 :very-dense)

    ;; mal3
    ("En dommer knipser en mynt opp i luften, og den lander igjen i hånden etter ett sekund. Hvor høyt kom mynten? Vi ser bort fra luftmotstand."
     ,(distance-vat (* 0.5 1) :acceleration +gravity+)
     "m" 1 :very-dense)
    ("Et egg kastes opp i luften og tas i mot igjen etter to sekunder. Hvor høyt kom egget? Vi ser bort fra luftmotstand."
     ,(distance-vat (* 0.5 2) :acceleration +gravity+)
     "m" 1 :very-dense)
    ("En ball kastes opp i luften og tas i mot igjen etter tre sekunder. Hvor høyt kom ballen? Vi ser bort fra luftmotstand."
     ,(distance-vat (* 0.5 3) :acceleration +gravity+)
     "m" 1 :very-dense)
    ("En fotball sparkes opp i luften og tas i mot igjen etter fire sekunder. Hvor høyt kom ballen? Vi ser bort fra luftmotstand."
     ,(distance-vat (* 0.5 4) :acceleration +gravity+)
     "m" 1 :very-dense)
    ("En baseball slås og treffer bakken igjen etter fem sekunder. Hvor høyt kom ballen? Vi ser bort fra luftmotstand og antar for enkelhets skyld at batteren treffer ballen nesten helt nede ved bakken."
     ,(distance-vat (* 0.5 5) :acceleration +gravity+)
     "m" 1 :very-dense)

    ;; mal 4
    ("En gutt slipper seg ned en sklie med høydeforskjell på 2 meter. Hva er farten til gutten når han glir av sklia? Vi ser bort fra friksjon."
     ,(velocity-from-height 2)
     "m/s" 1 :very-dense)
    ("En jente slipper seg ned en sklie med høydeforskjell på 3 meter. Hva er farten til jenta når hun glir av sklia? Vi ser bort fra friksjon."
     ,(velocity-from-height 3)
     "m/s" 1 :very-dense)
    ("En alpinist setter utfor en bakke med høydeforskjell på 10 meter. Hva er farten til alpinisten når han kommer ut på sletta? Vi ser bort fra friksjon."
     ,(velocity-from-height 10)
     "m/s" 1 :very-dense)
    ("En alpinist setter utfor en bakke med høydeforskjell på 15 meter. Hva er farten til alpinisten når han kommer ut på sletta? Vi ser bort fra friksjon."
     ,(velocity-from-height 15)
     "m/s" 1 :very-dense)
    ("En alpinist setter utfor en bakke med høydeforskjell på 20 meter. Hva er farten til alpinisten når han kommer ut på sletta? Vi ser bort fra friksjon."
     ,(velocity-from-height 20)
     "m/s" 1 :very-dense)
    
    ;; mal 5
    ("En astronaut slipper en stein 1.5 meter over bakken på en måne. 2 sekunder etter treffer steinen bakken. Hva er månens overflategravitasjon?"
     ,(acceleration-st 1.5 2)
     "m/s²" 1 :normal)
    ("En astronaut slipper en stein 2 meter over bakken på en måne. 2 sekunder etter treffer steinen bakken. Hva er månens overflategravitasjon?"
     ,(acceleration-st 2 2)
     "m/s²" 1 :normal)
    ("En astronaut slipper en stein 3 meter over bakken på en måne. 2 sekunder etter treffer steinen bakken. Hva er månens overflategravitasjon?"
     ,(acceleration-st 3 2)
     "m/s²" 1 :normal)
    ("En astronaut slipper en stein 4 meter over bakken på en måne. 2 sekunder etter treffer steinen bakken. Hva er månens overflategravitasjon?"
     ,(acceleration-st 4 2)
     "m/s²" 1 :normal)
    ("En astronaut slipper en stein 5 meter over bakken på en måne. 2 sekunder etter treffer steinen bakken. Hva er månens overflategravitasjon?"
     ,(acceleration-st 5 2)
     "m/s²" 1 :normal)

    ;; mal 6
    ("Et romskip har landet på en en måne som har samme massetetthet som jorda. En astonaut hopper ut fra romskipet sitt og lander etter 2 sekunder på overflaten 6 meter nedenfor. Hva er da månens radius?"
     ,(change-unit (* +earth-radius+ (/ (acceleration-st 6 2) +gravity+)) :from :meter :to '(:kilo :meter))
     "km" 1 :normal)
    ("Et romskip har landet på en en måne som har samme massetetthet som jorda. En astonaut hopper ut fra romskipet sitt og lander etter 3 sekunder på overflaten 6 meter nedenfor. Hva er da månens radius?"
     ,(change-unit (* +earth-radius+ (/ (acceleration-st 6 3) +gravity+)) :from :meter :to '(:kilo :meter))
     "km" 1 :normal)
    ("Et romskip har landet på en en måne som har samme massetetthet som jorda. En astonaut hopper ut fra romskipet sitt og lander etter 4 sekunder på overflaten 6 meter nedenfor. Hva er da månens radius?"
     ,(change-unit (* +earth-radius+ (/ (acceleration-st 6 4) +gravity+)) :from :meter :to '(:kilo :meter))
     "km" 1 :normal)
    ("Et romskip har landet på en en måne som har samme massetetthet som jorda. En astonaut hopper ut fra romskipet sitt og lander etter 5 sekunder på overflaten 6 meter nedenfor. Hva er da månens radius?"
     ,(change-unit (* +earth-radius+ (/ (acceleration-st 6 5) +gravity+)) :from :meter :to '(:kilo :meter))
     "km" 1 :normal)
    ("Et romskip har landet på en en måne som har samme massetetthet som jorda. En astonaut hopper ut fra romskipet sitt og lander etter 6 sekunder på overflaten 6 meter nedenfor. Hva er da månens radius?"
     ,(change-unit (* +earth-radius+ (/ (acceleration-st 6 6) +gravity+)) :from :meter :to '(:kilo :meter))
     "km" 1 :normal)

    ;; mal 7
    ("En satelitt går i sirkel rundt jorden, 1000 km over jordoverflaten. Hva er omløpshastigheten?"
     ,(revolution-speed (+ +earth-radius+ 1E6))
     "m/s" 0 :spread)
    ("En satelitt går i sirkel rundt jorden, 1250 km over jordoverflaten. Hva er omløpshastigheten?"
     ,(revolution-speed (+ +earth-radius+ 1.25E6))
     "m/s" 0 :spread)
    ("En satelitt går i sirkel rundt jorden, 1500 km over jordoverflaten. Hva er omløpshastigheten?"
     ,(revolution-speed (+ +earth-radius+ 1.5E6))
     "m/s" 0 :spread)
    ("En satelitt går i sirkel rundt jorden, 1750 km over jordoverflaten. Hva er omløpshastigheten?"
     ,(revolution-speed (+ +earth-radius+ 1.75E6))
     "m/s" 0 :spread)
    ("En satelitt går i sirkel rundt jorden, 2000 km over jordoverflaten. Hva er omløpshastigheten?"
     ,(revolution-speed (+ +earth-radius+ 2E6))
     "m/s" 0 :spread)

    ;; mal 8
    ("En satelitt går i sirkel rundt jorden, 1000 km over jordoverflaten. Hva er omløpsperioden?"
     ,(period-of-revolution (+ +earth-radius+ 1E6) :time-unit :minute)
     "minutter" 0 :normal)
    ("En satelitt går i sirkel rundt jorden, 1250 km over jordoverflaten. Hva er omløpsperioden?"
     ,(period-of-revolution (+ +earth-radius+ 1.25E6) :time-unit :minute)
     "minutter" 0 :normal)
    ("En satelitt går i sirkel rundt jorden, 1500 km over jordoverflaten. Hva er omløpsperioden?"
     ,(period-of-revolution (+ +earth-radius+ 1.5E6) :time-unit :minute)
     "minutter" 0 :normal)
    ("En satelitt går i sirkel rundt jorden, 1750 km over jordoverflaten. Hva er omløpsperioden?"
     ,(period-of-revolution (+ +earth-radius+ 1.75E6) :time-unit :minute)
     "minutter" 0 :normal)
    ("En satelitt går i sirkel rundt jorden, 2000 km over jordoverflaten. Hva er omløpsperioden?"
     ,(period-of-revolution (+ +earth-radius+ 2E6) :time-unit :minute)
     "minutter" 0 :normal)

    ;; mal 9
    ("Hva er treghetsmomentet til en rullende boks med frossen lapskaus? Boksen er sylinderformet med radius 5 cm og den veier 0.8 kg."
     ,(moment-of-inertia-cylinder 0.8 0.05)
     "kg m²" 1 :normal)
    ("Hva er treghetsmomentet til en rullende stokk med tversnittdiameter 30 cm og som veier 200 kg?"
     ,(moment-of-inertia-cylinder 200 0.3)
     "kg m²" 1 :normal)
    ("Hva er treghetsmomentet til en bowlingball på 7 kg og med diameter 21.7 cm? Vi ser bort de tre hullene i denne oppgaven."
     ,(moment-of-inertia-sphere 7 0.217)
     "kg m²" 1 :normal)
    ("Hva er treghetsmomentet til en rullende tom hermetikkboks på 30 g og med (ytre) diameter 6.5 cm? Tykkelsen på boksen er 1 mm."
     ,(moment-of-inertia-hollow-cylinder 0.030 (/ 0.065 2) (/ 0.063 2))
     "kg m²" 1 :spread)
    ("Hva er treghetsmomentet til en rullende norsk 1-krone? (Diameter er 21 mm, hulldiameter er 3.2 mm og vekt er 4.35 gram.)"
     ,(moment-of-inertia-hollow-cylinder 0.00435 (/ 0.021 2) (/ 0.0032 2))
     "kg m²" 1 :spread)

    ;; mal 10
    ("Hva er vinkelhastigheten til jordrotasjonen?"
     ,(angular-velocity-from-period  (change-time-unit 1 :from-unit :day))
     "rad/s" 1 :spread)
	    ("Hva er vinkelhastigheten til en spillende LP-plate? Anta at platespilleren gjør 33 omdreininger i minuttet."
     ,(angular-velocity-from-rpm 33)
     "rad/s" 1 :spread)
    ("Hva er vinkelhastigheten til hjulrotasjonen på en bil som kjører i 70 km/h? Diameteren på dekkene er 80 cm og vi ser bort fra sammenpressing av dekkene under kjøringen."
     ,(angular-velocity-of-car-wheel (/ 0.8 2) (/ 70 3.6))
     "rad/s" 1 :spread)
    ("Hva er vinkelhastigheten til hjulrotasjonen på en sykkel som triller i 30 km/h? Diameteren på dekkene er 70 cm og vi ser bort fra sammenpressing av dekkene under kjøringen."
     ,(angular-velocity-of-car-wheel (/ 0.7 2) (/ 30 3.6))
     "rad/s" 1 :spread)
    ("Hva er vinkelhastigheten til rotasjonen til en klinkekule som triller med en fart av 0.5 m/s? Klinkekulens diameter er 16 mm."
     ,(angular-velocity-of-car-wheel (/ 0.016 2) 0.5)
     "rad/s" 1 :spread)

    ;; mal 11
    ("En kule på 300 gram og diameter 10 cm slippes ned en bakke. Kula sklir men får også noe rotasjon som følge av friksjon mot underlaget. 10 høydemeter lenger ned roterer kulen 2 ganger i sekundet. Hva er da farten til kula?"
     ,(velocity-from-sphere-rolling-down 0.3 (/ 0.1 2) 10 (/ 1.0 2))
     "kg m²" 1 :spread)
    ("En kule på 300 gram og diameter 10 cm slippes ned en bakke. Kula sklir men får også noe rotasjon som følge av friksjon mot underlaget. 10 høydemeter lenger ned roterer kulen 2 ganger i sekundet. Hva er da farten til kula?"
     ,(velocity-from-sphere-rolling-down 0.3 (/ 0.1 2) 10 (/ 1.0 2))
     "kg m²" 1 :spread)
    ("En kule på 300 gram og diameter 10 cm slippes ned en bakke. Kula sklir men får også noe rotasjon som følge av friksjon mot underlaget. 10 høydemeter lenger ned roterer kulen 2 ganger i sekundet. Hva er da farten til kula?"
     ,(velocity-from-sphere-rolling-down 0.3 (/ 0.1 2) 10 (/ 1.0 2))
     "kg m²" 1 :spread)
    ("En bowlingkule på 7 kg og diameter 21.7 cm slippes ned en bakke. Kula sklir men får også noe rotasjon som følge av friksjon mot underlaget. 10 høydemeter lenger ned roterer kulen 2 ganger i sekundet. Hva er da farten til kula?"
     ,(velocity-from-sphere-rolling-down 0.3 (/ 0.1 2) 10 (/ 1.0 2))
     "kg m²" 1 :spread)
    ("En bowlingkule på 7 kg og diameter 21.7 cm slippes ned en bakke. Kula sklir men får også noe rotasjon som følge av friksjon mot underlaget. 50 høydemeter lenger ned roterer kulen 3 ganger i sekundet. Hva er da farten til kula?"
     ,(velocity-from-sphere-rolling-down 0.3 (/ 0.1 2) 10 (/ 1.0 2))
     "kg m²" 1 :spread)

    ;; mal 12/13
    ("Hvor mange forskjellige ord kan fås ved å stokke om på bokstavene i ANAGRAM?"
     ,(number-of-anagrams "ANAGRAM")
     "" 0 :spread)
    ("Hvor mange forskjellige ord kan fås ved å stokke om på bokstavene i ANAGRAM uten at noen av A-ene kommer ved siden av hverandre?"
     ,(number-of-anagrams-without-adjacent-character "ANAGRAM" ?A)
     "" 0 :spread)
    ("Hvor mange forskjellige ord kan fås ved å stokke om på bokstavene i OMSTOKKING?"
     ,(number-of-anagrams "OMSTOKKING")
     "" 0 :spread)
    ("Hvor mange forskjellige ord kan fås ved å stokke om på bokstavene i OMSTOKKING uten at noen av O-ene kommer ved siden av hverandre?"
     ,(number-of-anagrams-without-adjacent-character "OMSTOKKING" ?O)
     "" 0 :spread)
    ("Hvor mange forskjellige ord kan fås ved å stokke om på bokstavene i BOKSTAVNØTT?"
     ,(number-of-anagrams "BOKSTAVNØTT")
     "" 0 :spread)
    ("Hvor mange forskjellige ord kan fås ved å stokke om på bokstavene i BOKSTAVNØTT uten at noen av T-ene kommer ved siden av hverandre?"
     ,(number-of-anagrams-without-adjacent-character "BOKSTAVNØTT" ?T)
     "" 0 :spread)
    ("Hvor mange forskjellige ord kan fås ved å stokke om på bokstavene i VARIANTER?"
     ,(number-of-anagrams "VARIANTER")
     "" 0 :spread)
    ("Hvor mange forskjellige ord kan fås ved å stokke om på bokstavene i VARIANTER uten at noen av A-ene kommer ved siden av hverandre?"
     ,(number-of-anagrams-without-adjacent-character "VARIANTER" ?A)
     "" 0 :spread)
    ("Hvor mange forskjellige ord kan fås ved å stokke om på bokstavene i ORDSMEDKUNST?"
     ,(number-of-anagrams "ORDSMEDKUNST")
     "" 0 :spread)
    ("Hvor mange forskjellige ord kan fås ved å stokke om på bokstavene i ORDSMEDKUNST uten at noen av D-ene kommer ved siden av hverandre?"
     ,(number-of-anagrams-without-adjacent-character "ORDSMEDKUNST" ?D)
     "" 0 :spread)

    ;; mal 14
    ("Hvis man borrer et sylindrisk hull med lengde 5 cm tvers gjennom en kule, hva er da volumet av det som er igjen av kula? Kulens diameter er naturligvis minst 5 cm."
     ,(sphere-volume (* 0.5 5))
     "cm³" 1 :normal)
    ("Hvis man borrer et sylindrisk hull med lengde 6 cm tvers gjennom en kule, hva er da volumet av det som er igjen av kula? Kulens diameter er naturligvis minst 5 cm."
     ,(sphere-volume (* 0.5 6))
     "cm³" 1 :normal)
    ("Hvis man borrer et sylindrisk hull med lengde 7 cm tvers gjennom en kule, hva er da volumet av det som er igjen av kula? Kulens diameter er naturligvis minst 5 cm."
     ,(sphere-volume (* 0.5 7))
     "cm³" 1 :normal)
    ("Hvis man borrer et sylindrisk hull med lengde 8 cm tvers gjennom en kule, hva er da volumet av det som er igjen av kula? Kulens diameter er naturligvis minst 5 cm."
     ,(sphere-volume (* 0.5 8))
     "cm³" 1 :normal)
    ("Hvis man borrer et sylindrisk hull med lengde 9 cm tvers gjennom en kule, hva er da volumet av det som er igjen av kula? Kulens diameter er naturligvis minst 5 cm."
     ,(sphere-volume (* 0.5 9))
     "cm³" 1 :normal)

    ;; mal 15
    ("Dersom den lengste rette linjen som kan trekkes innenfor arealet begrenset av to konsentriske sirkler, har lengde 1, hva er da arealet mellom de to sirklene?"
     ,(circle-area (* 0.5 1))
     "" 1 :normal)
    ("Dersom den lengste rette linjen som kan trekkes innenfor arealet begrenset av to konsentriske sirkler, har lengde 2, hva er da arealet mellom de to sirklene?"
     ,(circle-area (* 0.5 2))
     "" 1 :normal)
    ("Dersom den lengste rette linjen som kan trekkes innenfor arealet begrenset av to konsentriske sirkler, har lengde 3, hva er da arealet mellom de to sirklene?"
     ,(circle-area (* 0.5 3))
     "" 1 :normal)
    ("Dersom den lengste rette linjen som kan trekkes innenfor arealet begrenset av to konsentriske sirkler, har lengde 4, hva er da arealet mellom de to sirklene?"
     ,(circle-area (* 0.5 4))
     "" 1 :normal)
    ("Dersom den lengste rette linjen som kan trekkes innenfor arealet begrenset av to konsentriske sirkler, har lengde 5, hva er da arealet mellom de to sirklene?"
     ,(circle-area (* 0.5 5))
     "" 1 :normal)

    ;; mal 16
    (,(how-many? "Per" 1 "Pål" 2 "Askeladden" 4 "epler")
      4
      "" 0 :integer)
    (,(how-many? "Ole" 3 "Dole" 4 "Doffen" 5 "klinkekuler")
      5
      "" 0 :integer)
    (,(how-many? "Hetti" 3 "Netti" 5 "Letti" 7 "terninger")
      7
      "" 0 :integer)
    (,(how-many? "Tom" 4 "Dick" 9 "Harry" 11 "kniver")
      11
      "" 0 :integer)
    (,(how-many? "Gaus" 5 "Roms" 4 "Brumun" 2 "spektralsteiner")
      2
      "" 0 :integer)
    
    ;; mal 17
    ("En iskrembutikk tilbyr kuler i ti ulike smaksvarianter. Hvor mange ulike iser kan man da komponere med fire kuler?"
     ,(selection-with-repetition 10 4)
     "" 0 :spread)

    ("En butikk selger 20 ulike typer jellybeans. På hvor mange forskjellige måter kan vi da plukke ut 25 jellybeans?"
     ,(selection-with-repetition 20 25)
     "" 0 :spread)

    ("På hvor mange forskjellige måter kan du fordele 25 kronestykker på fem unger?"
     ,(selection-with-repetition 5 25)
     "" 0 :spread)

    ("På hvor mange forskjellige måter kan du fordele 20 kronestykker på seks unger?"
     ,(selection-with-repetition 6 20)
     "" 0 :spread)

    ("På hvor mange forskjellige måter kan du fordele 15 kronestykker på sju unger?"
     ,(selection-with-repetition 7 15)
     "" 0 :spread)
    ))
;;(length (compile-all-questions))
;;(apply #'compile-question (last-elt +questions+))
;;(setf qwe (last-elt +questions+))
;;(/ 19 (/ 22.0 60))

(defvar *compiled-questions* nil)

;;;;;;;;;;(setf *compiled-questions* (compile-all-questions))
;;(insert (questions-to-csv))
