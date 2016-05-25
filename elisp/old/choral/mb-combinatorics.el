(cl-defun faculty (n &optional (m 1))
  "Returns N!. See also `derangement'"
  (loop for i from m to n
	for res = m then (* res i)
	finally return res))
;;(faculty 2)
;;(mapcar #'faculty (0-n 10))

(cl-defun derangement (n &optional (!0 1) (!1 0))
  "Returns !N. See also `faculty'"
  (case n
    (0 !0) 
    (1 !1) 
    (otherwise 
     (loop for i from 2 to n
	   for Ln-2 = !0 then Ln-1
	   for Ln-1 = !1 then Ln
	   for Ln = (* (1- i) (+ Ln-1 Ln-2))
	   finally return Ln))))
;;(mapcar #'derangement (0-n 11))

(defalias 'n! 'faculty)
(defalias '!n 'derangement)

(defun binomial-coefficient-simple (n m)
  "Weak version, cannot deal with Ns and Ms greater than ~ 11"
  (/ (faculty n (1+ m)) (faculty (- n m))))
;;(binomial-coefficient-simple 10 4)

(defun remove-factors (factors divisor)
  "Removes all factors in FACTORS that are also factors in DIVISOR"
  (loop for factor in factors
	for d = divisor then (/ d gcd)
	for gcd = (gcd factor d)
        collect (/ factor gcd)))
;;(remove-factors '(2 3 4 5) 12)

(defun binomial-coefficient (n m &optional float-result)
  "Returns n!/(m!*(n-m)!"
  (let* ((m* (max m (- n m)))
	 (factors (loop for denominator-factor in (a-b 1 (- n m*))
			for numerator-factors = (a-b (1+ m*) n)
			then (remove-factors numerator-factors denominator-factor)
			finally return numerator-factors)))
    (apply #'* (if float-result (mapcar #'float factors) factors))))
;;(binomial-coefficient 14 10)

(defconst uefa-groups
  '(((PSG fra) (Porto por))
    ((Schalke ger) (Arsenal eng))
    ((Malaga spa) (Milan ita))
    ((Dortmund ger) (Real-Madrid spa))
    ((Juventus ita) (Shakhtar-Donetsk rus))
    ((Bayern-Munchen ger) (Valencia spa))
    ((Barcelona spa) (Celtic sco))
    ((Manchester-United eng) (Galatasaray tur))))

(cl-defun uefa-team-nation-map (&optional (groups uefa-groups))
  (apply #'append groups))
;;(uefa-team-nation-map)

(cl-defun uefa-winners (&optional (groups uefa-groups))
  (mapcar (compose #'first #'first) groups))
;;(uefa-winners)

(cl-defun uefa-runner-ups (&optional (groups uefa-groups))
  (mapcar (compose #'first #'second) groups))
;;(uefa-runner-ups)

(cl-defun uefa-winner-p (team &optional (groups uefa-groups))
  (member team (uefa-winners groups)))
;;(uefa-winner-p 'Real-Madrid)

(cl-defun uefa-teams (&optional (groups uefa-groups))
  (append (uefa-winners groups) (uefa-runner-ups groups)))
;;(uefa-teams)

(cl-defun uefa-group-member-p (team group)
  (member* team (mapcar #'first group) :test #'equal))
;;(uefa-group-member-p 'PSG (first uefa-groups))

(cl-defun uefa-group (team &optional (groups uefa-groups))
  (find team groups :test #'uefa-group-member-p))
;;(uefa-group (first (uefa-winners)))

(cl-defun uefa-same-group-p (team1 team2 &optional (groups uefa-groups))
  (uefa-group-member-p team1 (uefa-group team2 groups)))
;;(uefa-same-group-p 'Schalke 'Arsenal)

(cl-defun uefa-team-country (team &optional (groups uefa-groups))
  (second (find team (uefa-group team groups) :key #'first)))
;;(uefa-team-country 'PSG)

(cl-defun uefa-same-country-p (team1 team2 &optional (groups uefa-groups))
  (eq (uefa-team-country team1 groups) (uefa-team-country team2 groups)))
;;(uefa-same-country-p 'Malaga 'Barcelona)

(cl-defun uefa-possible-opponents-gen-p (team1 team2 &optional (groups uefa-groups))
  "Returns nil iff TEAM1 TEAM2 are not allowed to meet. See also
`uefa-possible-opponents-p'"
  (not (or (uefa-same-group-p team1 team2 groups)
	   (uefa-same-country-p team1 team2 groups)
	   (all-true (uefa-winner-p team1 groups) (uefa-winner-p team2 groups)))))
;;(uefa-possible-opponents-gen-p 'Schalke 'PSG)

(cl-defun uefa-possible-opponents-gen (team &optional (groups uefa-groups))
  "Returns a list of all the teams that are allowed to meet TEAM.
See also `uefa-possible-opponents-gen"
  (copy-if #'(lambda (x) (uefa-possible-opponents-p x team groups)) (uefa-teams groups)))
;;(uefa-possible-opponents-gen 'Barcelona)

(cl-defun uefa-opposite-team-group (team &optional (groups uefa-groups))
  (if (uefa-winner-p team groups)
    (uefa-winners groups) (uefa-runner-ups groups)))
;;(uefa-opposite-team-group 'Barcelona)

(cl-defun uefa-possible-opponents-p (winner runner-up &optional (groups uefa-groups))
  "Returns nil iff the WINNER and RUNNER-UP teams are not allowed to meet."
  (not (or (uefa-same-group-p winner runner-up groups)
	   (uefa-same-country-p winner runner-up groups))))
;;(uefa-possible-opponents-p 'Barcelona 'Real-Madrid)

(cl-defun uefa-possible-opponents (winner &optional (groups uefa-groups))
  "Returns a list of all the teams that are allowed to meet the group WINNER."
  (copy-if #'(lambda (runner-up) (uefa-possible-opponents-p winner runner-up groups))
	   (uefa-runner-ups groups)))
;;(uefa-possible-opponents 'Barcelona)

(cl-defun uefa-possible-draws (winners-left runner-ups-left &optional (groups uefa-groups))
  "Recursive function that on each recursion stage returns list of elements on the form (MATCH DRAWS-GIVEN-MATCH),
where MATCH is formed fro"
  (if winners-left
    (loop with winner = (first winners-left)
	  for runner-up in runner-ups-left
	  for match = (list winner runner-up)
	  for draws-given-match = (and (uefa-possible-opponents-p winner runner-up groups)
				       (uefa-possible-draws (rest winners-left) (remove runner-up runner-ups-left) groups))
	  if res collect (cons match draws-given-match))
    (list t)))

(cl-defun uefa-possible-draws-main (&optional (groups uefa-groups))
  (uefa-possible-draws (copy-list (uefa-winners groups)) 
		       (copy-list (uefa-runner-ups groups))
		       groups))
;;(setq res (uefa-possible-draws-main (last qwe 8)))
;;(count t (flatten res))
