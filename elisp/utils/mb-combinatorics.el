(require 'mb-utils-math)

(cl-defun factorial (n &optional (m 1))
  "Returns N!. See also `derangement'"
  (cond
    ((zerop n) 1)
    ((minusp n) (error "FACTORIAL is only valid for non-negative integers."))
    (t (product-a-b m n))))
;;(mapcar #'factorial (0-n 10))

(cl-defun derangement (n &optional (!0 1) (!1 0))
  "Returns !N. See also `factorial'.
For the definition of !N, see https://en.wikipedia.org/wiki/Derangement"
  (cl-case n
    (0 !0) 
    (1 !1) 
    (otherwise 
     (cl-loop for i from 2 to n
	   for Ln-2 = !0 then Ln-1
	   for Ln-1 = !1 then Ln
	   for Ln = (* (1- i) (+ Ln-1 Ln-2))
	   finally return Ln))))
;;(mapcar #'derangement (0-n 11))

(defalias 'n! 'factorial)
(defalias '!n 'derangement)

(cl-defun factorials (n &optional (m 1))
  "Returns M!, (M + 1)!, ... N!.
By default, M is 1."
  (cl-loop for i from m to n
	for p = (n! m) then (* p i)
	collect p))
;;(factorials 5 3)

(cl-defun fibonacci-numbers (n &optional (start-values '(0 1)))
  "Return the N first Fibonacci numbers.
The optional START-VALUES modifies the two first elements in the
resulting sequence."
  (if (< n 2)
    (head n start-values)
    (let ((res (reverse start-values)))
     (cl-loop for i below (- n 2)
	   do (push (+ (first res) (second res)) res))
     (nreverse res))))
;;(fibonacci-numbers 0)

(defconst +golden-ratio+ (/ (1+ (sqrt 5)) 2))

(cl-defun fibonacci-nth (n)
  (let ((psi +golden-ratio+)
	(phi (- 1 +golden-ratio+)))
    (/ (- (expt phi n) (expt psi n))
       (- phi psi))))
;;(time (fibonacci-nth 50))

(cl-defun binomial-coefficient-simple (n m)
  "Weak version, cannot deal with Ns and Ms greater than ~ 11"
  (/ (factorial n (1+ m)) (factorial (- n m))))
;;(binomial-coefficient-simple 10 4)

(cl-defun simplify-ratio (ratio)
  (mapcar (bind #'/ (apply #'cl-gcd ratio)) ratio))
;;(simplify-ratio '(6 10))

(cl-defun divisor-function (n &optional (order 1))
  (Ln-sum (all-factors n) order))
;;(divisor-function 220)
;;(divisor-function 284)

(cl-defun aliquot-sum (n)
  (- (divisor-function n 1) n))
;;(mapcar #'aliquot-sum (1-n 10))

(cl-defun amicable-numbers-p (x y)
  (= (divisor-function x) (divisor-function y)))
;;(amicable-numbers-p 220 284)

(cl-defun abundancy-index (n)
  (simplify-ratio (list (sum (all-factors n)) n)))
;;(mapcar #'abundancy-index '(220 284 30))

(cl-defun remove-factors (factors divisor)
  "Removes all factors in FACTORS that are also factors in DIVISOR"
  (cl-loop for factor in factors
	for d = divisor then (/ d cl-gcd)
	for cl-gcd = (cl-gcd factor d)
        collect (/ factor cl-gcd)))
;;(remove-factors '(2 3 4 5) 12)

(cl-defun binomial-coefficient (n m &optional (method :auto))
  "Returns n!/(m!*(n-m)!"
  (if (minusp m)
    0
    (let* ((m* (max m (- n m)))
	   (factors
	    (cl-loop for denominator-factor in (a-b 1 (- n m*))
		     for numerator-factors = (a-b (1+ m*) n)
		     then (remove-factors numerator-factors denominator-factor)
		     finally return numerator-factors)))
      (product* factors :method method))))
;;(time (binomial-coefficient 112 100 :auto))

(cl-defun binomial-coefficients-n (n1 n2 m &optional (method :auto))
  "Returns (n1 / m), ((n1 + 1) / m) ... (n2 / m)"
  (cl-loop for n from n1 to n2
	for bc = (binomial-coefficient n m method) then (/ (* bc n) (- n m))
	collect bc))
;;(binomial-coefficients-n 4 6 3)

(cl-defun binomial-coefficients-m (n m1 m2 &optional (method :auto))
  "Returns (n / m1), (n / (m1 + 1)) ... (n2 / m2)"
  (cl-loop for m from m1 to m2
	for bc = (binomial-coefficient n m method) then (/ (* bc (- n m -1)) m)
	collect bc))
;;(binomial-coefficients-m 6 0 5)
;;(binomial-coefficient 5 2)

(cl-defun catalan-nth (n)
  "Return the Nth Catalan number.
The Catalan numbers form the sequence A000108 in the OEIS."
  (/ (binomial-coefficient (2* n) n) (1+ n)))
;;(catalan-nth 3)

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
  (cl-member team (mapcar #'first group) :test #'equal))
;;(uefa-group-member-p 'PSG (first uefa-groups))

(cl-defun uefa-group (team &optional (groups uefa-groups))
  (cl-find team groups :test #'uefa-group-member-p))
;;(uefa-group (first (uefa-winners)))

(cl-defun uefa-same-group-p (team1 team2 &optional (groups uefa-groups))
  (uefa-group-member-p team1 (uefa-group team2 groups)))
;;(uefa-same-group-p 'Schalke 'Arsenal)

(cl-defun uefa-team-country (team &optional (groups uefa-groups))
  (second (cl-find team (uefa-group team groups) :key #'first)))
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

;; (copy-if #'(lambda (runner-up) (uefa-possible-opponents-p winner runner-up groups))
;; 	   (uefa-runner-ups groups))
;;(uefa-possible-opponents 'Barcelona)

(cl-defun uefa-possible-draws (winners-left runner-ups-left &optional (groups uefa-groups))
  "Recursive function that on each recursion stage returns list of elements on the form (MATCH DRAWS-GIVEN-MATCH),
where MATCH is formed fro"
  (if winners-left
    (cl-loop with winner = (first winners-left)
	  for runner-up in runner-ups-left
	  for match = (list winner runner-up)
	  for draws-given-match =
	  (and (uefa-possible-opponents-p winner runner-up groups)
	       (uefa-possible-draws (rest winners-left)
				    (remove runner-up runner-ups-left) groups))
	  if res collect (cons match draws-given-match))
    (list t)))

(cl-defun uefa-possible-draws-main (&optional (groups uefa-groups))
  (uefa-possible-draws (copy-list (uefa-winners groups)) 
		       (copy-list (uefa-runner-ups groups))
		       groups))
;;(setf res (uefa-possible-draws-main))
;;(cl-count t (flatten res))

(cl-defun rummy-hands (&optional (players 2) (cards (if (< players 5) 10 6)) (deck 52))
  "Return the number of possible initial rummy hands,
given PLAYERS number of players each having HANDS initial hands.
The function is inspired by the film 'The Midnight Sky' where XXX
claims there are over 60 billions initial rummy hands. Since the
scene where this quote occurs involves two players, we assume
PLAYERS is 2 and CARDS is 10.

This assertion seems wrong given the Wikipedia definition of
standard rummy."
  (cl-assert (<= (* players cards) deck) t "Too many players or cards!")
  (/ (product (cl-loop for n below players
		    collect (binomial-coefficient deck cards)
		    do (decf deck cards)))
     (n! players)))
;;(rummy-hands 2)

(cl-defun stirling-numbers-2 (n k)
  "Return coefficients s(n, k), i.e. a Stirling number of the first kind.
This is the number of permutations of n elements with k disjoint
cycles."
  (round
   (cl-loop with k-i!s = (nreverse (factorials k 0))
	 for i from 0 to k
	 for sign = (expt -1 (- k i))
	 for i! = 1 then (* i! i)
	 for k-i! in k-i!s
	 for addend = (* sign (/ (expt i n) 1.0 k-i! i!))
	 ;; do (print (list i sign i! k-i! addend))
	 sum addend)))
;;(stirling-numbers-2 0 0)
;;(cl-loop for k to 10 collect (stirling-numbers-2 10 k))

(cl-defun stirling-numbers-1 (n k)
  "Return coefficients s(n, k), i.e. a Stirling number of the first kind.
This is the number of permutations of n elements with k disjoint
cycles."
  (cl-loop with 2n-k = (- (2* n) k)
	for j from n to 2n-k
	for signum = (expt -1 (- j k))
	for addend = (* signum
			(binomial-coefficient (1- j) (1- k))
			(binomial-coefficient 2n-k j)
			(stirling-numbers-2 (- j k) (- j n)))
	sum addend))
;;(cl-loop for k to 4 collect (stirling-numbers-1 4 k))

(provide 'mb-combinatorics)
