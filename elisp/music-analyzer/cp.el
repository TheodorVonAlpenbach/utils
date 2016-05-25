(require 'bach-contrapunctus)

(defconst cp-legal-styles '(jeppesen fux wikipedia grinde hindemith))
(defconst cp-legal-species '(1 2 3 4 5 canon fuga imitatio inversio))

(require 'hindemith-cp)

(defconst cp-rules
  `((palestrina
     ((1 ())
      (2 ())
      (3 ())
      (4 ())
      (5 ())))
    (grinde
     ((1 ())
      (2 (2cp-dissonances))
      (3 ())
      (4 ())
      (5 ())))
    (hindemith
     ((1 (hcp-model-rules))
      (2 (hcp-two-part-rules))
      (3 ())
      (4 ())
      (5 ())))))

(defstruct (counterpoint :named (:conc-name cp-))
  (name)
  (style)
  (species) 
  (voices))

(defun* cp-new (voices species &optional (style 'grinde)  (name nil))
  (unless (member style cp-legal-styles)
    (error "STYLE argument %S is illegal. It must be one of %S" style cp-legal-styles))
  (unless (member species cp-legal-species)
    (error "SPECIES argument %S is illegal. It must be one of %S" species cp-legal-species))
  (make-counterpoint :voices voices :species species :style style :name name))

(defun cp-to-string (cp)
  (format "Style: %S\nSpecies: %S\n%s" (cp-style cp) (cp-species cp) (voices-to-string (cp-voices cp))))

;;(vg-voices (first (test-vgs refresh))) '2 'grinde
(lexical-let ((cp nil)
	      (cp-file "c:/Documents and Settings/matsb/My Documents/projects/UiO/Var-2012/2270-Satslaere2B/ex-Hindemith-cp1.ly"))
  (defun* cp-test (&optional (refresh nil) (style 'hindemith) (species 1) (n 1))
    (when (or refresh (not cp))
      (setq cp (cp-new (vg-voices (nth n (ly-parse-file cp-file))) species style)))
    cp))
;;(cp-test t 'hindemith 2 1)

(defun cp-rules (cp)
  (aif (find (cp-style cp) cp-rules :key #'first)
    (aif (find (cp-species cp) (second it) :key #'first)
      (second it)
      (error "Couldn't find species %S in style %S" (cp-species cp) (cp-style cp)))
    (error "Couldn't find style %S in the rules collection." (cp-style cp))))
;;(cp-rules (cp-test))

(defun cp-check (cp)
  (loop for rule in (cp-rules cp)
	append (funcall rule cp)))
;;(cp-check (cp-test))

(defun* cp-vertical-intervals (cp &key (filter nil))
  (case filter
    (accented)))

(defun* cp-check-print (cp &optional (indent 0))
  (concat* (cp-check cp) :in "\n" :indent-string (make-string indent ? )))
;;(cp-check-print (hcp-test) 3)

(defun* cps-check-print (cps &optional (indent 3))
  (concat* 
   (loop for cp in cps
	 collect (format "%s:\n%s" (cp-name cp) (cp-check-print cp indent)))
   :in "\n"))
;;(cp-check-print (hcp-test))

(provide 'cp)
