'(require 'mb-utils-div)

(defconst *pm-edit-buffer-name-prefix* "pm: ")

(defun pm-edit-mode () "Major mode edititing a task list.
\\{pm-edit-mode-map}
\\<pm-edit-mode-map>"
  (interactive)
  (kill-all-local-variables)
; (set-syntax-table pm-edit-mode-syntax-table)
  (use-local-map pm-edit-mode-map)
; (make-local-variable 'font-lock-defaults)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'pm-edit-mode)
  (setq mode-name "pm edit mode")
  (setq buffer-offer-save nil)
;   (setq font-lock-defaults '(pm-edit-font-lock-keywords t))
;  (set (make-local-variable 'b-point-max) (point-max))
;  (set (make-local-variable 'b-point) (point))
;  (setf fill-prefix "   ")
  ;(pm-edit-proxy-read-db)
  (run-hooks 'text-mode-hook 'pm-edit-mode-hook))

(defvar pm-edit-mode-map ()
  "Keymap used in pm-edit mode.")
(when (not pm-edit-mode-map) ;(nilf pm-edit-mode-map)
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'pm-edit-goto-url)
    (define-key map "u" #'(lambda () (mb-local-url-map 1)))
    (define-key map "G" 
      #'(lambda (url) "Goto url without using proxy."
		(interactive "sURL: ") 
		(pm-edit-goto-url url t)))
    (setq pm-edit-mode-map map)))

;;((PROJECT-TYPE SUPERPROJECT-TITLE) TITLE WITHIN-PERIOD DURATION RESPONSIBLE AUTHOR TODO-LIST WRITTEN LAST-UPDATED)
(setq *pm-db*
'(((s "") "employment" (2000-08-06 2000-09-01) 5 mb mb TODO 2000-08-07 2000-08-07) 
((s "") "tilleggssøknad?" (2000-08-06 2000-08-09) 5 mb mb (("Avklare med Egbert om (f eks) tilleggssøknad.")) 2000-08-07 2000-08-07)
((s "") "stepstonesøknad" (2000-08-06 2000-08-14) 5 lbs mb (("Skrive generell søknad")) 2000-08-07 2000-08-07)
((s "") "nyhetsgruppesøknad" (2000-08-06 2000-08-14) 5 lbs mb (("no.annonser.it.unix")) 2000-08-07 2000-08-07)
((s "") "nyhetsgruppesøknad" (2000-08-06 2000-08-14) 5 lbs mb (("no.annonser.it.diverse")) 2000-08-07 2000-08-07)
((s "") "stepstonesøknad" (2000-08-06 2000-08-14) 5 lbs mb (("Skaffe tilgang til tavlen") ("skrive tavlesøknad (bør være noe annerledes enn andre søknader, da studenter er målgruppen)") ("Sett opp søknad på tavlen")) 2000-08-07 2000-08-07)
))

'((s "") 
 "stepstonesøknad"
 (2000-08-06 2000-08-14)
 5
 lbs
 mb
 (("Skaffe tilgang til tavlen" nil) 
  ("skrive tavlesøknad (bør være noe annerledes enn andre søknader, da
studenter er målgruppen)" nil)
  ("Sett opp søknad på tavlen" nil))
 2000-08-07 
 2000-08-07)

(setf *pm-db*
(mapc #'(lambda (x)
	  (asetf (first (third x)) (decode-iso-date (symbol-name it)))
	  (asetf (second (third x)) (decode-iso-date (symbol-name it)))
	  (asetf (nth 7 x) (decode-iso-date (symbol-name it)))
	  (asetf (nth 8 x) (decode-iso-date (symbol-name it))))
      *pm-db*))

;; (sort* *pm-db* #'< (now))
;; (mapcar #'second *pm-db*)


(defconst *pm-db* ())
(defconst *pm-db-file* (concat *lynx-proxy-dir* "lynx-favorites"))

(defun pm-show-db ()
  "Shows favorite urls in temporary buffer *pm-info*."
  (interactive)
  (with-output-to-temp-buffer "*pm-info*"
    (loop for f in *pm-db*
	  for i below (length *pm-db*) do
	  (princ (format "%d:  %s\n" i f)))))

(defun pm-write-db ()
  (with-temp-file *pm-db-file*
    (overwrite-safe (prin1-to-string *pm-db*))))

(defun pm-read-db ()
  (with-temp-buffer
    (insert-file *pm-db-file*)
    (setq *pm-db* (read (current-buffer)))))

(unless *pm-db* (pm-read-db)) ;(nilf *pm-db*)
(add-hook 'kill-emacs-hook 'pm-write-db)
(push (cons *pm-db-file* 'iso-8859-1) file-coding-system-alist)

(defun* time-string (time-ivs &optional (length 50) &rest args)
  (let ((s (make-string 50 ?-)))
    (reduce #'(lambda (x y)
		(if y (replace x (make-string (- (second y) (first y)) ?*) :start1 (first y))) x)
	    (mapcar #'(lambda (x) (i-intersection (list 0 (length s)) (apply #'time-map-iv x args)))
		    time-ivs)
	    :initial-value s)))
;;(time-string (list (today :day 2)) :referance-time (now :day 0) :unit :hour)
;;(setf debug-on-error t)

(defun pm-task-string (task &rest args)
  (let ((columns '(20 50)))
    (concat (replace (make-string (first columns) ? ) (second task))
	    (apply #'time-string (list (third task))
		   (second columns) args))))
;(setq qwe '((s "") "employment" ((0 0 0 6 8 2000 0 t 7200) (0 0 0 1 9 2000 5 t 7200)) "5" "mb" "mb" () (0 0 0 7 8 2000 1 t 7200) (0 0 0 7 8 2000 1 t 7200)))
;;(asetf (third qwe) (period :from (first it) :to (second it)))
;;(pm-task-string qwe :referance-time (now :month -2) :unit :month)
