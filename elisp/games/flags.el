(defun flags-buffer ()
  (get-buffer-create "*Flags*"))

(defun flags-test-ord (q-f)
  (cl-destructuring-bind (q &rest fasits) q-f
    (let* ((answer (read-string (format "%s: " q) nil nil nil t))
	   (res (find answer fasits :test #'string=)))
      (read-string (format "%s Trykk Enter for å fortsette..."
		       (if res
		       "Riktig!"
		       (format "Feil. Du svarte '%s'. Riktig svar er '%s'." answer (first fasits)))))
      (list res answer q fasits))))
;;(flags-buffer)

(defun flags-submit-row ()
  (interactive)
  (flags-check-answer)
  (newline))

(cl-defun flags-insert-instructions ()
  (insert (format "Spill Gloseprøve!")))

;;(insert (tab-format '(("foo" 1 "bar") ("qwe" 1233456 "qwebar")) :header '("qwe" "ewq" "qwebar")))
(defun flags-result-stats (list)
  "TODO: list all alternative fasits, not only the first one"
  (loop for (res answer q fasits) in list
     collect (list answer (if res "Riktig" "Feil") (first fasits)) into table
     count res into num-correct
     count (not res) into num-wrong
     finally return (list table num-correct num-wrong)))

(defun flags-show-result (list)
  (erase-buffer)
  (destructuring-bind (table num-correct num-wrong)
      (flags-result-stats list)
    (insert (if (zerop num-wrong)
	      "Gratulerer, du svarte riktig på alt!\n\n"
	      (format "Du fikk %d riktige og %d gale svar.\n\n" num-correct num-wrong)))
    (insert (tab-format table :header '("Ditt svar" "Resultat" "Fasit")))))

(defvar *gloselistefil* (expand-file-name "games/ord.el" *mb-lisp-dir*))
(defvar *flags* nil)
(defvar *current-glose* nil)
(defvar *flags-session-history* nil)

(defun flags-read (&optional rehearse-p)
  (if rehearse-p
    (loop for (res nil q (a)) in (first *flags-session-history*)
	  unless res collect (list q a))
    (read* *flagsfil*)))

(cl-defun flags-new (&key (force-new-game-p nil) (show-instructions-p t) (rehearse-p nil))
  (interactive)
  (when (or force-new-game-p
	    (yes-or-no-p "Do you want to quit this game? "))
    (erase-buffer)
    (when show-instructions-p
      (flags-insert-instructions))
    (setf *flags* (flags-read rehearse-p))
    (let ((session (loop for x in *flags* collect (flags-test-ord x))))
      (push (copy-tree session) *flags-session-history*)
      (flags-show-result session))))

(defun flags (&optional rehearse-p)
  "Start a new Flags game.
If optional argument rehearse-p is non nil, the game based on
errors in last session only"
  (interactive)
  (switch-to-buffer (flags-buffer))
  (activate-input-method 'norwegian-keyboard)
  (flags-new :force-new-game-p nil :rehearse-p rehearse-p))
;;(flags)

(provide 'flags)
