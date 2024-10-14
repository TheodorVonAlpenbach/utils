(cl-defun stopwatch-buffer ()
  (get-buffer-create "*Stopwatch*"))

(cl-defun gloser-buffer-p ()
  (get-buffer "*Gloser*"))

(cl-defun gloser-test-ord (q-f)
  (cl-destructuring-bind (q &rest fasits) q-f
    (let* ((answer (read-string (format "%s: " q) nil nil nil t))
	   (res (find answer fasits :test #'string=)))
      (read-string (format "%s Trykk Enter for å fortsette..."
		       (if res
		       "Riktig!"
		       (format "Feil. Du svarte '%s'. Riktig svar er '%s'." answer (first fasits)))))
      (list res answer q fasits))))
;;(gloser-buffer)

(cl-defun gloser-submit-row ()
  (interactive)
  (gloser-check-answer)
  (newline))

(cl-defun gloser-insert-instructions ()
  (insert (format "Spill Gloseprøve!")))

;;(insert (tab-format '(("foo" 1 "bar") ("qwe" 1233456 "qwebar")) :header '("qwe" "ewq" "qwebar")))
(cl-defun gloser-result-stats (list)
  "TODO: list all alternative fasits, not only the first one"
  (cl-loop for (res answer q fasits) in list
     collect (list answer (if res "Riktig" "Feil") (first fasits)) into table
     count res into num-correct
     count (not res) into num-wrong
     finally return (list table num-correct num-wrong)))

(cl-defun gloser-show-result (list)
  (erase-buffer)
  (destructuring-bind (table num-correct num-wrong)
      (gloser-result-stats list)
    (insert (if (zerop num-wrong)
	      "Gratulerer, du svarte riktig på alt!\n\n"
	      (format "Du fikk %d riktige og %d gale svar.\n\n" num-correct num-wrong)))
    (insert (tab-format table :header '("Ditt svar" "Resultat" "Fasit")))))

(defvar *gloselistefil* (expand-file-name "ord.el"))
;;(setf *gloselistefil* (list (expand-file-name "quiz/koldens-quiz.txt" *shared-data-dir*)))
(defvar *gloseliste* nil)
(defvar *current-glose* nil)
(defvar *gloser-session-history* nil)

(cl-defun gloser-parse (source)
  (if (listp source)
    (cut (remove-if (bind #'string-match "^#\\|^[[:space:]]*$" 1) (string-lines (file-string (first source)))))
    (read* source)))
;;(gloser-parse '("/cygdrive/c/Users/eier/Google Drive/mb-data/quiz/koldens-quiz.txt"))
;;(gloser-parse *gloselistefil*)

(cl-defun gloser-read (&optional rehearse-p)
  (if rehearse-p
    (cl-loop for (res nil q (a)) in (first *gloser-session-history*)
	  unless res collect (list q a))
    (gloser-parse *gloselistefil*)))

(cl-defun gloser-new (&key (force-new-game-p nil) (show-instructions-p t) (rehearse-p nil))
  (interactive)
  (when (or force-new-game-p
 	    (not (gloser-buffer-p))
	    (yes-or-no-p "Do you want to quit this game? "))
    (switch-to-buffer (gloser-buffer))
    (activate-input-method 'norwegian-keyboard)
    (erase-buffer)
    (when show-instructions-p
      (gloser-insert-instructions))
    (setf *gloseliste* (gloser-read rehearse-p))
    (let ((session (cl-loop for x in *gloseliste* collect (gloser-test-ord x))))
      (push (copy-tree session) *gloser-session-history*)
      (gloser-show-result session))))

(cl-defun gloser (&optional rehearse-p)
  "Start a new Gloser game.
If optional argument rehearse-p is non nil, the game based on
errors in last session only"
  (interactive)
  (gloser-new :force-new-game-p nil :rehearse-p rehearse-p))
;;(gloser)

(cl-defun gloser-rehearse ()
  "Start a new Gloser game based on errors in last session only"
  (interactive)
  (if (first *gloser-session-history*)
    (gloser t)
    (message "Last session unavailable. Starting a new Gloser game")
    (gloser)))

(define-key global-map [(f12)] #'gloser)
(define-key global-map [(control f12)] #'gloser-rehearse)

(cl-defun gloser-parse-raw-tex-string (string)
  (cl-loop for l in (string-lines string t)
	collect (mapcar #'string-trim (split-string l ";"))))
;;(gloser-parse-raw-tex-string "sly; slu\ndistant; fjern\n")

(cl-defun gloser-parse-raw-tex-region (beg end)
  (interactive "r")
  (insert (pp (gloser-parse-raw-tex-string (region-string beg end)))))

(provide 'gloser)
