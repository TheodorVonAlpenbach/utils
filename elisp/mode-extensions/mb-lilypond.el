(defun LilyPond-command (name file)
  "Run command NAME on the file you get by calling FILE.

FILE is a function return a file name.  It has one optional argument,
the extension to use on the file.

Use the information in LilyPond-command-alist to determine how to run the
command."

  (let ((entry (assoc name LilyPond-command-alist)))
    (if entry
      (let ((command (LilyPond-command-expand (cadr entry)
					      (apply file nil)))
	    (jobs nil)
	    (job-string "no jobs"))
	(cond ((member name (list "View"))
	       ;; simple implementation of View...
	       (let ((pdf-path (concat (file-name-sans-extension (apply file nil)) ".pdf")))
		 (find-file pdf-path)))
	      ;; here is the old implementation
	      ((member name (list "View" "ViewPS"))
	       ;; is USR1 a right signal for viewps?
	       (let ((buffer-xdvi (get-buffer-create (concat "*" name "*"))))
		 ;; what if XEDITOR is set to gedit or so, should we steal it?
		 (if (not (getenv "XEDITOR"))
		   (setenv "XEDITOR" "emacsclient --no-wait +%l:%c %f"))
		 (if LilyPond-kick-xdvi
		   (let ((process-xdvi (get-buffer-process buffer-xdvi)))
		     (if process-xdvi
		       (signal-process (process-id process-xdvi) 'SIGUSR1)
		       (LilyPond-shell-process name buffer-xdvi command)))
		   (LilyPond-shell-process name buffer-xdvi command))))
	      (t
	       (if (string-equal name "Midi")
		 (progn
		   (setq command (concat LilyPond-midi-command " " (LilyPond-string-current-midi)))
		   (if (LilyPond-kill-midi)
		     (setq job-string nil)))) ; either stop or start playing
	       (if (string-equal name "MidiAll")
		 (progn
		   (setq command (concat LilyPond-all-midi-command " " (LilyPond-string-all-midi)))
		   (LilyPond-kill-midi))) ; stop and start playing
	       (if (and (member name (list "Midi" "MidiAll")) job-string)
		 (if (file-newer-than-file-p
		      (LilyPond-get-master-file)
		      (concat (substring (LilyPond-get-master-file) 0 -3) ".midi"))
		   (if (y-or-n-p "Midi older than source. Reformat midi?")
		     (progn
		       (LilyPond-command-formatmidi)
		       (while (LilyPond-running)
			 (message "Starts playing midi once it is built.")
			 (sit-for 0 100))))))
	       (if (member name (list "LilyPond" "TeX" "2Midi" "2PS"
				      "Book" "LaTeX"))
		 (if (setq jobs (LilyPond-running))
		   (progn
		     (setq job-string "Process") ; could also suggest compiling after process has ended
		     (while jobs
		       (setq job-string (concat job-string " \"" (pop jobs) "\"")))
		     (setq job-string (concat job-string " is already running; kill it to proceed "))
		     (if (y-or-n-p job-string)
		       (progn
			 (setq job-string "no jobs")
			 (LilyPond-kill-jobs)
			 (while (LilyPond-running)
			   (sit-for 0 100)))
		       (setq job-string nil)))))

	       (setq LilyPond-command-next
		     (let* ((entry (assoc name LilyPond-command-alist))
			    (next-command (nth 3 (cdr entry))))
		       (or next-command
			   LilyPond-command-default)))
	       
	       (if (string-equal job-string "no jobs")
		 (LilyPond-compile-file command name))))))))
