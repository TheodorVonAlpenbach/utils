(setq ie-default-user "Administrator")

(cl-defun ie-favorites-file (&optional (user ie-default-user))
  (concat user ".el"))
;;(ie-favorites-file)

(cl-defun ie-saved-favorites (&optional (user ie-default-user))
  (read* (ie-favorites-file user)))
;;(ie-saved-favorites)

(cl-defun ie-favorites-dir (&optional (user ie-default-user))
  (format "C:/Documents and Settings/%s/Favorites/" user))
;;(ie-favorites-dir "cw")

(cl-defun ie-favorites (&optional (user ie-default-user))
  "Returns a list of the paths to the current IE Favorites files"
  (set-difference (directory-files (ie-favorites-dir user)) '(".." ".") :test #'string=))
;;(ie-favorites)

(cl-defun ie-save-current-favorites (&optional (user ie-default-user))
  "Saves the list of the current IE Favorites to file"
  (print* (ie-favorites) (ie-favorites-file user)))
;;(ie-save-current-favorites)

(cl-defun ie-favorites-trash (&optional (user ie-default-user))
  "Returns a list of the paths to the current IE Favorites files that
are considered as trash favorites"
  (mapcar #'(lambda (x) (concat (ie-favorites-dir user) x))
	  (set-difference (ie-favorites) (ie-saved-favorites) :test #'string=)))
;;(ie-favorites-trash)

(cl-defun ie-reset-my-favorites (&optional (user ie-default-user))
  "Deletes all teh trash IE Favorite files while keeping the non-trash
files"
  (cl-loop for path in (ie-favorites-trash user) 
	if (file-directory-p path)
	do (delete-directory path)
	else
	do (delete-file path)))
;;(ie-reset-my-favorites)
