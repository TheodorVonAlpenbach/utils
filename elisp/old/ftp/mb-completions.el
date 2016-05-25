;;; TODO 

;;; Reimplement: `default-directory', `file-name-all-completions',
;;; `file-name-completion'

(defun mb-file-relative-name (filename &optional directory)
  "Refinement of `file-relative-name'. Avoids filenames like '.'"
  (let ((res (file-relative-name filename directory)))
    (cond  ((string= res "./") "")
	   ((string= res ".") "")
	   (t res))))
;;(file-relative-name "" default-directory)
;;(mb-file-relative-name "" default-directory)

(defun mb-completion (string predicate flag)
  "Pretty much reimplementation of `read-file-name''s \(assumed\)
version of `try-completion'"
  (let* ((dir (file-name-directory string)))
    (if flag
      (mb-all-completions string dir)
      (mb-try-completion string dir))))
;;(completing-read "MB find file: " #'mb-completion nil nil default-directory nil nil)

(defun mb-all-completions (string dir)
  "Refinement of base `all-completions'"
  (mb-file-name-all-completions (mb-file-relative-name string dir) (or dir default-directory)))

(defun mb-file-name-all-completions (string dir)
  "Refinement of base `all-completions'"
  (file-name-all-completions string dir))

(defun mb-try-completion (string dir)
  "Refinement of base `try-completion'"
  (let ((res (mb-file-name-completion (mb-file-relative-name string (or dir default-directory))
				      (or (not-empty dir) default-directory))))
    (or (eq res t) (concat dir res))))

(defun mb-file-name-completion (string dir)
  "Refinement of base `try-completion'"
  (file-name-completion string dir))

(defun mb-read-file-name (prompt )
  "Reimpl `read-file-name', `default-directory'"
  (read-file-name prompt dir))
;;(read-file-name "Test find file: ")

;;(ftp-put (ftp-get-process "ftps8.brinkster.com") buffer-file-name "Mats/mb-completions.el")
