(require 'org)

(cl-defun mb-org-kbd-maps ()
  (let ((mb-local-map (make-sparse-keymap))
	(move-map (make-sparse-keymap))
	(insert-map (make-sparse-keymap)))
    ;; Override evil bindings 
    (define-key (current-local-map) [tab] #'org-cycle)
    ;; Local map
    (key-chord-define evil-normal-state-local-map "gh" mb-local-map)
    ;; common
    (define-key mb-local-map "t" #'org-todo)
    ;; move
    (define-key mb-local-map "m" move-map)
    (define-key move-map "j" #'org-move-subtree-down)
    (define-key move-map "k" #'org-move-subtree-up)
    (define-key move-map "h" #'org-do-promote)
    (define-key move-map "l" #'org-do-demote)
    (define-key move-map "H" #'org-promote-subtree)
    (define-key move-map "L" #'org-demote-subtree)
    ;; insert
    (define-key mb-local-map "i" insert-map)
    (define-key insert-map "d" #'mb-org-insert-date)
    (define-key insert-map "t" #'mb-org-insert-time)
    (define-key insert-map "f" #'mb-org-insert-fact-of-today)
    (define-key insert-map "a" #'mb-org-insert-arrival)
    (define-key insert-map "h" #'mb-org-insert-departure)))

(cl-defun mb-org-init ()
  (mb-org-kbd-maps))

(add-hook 'org-mode-hook 'mb-org-init)

;;; Insert
(cl-defun insert-line-prefix (string)
  "Insert STRING at the beginning of the line where POINT is.
TODO: move this to some util module?"
  (save-excursion
    (bol)
    (insert string)))
;;(insert-line-prefix "qwe")

(cl-defun mb-org-insert-date (&optional (prefix "* ") (suffix ""))
  "Insert date in mb-org file"
  (interactive)
  (insert-line-prefix (concat prefix (iso-date) suffix "\n"))
  (forward-line 1))

(cl-defun mb-org-insert-time (&optional (prefix "** ") (time (now)) (suffix " "))
  "Insert time in mb-org file"
  (interactive)
  (insert-line-prefix (concat prefix (iso-time :time time) suffix "\n"))
  (eol))

(cl-defun mb-org-insert-fact-of-today (&rest args)
  "Insert header for the `fact of today' in mb-org file"
  (interactive)
  (let ((text (popf args :text "Dagens lærdom")))
    (apply #'mb-org-insert-time args)
    (insert text)
    (newline)))
;;(mb-org-insert-fact-of-today :text "Fact of today")

(cl-defun mb-org-insert-arrival (&optional (time (add-etime-time (now) :minute -5)))
  "Insert \"Ankomst\" stamp in mb-org file"
  (interactive)
  (eob t)
  (just-one-blank-line 1)
  (newline)
  (mb-org-insert-date)
  (mb-org-insert-time "** " time " Ankomst")
  (eob t))

(cl-defun mb-org-insert-departure (&optional (time (add-etime-time (now) :minute 5)))
  "Insert \"Hjem\" stamp in mb-org file"
  (interactive)
  (mb-org-insert-time "** " time " Hjem")
  (eob t))

(provide 'mb-org)
