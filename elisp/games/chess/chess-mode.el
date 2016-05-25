;;; todo
;;; keymap
;;; move control
;;; computer control
;;; debug commands

(defconst chess-buffer-name "*chess*")

(defun chess ()
  (switch-to-buffer chess-buffer-name)
  (chess-mode))

(defun chess-mode () 
  "Mode for playing chess. Creates a new chess-board 
buffer with name `chess-buffer-name' if no such buffer is already existing.

 \\{chess-mode-map}
 \\<chess-mode-map>"
  (interactive)
  (kill-all-local-variables)
  (use-local-map chess-mode-map)
  (setq major-mode 'chess-mode)
  (setq mode-name "Chess mode")
  (setq buffer-offer-save t)		;but why?
  (set (make-local-variable 'font-lock-defaults) '(chess-font-lock-keywords))
  (set (make-local-variable 'chess-local-url-refs) ())
  (set (make-local-variable '*chess-current-url*) nil)
  (set (make-local-variable 'chess-current-proxy-refs-regexp<) ())
  (set (make-local-variable '*chess-history*) ())
  (set (make-local-variable '*chess-home-url*) *chess-home-url*)
  (set (make-local-variable '*chess-url-jobs*) *chess-url-jobs*)
  (setf fill-prefix "   ")
  (make-local-hook 'chess-mode-hook)
  (run-hooks 'text-mode-hook 'chess-mode-hook))

(defconst chess-mode-map () "Keymap used in chess mode."
  )