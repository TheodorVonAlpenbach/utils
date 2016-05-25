(provide 'mb-vc)

(defun vc-log-prolog ()
  "Things to be done when checking in"
  (vc-diff nil t)
  (switch-to-buffer-other-window "*VC-log*")
  (next-line 1) ; hack! I don't why it doesn't work without this!
  (make-local-variable 'window-min-height)
  (setq window-min-height 7)
  (shrink-window-if-larger-than-buffer))

(add-hook 'vc-log-mode-hook 'vc-log-prolog)

(setq vc-path (cons "f:/Program Files/GNU/WinCvs" vc-path))
(setq vc-default-back-end "CVS")

(defun vc-backend-print-log (file)
  ;; Get change log associated with FILE. ;; Use log instead of rlog
  (vc-backend-dispatch 
   file
   (vc-do-command nil 0 "prs" file 'MASTER)
   (vc-do-command nil 0 "log" file 'MASTER) ;; rlog -> log
   (vc-do-command nil 0 "cvs" file 'WORKFILE "log"))) ;; rlog -> log
