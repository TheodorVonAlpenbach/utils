(provide 'mb-print)

;;; printing utilities, using notepad
;;; TODO:
;;; 1) set better temporary filenames
;;; 2) handle read-only directories
;;; 2) set better temporary filenames

(defun print-notepad-file (file)
  "prints file using notepad"
  (interactive "f")
  (call-process "notepad" nil "*Messages*" nil "/p" (expand-file-name file)))

(defun print-notepad-region (beg end)
  "prints region using notepad"
  (interactive "r")
  (let ((file "temp-print-notepad-buffer"))
    (write-region beg end file)
    (print-notepad-file file)
    (delete-file file)))

(defun print-notepad-buffer ()
  "prints this buffer using notepad"
  (interactive)
  (if buffer-file-name 
      (print-notepad-file buffer-file-name)
    (print-notepad-region (point-min) (point-max))))

