;;; mb-print.el --- Print files and buffers through Notepad -*- lexical-binding: t; -*-

;;; Commentary:

;; Print files through an external program using Notepad's /p convention by
;; default.  Region and non-file buffer printing use securely generated
;; temporary files that are removed even when printing fails.

;;; Code:

(defgroup mb-print nil
  "Personal external printing commands."
  :group 'applications)

(defcustom mb-print-program "notepad"
  "External program used by `mb-print-file'."
  :type 'string
  :group 'mb-print)

(defcustom mb-print-program-arguments '("/p")
  "Arguments passed to `mb-print-program' before the file name."
  :type '(repeat string)
  :group 'mb-print)

(defcustom mb-print-output-buffer "*Messages*"
  "Buffer receiving output from `mb-print-program'."
  :type 'string
  :group 'mb-print)

(defun mb-print--executable ()
  "Return the executable for `mb-print-program' or report its absence."
  (or (executable-find mb-print-program)
      (user-error "Print program is not executable: %s" mb-print-program)))

(defun mb-print-file (file)
  "Print FILE using `mb-print-program'.
Report a nonzero process status instead of treating it as success."
  (interactive "fFile to print: ")
  (let ((path (expand-file-name file)))
    (unless (file-readable-p path)
      (user-error "Print file is not readable: %s" path))
    (let* ((program (mb-print--executable))
           (status
            (apply #'call-process
                   program nil mb-print-output-buffer nil
                   (append mb-print-program-arguments (list path)))))
      (unless (and (integerp status) (zerop status))
        (user-error "Print program failed for %s: %s" path status))
      status)))

(defalias 'print-notepad-file #'mb-print-file)

(defun mb-print-region (beg end)
  "Print the region between BEG and END using a temporary text file."
  (interactive "r")
  (let ((file (make-temp-file "mb-print-" nil ".txt")))
    (unwind-protect
        (progn
          (write-region beg end file nil 'silent)
          (mb-print-file file))
      (when (file-exists-p file)
        (delete-file file)))))

(defalias 'print-notepad-region #'mb-print-region)

(defun mb-print-buffer ()
  "Print the current buffer.
Print the visited file when there is one; otherwise print the current
buffer contents through `mb-print-region'."
  (interactive)
  (if buffer-file-name
      (mb-print-file buffer-file-name)
    (mb-print-region (point-min) (point-max))))

(defalias 'print-notepad-buffer #'mb-print-buffer)

(provide 'mb-print)

;;; mb-print.el ends here
