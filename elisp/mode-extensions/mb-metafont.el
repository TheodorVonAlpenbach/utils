;;; mb-metafont.el --- Compile and view Metafont files -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(defvaralias '*meta-compilation-buffer-name* 'meta-compilation-buffer-name)

(defvar meta-compilation-buffer-name "*Metafont-compilation*"
  "Name of the buffer used for Metafont process output.")

(defun meta-compilation-buffer ()
  "Return the Metafont compilation buffer, or nil if it does not exist."
  (get-buffer meta-compilation-buffer-name))

(defun meta-check-compilation (&optional buffer)
  "Return the generated GF filename reported in BUFFER.
BUFFER defaults to the Metafont compilation buffer.  Signal an
error when the buffer has no Metafont output or reports no output
file."
  (let ((compilation-buffer (or buffer (meta-compilation-buffer))))
    (unless (buffer-live-p compilation-buffer)
      (error "No Metafont compilation buffer"))
    (with-current-buffer compilation-buffer
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward "^This is METAFONT" nil t)
          (error "Metafont did not produce recognizable output"))
        (unless (re-search-forward
                 "Output written on \\([^][()[:space:]]+\\)" nil t)
          (error "Metafont did not report an output file"))
        (match-string-no-properties 1)))))

(cl-defun meta-compile-file (filename &optional (show-buffer-p t))
  "Compile FILENAME with Metafont and return its generated GF filename.
When SHOW-BUFFER-P is non-nil, display the compilation buffer."
  (let* ((absolute-file (expand-file-name filename))
         (default-directory (file-name-directory absolute-file))
         (buffer (get-buffer-create meta-compilation-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (let ((status
           (call-process
            "mf" nil buffer nil (file-name-nondirectory absolute-file))))
      (when show-buffer-p
        (display-buffer buffer))
      (unless (and (integerp status) (zerop status))
        (error "Metafont failed for %s with status %s" filename status))
      (meta-check-compilation buffer))))

(defun gftodvi (filename)
  "Convert the GF file FILENAME to DVI and return the DVI filename.
Signal an error when `gftodvi' fails."
  (let* ((absolute-file (expand-file-name filename))
         (default-directory (file-name-directory absolute-file))
         (buffer (get-buffer-create "*GFtoDVI-compilation*"))
         status)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (setq status
          (call-process
           "gftodvi" nil buffer nil
           (file-name-nondirectory absolute-file)))
    (unless (and (integerp status) (zerop status))
      (error "gftodvi failed for %s with status %s" filename status))
    (file-name-with-extension
     (file-name-sans-extension absolute-file) "dvi")))

(defun meta-eval-buffer (&optional buffer)
  "Compile the Metafont file visited by BUFFER and display its DVI output.
BUFFER defaults to the current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless buffer-file-name
      (user-error "Buffer is not visiting a file"))
    (save-buffer)
    (let* ((source buffer-file-name)
           (gf-file (meta-compile-file source nil))
           (dvi-file
            (gftodvi (expand-file-name gf-file
                                      (file-name-directory source))))
           (dvi-buffer (find-file-other-window dvi-file)))
      (with-current-buffer dvi-buffer
        (revert-buffer t t))
      dvi-buffer)))

(provide 'mb-metafont)

;;; mb-metafont.el ends here
