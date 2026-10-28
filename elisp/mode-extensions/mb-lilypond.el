;;; mb-lilypond.el --- Personal LilyPond helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'compile)
(require 'subr-x)

(defvar-local mb-lilypond-source-file nil
  "LilyPond source file associated with the current compilation buffer.")

(defun mb-lilypond-compile (&optional filename)
  "Compile FILENAME with LilyPond and display its PDF on success.
Interactively, compile the file visited by the current buffer."
  (interactive)
  (let ((source (or filename buffer-file-name)))
    (unless source
      (user-error "Current buffer is not visiting a file"))
    (let ((compilation-buffer
           (compilation-start
            (format "lilypond %s" (shell-quote-argument source)))))
      (with-current-buffer compilation-buffer
        (setq-local mb-lilypond-source-file source))
      compilation-buffer)))

(defun mb-lilypond-compile-view-pdf (compilation-buffer exit-status)
  "Display the generated PDF after a successful LilyPond compilation.
COMPILATION-BUFFER records the source file, and EXIT-STATUS is the
status supplied by `compilation-finish-functions'."
  (when (string= (string-trim exit-status) "finished")
    (let ((source
           (buffer-local-value 'mb-lilypond-source-file compilation-buffer)))
      (when source
        (let ((pdf-file
               (file-name-with-extension
                (file-name-sans-extension source) "pdf")))
          (when (file-exists-p pdf-file)
            (let ((pdf-buffer (find-file-other-window pdf-file)))
              (with-current-buffer pdf-buffer
                (revert-buffer t t)))))))))

(add-hook 'compilation-finish-functions #'mb-lilypond-compile-view-pdf)

(defun mb-lilypond--command-around (original name file)
  "Use Emacs to view PDFs, otherwise call ORIGINAL with NAME and FILE."
  (if (equal name "View")
      (find-file
       (file-name-with-extension
        (file-name-sans-extension (funcall file)) "pdf"))
    (funcall original name file)))

(with-eval-after-load 'LilyPond-mode
  (advice-add 'LilyPond-command :around #'mb-lilypond--command-around))

(provide 'mb-lilypond)

;;; mb-lilypond.el ends here
