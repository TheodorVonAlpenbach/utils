(require 'sgml-mode)
(load "asp/insert")

(setq asp-insert-map (make-sparse-keymap "VBInsert"))
(define-key visual-basic-mode-map "\C-ci" asp-insert-map)
(define-key html-mode-map "\C-ci" asp-insert-map)
(define-key asp-insert-map "a" 'asp-insert-html-sec)
(define-key asp-insert-map "s" 'asp-insert-ss-script)
(define-key asp-insert-map "c" 'asp-insert-cs-script)
(define-key asp-insert-map "f" 'asp-include-file)
(define-key asp-insert-map "m" 'visual-basic-new-sub)

(define-key html-mode-map [(f11)] 'asp-toggle-vb-html-modes)
(define-key visual-basic-mode-map [(f11)] 'asp-toggle-vb-html-modes)

;; vb only
(define-key visual-basic-mode-map "\C-c\C-c" 'comment-region)
(define-key visual-basic-mode-map "\C-cq" 'indent-region)

;; html only
(define-key html-mode-map "\C-ci\r" 'asp-insert-endl)

(defun asp-toggle-vb-html-modes () ""
  (interactive)
  (if (string= mode-name "Visual Basic") (html-mode)
    (if (string= mode-name "HTML") (visual-basic-mode))))


