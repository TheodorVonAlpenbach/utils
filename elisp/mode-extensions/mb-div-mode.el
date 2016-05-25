(defun mb-php-mode-hook-function ()
  (linum-mode)
  ;;(setq tab-width 4)
  ;; to setup tabs
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setf comment-start "//")
  (setf comment-end ""))

(defun mb-js-mode-hook-function ()
  (linum-mode)
  ;;(setq tab-width 4)
  ;; to setup tabs
  (setq js-indent-level 4)
  (setq tab-width 4)
  (setq indent-tabs-mode t))

