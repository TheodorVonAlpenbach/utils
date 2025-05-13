(cl-defun mb-php-mode-hook-function ()
  (display-line-numbers-mode)
  ;;(setq tab-width 4)
  ;; to setup tabs
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setf comment-start "//")
  (setf comment-end ""))


