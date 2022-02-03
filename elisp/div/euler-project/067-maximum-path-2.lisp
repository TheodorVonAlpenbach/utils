(require 'cl-utils "~/git/utils/elisp/div/euler-project/cl-utils.lisp")
(require 'triangle "~/git/utils/elisp/div/euler-project/triangle.lisp")

(defun 067-read-triangle ()
  (parse-triangle-lines
   (file->lines "~/git/utils/elisp/div/euler-project/data/p067_triangle.txt")))
;;(067-read-triangle)

(defun 067-solution (&optional (triangle (067-read-triangle)))
  (maximum-path-sum triangle))
;;(067-solution)
;; => 7273
;; (59 73 52 53 87 57 92 81 81 79 81 32 86 82 97 55 97 36 62 65 90 93
;;  95 54 71 77 68 71 94 8 89 54 42 90 84 91 31 71 93 94 53 69 73 99
;;  89 47 80 96 81 52 98 38 91 78 90 70 7 18 55 84 74 55 81 87 89 99
;;  73 88 95 68 37 87 73 77 60 82 87 64 96 65 47 94 85 51 87 65 65 66
;;  91 83 72 24 98 89 53 82 57 99 98 95)
