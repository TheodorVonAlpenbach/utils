;;; test-mb-frames.el --- Tests for mb-frames -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mb-frames)

(ert-deftest mb-frames-resizes-selected-frame-vertically ()
  (let (resized)
    (cl-letf (((symbol-function 'selected-frame)
               (lambda () 'selected))
              ((symbol-function 'frame-height)
               (lambda (frame)
                 (should (eq frame 'selected))
                 40))
              ((symbol-function 'set-frame-height)
               (lambda (frame size)
                 (setq resized (list frame size)))))
      (mb-frames-resize 3)
      (should (equal resized '(selected 37))))))

(ert-deftest mb-frames-resizes-explicit-frame-horizontally ()
  (let (resized)
    (cl-letf (((symbol-function 'selected-frame)
               (lambda () (ert-fail "Selected frame should not be used")))
              ((symbol-function 'frame-width)
               (lambda (frame)
                 (should (eq frame 'other))
                 100))
              ((symbol-function 'set-frame-width)
               (lambda (frame size)
                 (setq resized (list frame size)))))
      (mb-frames-resize 15 t 'other)
      (should (equal resized '(other 85))))))

(ert-deftest mb-frames-normalizes-raw-prefixes ()
  (let (resized)
    (cl-letf (((symbol-function 'selected-frame)
               (lambda () 'selected))
              ((symbol-function 'frame-height)
               (lambda (_frame) 40))
              ((symbol-function 'set-frame-height)
               (lambda (_frame size) (setq resized size))))
      (mb-frames-resize '(4))
      (should (= resized 36)))))

(ert-deftest mb-frames-enlarge-commands-use-negative-deltas ()
  (let (arguments)
    (cl-letf (((symbol-function 'mb-frames-resize)
               (lambda (&rest values) (setq arguments values))))
      (mb-frames-enlarge 3 'frame)
      (should (equal arguments '(-3 nil frame)))
      (mb-frames-enlarge-horizontally '(4) 'frame)
      (should (equal arguments '(-4 t frame))))))

(ert-deftest mb-frames-shrink-horizontal-command-forwards-prefix ()
  (let (arguments)
    (cl-letf (((symbol-function 'mb-frames-resize)
               (lambda (&rest values) (setq arguments values))))
      (mb-frames-shrink-horizontally '(4) 'frame)
      (should (equal arguments '((4) t frame))))))

(ert-deftest mb-frames-rejects-nonpositive-result ()
  (cl-letf (((symbol-function 'selected-frame)
             (lambda () 'selected))
            ((symbol-function 'frame-height)
             (lambda (_frame) 3)))
    (should-error (mb-frames-resize 3) :type 'user-error)
    (should-error (mb-frames-resize 4) :type 'user-error)))

(ert-deftest mb-frames-compatibility-aliases ()
  (dolist (pair
           '((shrink-frame . mb-frames-resize)
             (enlarge-frame . mb-frames-enlarge)
             (shrink-frame-horizontally
              . mb-frames-shrink-horizontally)
             (enlarge-frame-horizontally
              . mb-frames-enlarge-horizontally)))
    (should
     (eq (indirect-function (car pair))
         (indirect-function (cdr pair))))
    (should (commandp (car pair)))))

(provide 'test-mb-frames)

;;; test-mb-frames.el ends here
