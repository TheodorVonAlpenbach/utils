;;; test-octave-debug-mode.el --- Tests for octave-debug-mode -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'octave-debug-mode)

(ert-deftest octave-debug-mode-builds-independent-keymaps ()
  (should (eq (lookup-key (octave-debug-map) "r") #'octave-run))
  (should (keymapp (lookup-key octave-debug-mode-map "s")))
  (should (keymapp (lookup-key octave-debug-mode-map "i"))))

(ert-deftest octave-debug-mode-manages-source-marker ()
  (let ((first (generate-new-buffer " *octave-debug-first*"))
        (second (generate-new-buffer " *octave-debug-second*")))
    (unwind-protect
        (progn
          (with-current-buffer first
            (insert "one\ntwo\n"))
          (with-current-buffer second
            (insert "three\nfour\n"))
          (setq mb-octave-debug-marker nil)
          (mb-octave-set-debug-marker 2 first)
          (should (eq (marker-buffer mb-octave-debug-marker) first))
          (should (= (marker-position mb-octave-debug-marker) 5))
          (with-current-buffer first
            (should (eq overlay-arrow-position
                        mb-octave-debug-marker)))
          (mb-octave-set-debug-marker 1 second)
          (with-current-buffer first
            (should-not overlay-arrow-position))
          (with-current-buffer second
            (should (eq overlay-arrow-position
                        mb-octave-debug-marker)))
          (mb-octave-set-debug-marker nil)
          (should-not (marker-buffer mb-octave-debug-marker))
          (with-current-buffer second
            (should-not overlay-arrow-position)))
      (mb-octave-set-debug-marker nil)
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest octave-debug-mode-queries-debugger-state ()
  (let (calls)
    (cl-letf (((symbol-function 'octave-send-string)
               (lambda (command &optional show-output-p)
                 (push (list command show-output-p) calls)
                 (pcase command
                   ("dbstack" "stack frame")
                   ("dbwhere"
                    "stopped in foo at line 27 [/tmp/foo.m]")))))
      (should (octave-debug-p))
      (should (equal (octave-where) '(27 "foo")))
      (should (equal (nreverse calls)
                     '(("dbstack" nil) ("dbwhere" t))))))
  (cl-letf (((symbol-function 'octave-send-string)
             (lambda (&rest _args) "")))
    (should-not (octave-debug-p)))
  (should-error (octave-debug--parse-where "not stopped")))

(ert-deftest octave-debug-mode-exits-in-all-octave-buffers ()
  (let ((first (generate-new-buffer " *octave-debug-first*"))
        (second (generate-new-buffer " *octave-debug-second*")))
    (unwind-protect
        (progn
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (octave-mode)
              (insert "function y = foo (x)\nendfunction\n")
              (octave-debug-mode 1)))
          (mb-octave-set-debug-marker 1 first)
          (octave-exit-debug-mode)
          (should-not (marker-buffer mb-octave-debug-marker))
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (should-not octave-debug-mode))))
      (mb-octave-set-debug-marker nil)
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest octave-debug-mode-starts-from-current-buffer ()
  (with-temp-buffer
    (octave-mode)
    (let (calls)
      (cl-letf (((symbol-function 'octave-update-dbstops-buffer)
                 (lambda (buffer) (push (list 'update buffer) calls)))
                ((symbol-function 'octave-eval-defun-test)
                 (lambda () (push '(evaluate) calls))))
        (octave-debug-start)
        (should octave-debug-mode)
        (should (equal (nreverse calls)
                       `((update ,(current-buffer)) (evaluate))))))))

(ert-deftest octave-debug-mode-refreshes-current-function ()
  (with-temp-buffer
    (octave-mode)
    (insert "function y = foo (x)\n"
            "  y = x;\n"
            "endfunction\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'octave-debug-p)
               (lambda () t))
              ((symbol-function 'octave-where)
               (lambda () '(2 "foo"))))
      (unwind-protect
          (progn
            (octave-debug-refresh-display)
            (should octave-debug-mode)
            (should (= (line-number-at-pos) 2))
            (should (= (point) (line-end-position)))
            (should (= (marker-position mb-octave-debug-marker)
                       (line-beginning-position))))
        (mb-octave-set-debug-marker nil)))))

(ert-deftest octave-debug-mode-dispatches-step-and-last-run ()
  (let (calls)
    (cl-letf (((symbol-function 'octave-debug-p)
               (lambda () t))
              ((symbol-function 'octave-send-string)
               (lambda (command &optional show-output-p)
                 (push (list 'send command show-output-p) calls)))
              ((symbol-function 'octave-debug-refresh-display)
               (lambda () (push '(refresh) calls)))
              ((symbol-function 'octave-update-all-dbstops)
               (lambda () (push '(update) calls))))
      (octave-step)
      (octave-step-in)
      (octave-step-out)
      (let ((octave-last-debug-expression "foo_test ()"))
        (octave-run-last))
      (should
       (equal
        (nreverse calls)
        '((send "dbstep" t)
          (refresh)
          (send "dbstep in" t)
          (refresh)
          (send "dbstep out" t)
          (refresh)
          (update)
          (send "foo_test ()" t)
          (refresh)))))
    (let ((octave-last-debug-expression nil))
      (should-error (octave-run-last) :type 'user-error))))

(provide 'test-octave-debug-mode)

;;; test-octave-debug-mode.el ends here
