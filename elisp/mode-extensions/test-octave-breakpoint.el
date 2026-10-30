;;; test-octave-breakpoint.el --- Tests for octave-breakpoint -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'octave-breakpoint)

(defmacro test-octave-breakpoint--with-buffer (&rest body)
  "Run BODY in a temporary Octave function buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (octave-mode)
     (insert "function y = foo (x)\n"
             "  y = x;\n"
             "  y += 1;\n"
             "endfunction\n")
     (goto-char (point-min))
     ,@body))

(ert-deftest octave-breakpoint-manages-buffer-markers ()
  (test-octave-breakpoint--with-buffer
    (should (octave-set-buffer-breakpoint 2))
    (should-not (octave-set-buffer-breakpoint 2))
    (octave--goto-line 2)
    (should (octave-buffer-breakpoint-p))
    (should (equal (get-text-property
                    (- (line-end-position)
                       (length octave-breakpoint-string))
                    'display)
                   octave-breakpoint-display))
    (should (octave-unset-buffer-breakpoint 2))
    (should-not (octave-buffer-breakpoint-p))
    (should-error (octave-unset-buffer-breakpoint :all)
                  :type 'user-error)))

(ert-deftest octave-breakpoint-collects-function-locations ()
  (test-octave-breakpoint--with-buffer
    (octave-set-buffer-breakpoint 2)
    (octave-set-buffer-breakpoint 3)
    (should (equal (octave-buffer-breakpoints)
                   '(("foo" 2) ("foo" 3))))))

(ert-deftest octave-breakpoint-parses-and-composes-dbstops ()
  (should
   (equal
    (octave-dbstop-list-1 "breakpoint in foo at lines 2, 7.")
    '("foo" (2 7))))
  (should-error (octave-dbstop-list-1 "not a breakpoint"))
  (should (equal (octave-set-dbstops-1 "foo" '(2 7))
                 "dbstop foo 2 7"))
  (should (equal (octave-set-dbstops-1 nil 3)
                 "dbstop 3"))
  (cl-letf (((symbol-function 'octave-send-string)
             (lambda (command &optional _show-output-p)
               (should (equal command "dbstatus foo"))
               (concat "breakpoint in foo at line 2.\n"
                       "breakpoint in bar at lines 4, 8."))))
    (should (equal (octave-dbstop-list "foo")
                   '(("foo" (2)) ("bar" (4 8)))))))

(ert-deftest octave-breakpoint-updates-marked-buffers ()
  (let ((buffer (generate-new-buffer " *octave-breakpoint-test*"))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'octave-source-buffer)
                   (lambda (no-help-p source-buffer)
                     (push (list 'source no-help-p source-buffer) calls)))
                  ((symbol-function 'octave-set-dbstops)
                   (lambda (function lines show-output-p)
                     (push (list 'set function lines show-output-p) calls))))
          (octave-update-dbstops
           (list (list buffer '(("foo" 2) ("bar" (3 5))))))
          (should
           (equal (nreverse calls)
                  `((source t ,buffer)
                    (set "foo" 2 t)
                    (set "bar" (3 5) t)))))
      (kill-buffer buffer))))

(ert-deftest octave-breakpoint-cleans-up-temporary-dbstop-on-error ()
  (let (calls)
    (cl-letf (((symbol-function 'octave-breakpoint-p)
               (lambda (_pos) nil))
              ((symbol-function 'octave-set-breakpoint)
               (lambda (pos) (push (list 'set pos) calls)))
              ((symbol-function 'octave-unset-breakpoint)
               (lambda (pos) (push (list 'unset pos) calls))))
      (should-error
       (octave-with-temp-dbstop 12
         (error "body failed")))
      (should (equal (nreverse calls)
                     '((set 12) (unset 12)))))))

(ert-deftest octave-breakpoint-preserves-existing-temporary-dbstop ()
  (let (changed)
    (cl-letf (((symbol-function 'octave-breakpoint-p)
               (lambda (_pos) t))
              ((symbol-function 'octave-set-breakpoint)
               (lambda (_pos) (setq changed t)))
              ((symbol-function 'octave-unset-breakpoint)
               (lambda (_pos) (setq changed t))))
      (should (eq (octave-with-temp-dbstop 12 :result) :result))
      (should-not changed))))

(ert-deftest octave-breakpoint-navigation-wraps ()
  (test-octave-breakpoint--with-buffer
    (octave-set-buffer-breakpoint 2)
    (octave-set-buffer-breakpoint 3)
    (goto-char (point-min))
    (octave-forward-breakpoint)
    (should (= (line-number-at-pos) 2))
    (octave-forward-breakpoint)
    (should (= (line-number-at-pos) 3))
    (octave-forward-breakpoint)
    (should (= (line-number-at-pos) 2))
    (octave-backward-breakpoint)
    (should (= (line-number-at-pos) 3))))

(provide 'test-octave-breakpoint)

;;; test-octave-breakpoint.el ends here
