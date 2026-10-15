;;; test-mb-utils-buffer.el --- Tests for mb-utils-buffer -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-utils-buffer)

(ert-deftest mb-utils-buffer-metadata ()
  (with-temp-buffer
    (rename-buffer "mb-utils-buffer-test" t)
    (emacs-lisp-mode)
    (should (eq (buffer-major-mode (current-buffer)) 'emacs-lisp-mode))
    (should (major-mode-p 'emacs-lisp-mode (buffer-name)))
    (should (empty-buffer-p))
    (insert "content")
    (should-not (empty-buffer-p))))

(ert-deftest mb-utils-buffer-file-name-parts ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/example/file.el")
    (should (equal (buffer-file-name-nondirectory) "file.el"))
    (should (equal (buffer-directory) "/tmp/example/"))))

(ert-deftest mb-utils-buffer-marked-text ()
  (with-temp-buffer
    (insert (propertize "abcdef" 'face 'bold))
    (mark-region '(2 5))
    (setq mark-active t)
    (should (equal (marked-text-region) '(2 5)))
    (should (equal (marked-text) "bcd"))
    (should (eq (get-text-property 0 'face (marked-text t)) 'bold))))

(ert-deftest mb-utils-buffer-clear-other-buffer ()
  (let ((buffer (generate-new-buffer " *mb-utils-buffer-clear*")))
    (unwind-protect
	(progn
	  (with-current-buffer buffer (insert "content"))
	  (buffer-clear buffer)
	  (should (empty-buffer-p buffer)))
      (kill-buffer buffer))))

(ert-deftest mb-utils-buffer-do-regions ()
  (with-temp-buffer
    (insert "a1 b22 c333")
    (let (matches)
      (buffer-do-regions beg end ("[0-9]+")
	(push (buffer-substring beg end) matches))
      (should (equal (nreverse matches) '("1" "22" "333"))))))

(ert-deftest mb-utils-buffer-line-navigation ()
  (with-temp-buffer
    (insert "zero\none\ntwo\n")
    (goto-char 3)
    (should (= (buffer-line-point 0) 1))
    (should (= (buffer-line-point 1) 6))
    (should (= (buffer-line-point -1) 10))
    (should (= (point) 3))
    (should (equal (buffer-tail 2) "one\ntwo\n"))
    (should (equal (buffer-substring* :start 1 :end 5) "zero")))
  (with-temp-buffer
    (insert "first\n\nsecond\n\nthird")
    (goto-char 3)
    (should (= (buffer-paragraph-point 1) 7))
    (should (= (buffer-paragraph-point -1) 15))
    (should (= (point) 3))))

(ert-deftest mb-utils-buffer-insert-and-overwrite ()
  (with-temp-buffer
    (insert "abcd")
    (insert-at "X" 3 2)
    (should (equal (buffer-string) "abXXcd"))
    (overwrite-region "!" 2 5)
    (should (equal (buffer-string) "a!cd"))))

(ert-deftest mb-utils-buffer-region-operations ()
  (with-temp-buffer
    (insert "first\nsecond")
    (should (equal (region-lines (point-min) (point-max))
		   '("first" "second")))
    (region-replace "<%s>" '(1 6))
    (should (equal (buffer-string) "<first>\nsecond"))))

(ert-deftest mb-utils-buffer-whole-buffer-operations ()
  (with-temp-buffer
    (insert "a\nb")
    (goto-char 2)
    (should (equal (buffer-region) '(1 4)))
    (should (equal (buffer-lines) '("a" "b")))
    (should (equal (buffer-string*) "a\nb"))
    (should (= (point) 2))))

(ert-deftest mb-utils-buffer-line-operations ()
  (with-temp-buffer
    (insert "abc\ndefgh\n")
    (should (equal (line-region 2) '(5 10)))
    (should (equal (line-string :point 7) "defgh"))
    (should (equal (line-string :point 7 :to-point t) "de"))
    (should (equal (line-string :point 7 :from-point t) "fgh"))
    (should-error (line-string :to-point t :from-point t))))

(ert-deftest mb-utils-buffer-reverse-words ()
  (with-temp-buffer
    (insert "one two three")
    (reverse-words-1 (point-min) (point-max))
    (should (equal (buffer-string) "three two one"))))

(ert-deftest mb-utils-buffer-comments ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(message \"one\")\n(message \"two\")")
    (toggle-comment-region (point-min) (point-max))
    (goto-char (point-min))
    (should (commented-line-p))
    (forward-line 1)
    (should (commented-line-p))
    (toggle-comment-region (point-min) (point-max))
    (should (equal (buffer-string)
		   "(message \"one\")\n(message \"two\")"))))

(ert-deftest mb-utils-buffer-within-temp-file-cleans-up ()
  (let (path)
    (within-temp-file
      (setq path file)
      (insert "content"))
    (should-not (file-exists-p path))))

(ert-deftest mb-utils-buffer-save-as-1 ()
  (let ((source (make-temp-file "mb-utils-buffer-source-"))
	(target (make-temp-file "mb-utils-buffer-target-"))
	buffer)
    (delete-file target)
    (unwind-protect
	(progn
	  (with-temp-file source (insert "content"))
	  (setq buffer (find-file-noselect source))
	  (with-current-buffer buffer
	    (save-as-1 target)
	    (should (equal buffer-file-name target))
	    (should-not (buffer-modified-p)))
	  (should (equal (with-temp-buffer
			   (insert-file-contents target)
			   (buffer-string))
			 "content")))
      (when (buffer-live-p buffer)
	(kill-buffer buffer))
      (dolist (file (list source target))
	(when (file-exists-p file)
	  (delete-file file))))))

(ert-deftest mb-utils-buffer-clipboard-and-transpose ()
  (let (kill-ring kill-ring-yank-pointer)
    (string-to-clipboard (propertize "abc" 'face 'bold) t)
    (should (equal (clipboard-to-string) "abc"))
    (should-not (text-properties-at 0 (clipboard-to-string)))
    (should (eq (get-text-property 0 'face (clipboard-to-string t)) 'bold)))
  (should (equal (transpose-string "abc") "a\nb\nc"))
  (should (equal (transpose-string "") "")))

(provide 'test-mb-utils-buffer)

;;; test-mb-utils-buffer.el ends here
