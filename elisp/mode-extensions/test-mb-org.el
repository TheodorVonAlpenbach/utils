;;; test-mb-org.el --- Tests for mb-org -*- lexical-binding: t; -*-

(require 'ert)
(require 'mb-org)

(defconst mb-org-test-time
  (encode-time 0 5 14 17 10 2026))

(ert-deftest mb-org-timestamp-formatting ()
  (should (equal (mb-org--format-date mb-org-test-time) "2026-10-17"))
  (should (equal (mb-org--format-time mb-org-test-time) "14:05")))

(ert-deftest mb-org-date-and-time-insertion ()
  (with-temp-buffer
    (insert "existing")
    (goto-char 4)
    (mb-org-insert-date "* " " heading" mb-org-test-time)
    (should (equal (buffer-string)
                   "* 2026-10-17 heading\nexisting"))
    (should (bolp))
    (should (looking-at "existing")))
  (with-temp-buffer
    (insert "existing")
    (mb-org-insert-time "** " mb-org-test-time " Work")
    (should (equal (buffer-string) "** 14:05 Work\nexisting"))
    (should (eq (char-after) ?\n))))

(ert-deftest mb-org-fact-insertion ()
  (with-temp-buffer
    (mb-org-insert-fact-of-today
     :time mb-org-test-time
     :text "Fact"
     :suffix " ")
    (should (equal (buffer-string) "** 14:05 Fact\n"))
    (should (= (point) (point-max)))))

(ert-deftest mb-org-arrival-and-departure ()
  (let ((late-night (encode-time 0 58 23 31 12 2025)))
    (with-temp-buffer
      (insert "existing\n\n\n")
      (mb-org-insert-arrival late-night)
      (should
       (equal (buffer-string)
              (concat "existing\n\n"
                      "* 2025-12-31\n"
                      "** 23:58 Ankomst\n")))
      (should (= (point) (point-max)))))
  (with-temp-buffer
    (insert "tail")
    (goto-char 3)
    (mb-org-insert-departure mb-org-test-time)
    (should (equal (buffer-string) "** 14:05 Hjem\ntail"))
    (should (= (point) (point-max)))))

(ert-deftest mb-org-keymap-and-hook ()
  (should (memq #'mb-org-init org-mode-hook))
  (with-temp-buffer
    (org-mode)
    (should (eq (local-key-binding [tab]) #'org-cycle))
    (let ((map (mb-org-kbd-maps)))
      (should (keymapp map))
      (should (eq (lookup-key map "t") #'org-todo))
      (should (eq (lookup-key (lookup-key map "m") "j")
                  #'org-move-subtree-down))
      (should (eq (lookup-key (lookup-key map "i") "a")
                  #'mb-org-insert-arrival)))))

(provide 'test-mb-org)

;;; test-mb-org.el ends here
