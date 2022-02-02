(require 'ert)
(require 'mb-evil)

(ert-deftest test-strip-ssh ()
  "Test of `strip-ssh'"
  (should
   (equal (strip-ssh "/home/Mats/git/utils/elisp/mode-extensions")
	  "/home/Mats/git/utils/elisp/mode-extensions"))
  (should
   (equal (strip-ssh "/ssh:pf:/home/mats_progfab_no/git/problem-server/")
	  "/home/mats_progfab_no/git/problem-server/")))

(provide 'test-mb-evil)
