(require 'ert)
(require 'ada-component-relations)

(ert-deftest test-include-relation-p ()
  "Test of `include-relation-p'"
  (should (include-relation-p :syllabus-folder '()))
  (should (include-relation-p :syllabus-folder :all))
  (should (include-relation-p :syllabus-folder '(:all)))
  (should (include-relation-p :syllabus-folder '(:syllabus-folder)))
  (should-not (include-relation-p :syllabus-folder 'qwe))
  (should-not (include-relation-p :syllabus-folder '(qwe))))

(provide 'test-ada-component-relations)
