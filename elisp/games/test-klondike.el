(require 'ert)
(require 'klondike)

(ert-deftest test-klondike-legal-visible-p ()
  "Test of `klondike-legal-visible-p'"
  (should (klondike-legal-visible-p 1 15))
  (should-not (klondike-legal-visible-p 1 16))
  (should (klondike-legal-visible-p 1 28))
  (should-not (klondike-legal-visible-p 1 41)))

(provide 'test-klondike)
