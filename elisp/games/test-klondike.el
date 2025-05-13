(require 'ert)
(require 'klondike)

(ert-deftest test-klondike-legal-visible-p ()
  "Test of `klondike-legal-visible-p'"
  (should (klondike-legal-visible-p 1 15))
  
  (should-not (klondike-legal-visible-p 1 16))
  (should (klondike-legal-visible-p 1 28))
  (should-not (klondike-legal-visible-p 1 41)))

(ert-deftest test-klondike-parse-column ()
  "Test of `klondike-parse-column'"
  (should (equal (klondike-parse-column "a") 0))
  (should (equal (klondike-parse-column "0") 0))
  (should (equal (klondike-parse-column "b") 1))
  (should (equal (klondike-parse-column "g") 6))
  (should (equal (klondike-parse-column "6") 6)))

(provide 'test-klondike)
