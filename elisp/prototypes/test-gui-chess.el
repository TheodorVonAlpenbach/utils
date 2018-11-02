(require 'ert)
(require 'gui-chess)

(ert-deftest test-gcfa-type ()
  "Test of `gcfa-type'"
 (should (equal (gcfa-type "SCAWV0" "label") 
		"<GUI/RW/LeftP/Tanker/SCAWV0/type(string) = label>")))

(ert-deftest test-gcfa-geo ()
  "Test of `gcfa-geo'"
  (should (equal (gcfa-geo "SCAWV0" "1;2;3;4")
		 "<GUI/RW/LeftP/Tanker/SCAWV0/geometry(rect) = 1;2;3;4>")))

(ert-deftest test-gcfa-sensor ()
  "Test of `gcfa-sensor'"
  (should (equal (gcfa-sensor "SCAWV0" "SlamCountAWV")
		 "<GUI/RW/LeftP/Tanker/SCAWV0/sensor(string) = SlamCountAWV>")))

(ert-deftest test-gcfa-text ()
  "Test of `gcfa-text'"
  (should (equal (gcfa-text "SCAWV0" "Slamcounter 1")
		 "<GUI/RW/LeftP/Tanker/SCAWV0/text(string) = Slamcounter 1>")))

(ert-deftest test-gcfa-displaynumber ()
  "Test of `gcfa-displaynumber'"
 (should (equal (gcfa-displaynumber "SCAWV0" "1;2;3;4" "SlamCountAWV")
		"<GUI/RW/LeftP/Tanker/SCAWV0/type(string) = displaynumber>
<GUI/RW/LeftP/Tanker/SCAWV0/geometry(rect) = 1;2;3;4>
<GUI/RW/LeftP/Tanker/SCAWV0/sensor(string) = SlamCountAWV>")))

(ert-deftest test-gcfa-label ()
  "Test of `gcfa-label'"
 (should (equal (gcfa-label "SCAWV0" "1;2;3;4" "SlamCountAWV")
		"<GUI/RW/LeftP/Tanker/SCAWV0/type(string) = label>
<GUI/RW/LeftP/Tanker/SCAWV0/geometry(rect) = 1;2;3;4>
<GUI/RW/LeftP/Tanker/SCAWV0/text(string) = SlamCountAWV>")))

(provide 'test-gui-chess)
