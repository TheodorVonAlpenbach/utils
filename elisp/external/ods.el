(cl-defun ods->cvs (filename)
  (call-process* "unoconv" "--stdout" "-f" "csv" filename))
;;(ods->cvs "/home/mbe/projects/veracity/13_001_Miros_HeidrunFSO/sensor-names-13001.ods")

(cl-defun read-ods (filename &optional sheet)
  "Read Calc SHEET from FILENAME and convert it to a tree.
SHEET is an integer that represent the index of the sheet in the workbook.
TODO: write a clearer doc."
  (when sheet
    (warn "SHEET argument is ignored in this version!"))
  (parse-csv-string (ods->cvs filename) ","))
;;(read-ods "~/projects/veracity/13_001_Miros_HeidrunFSO/sensor-names-13001.ods")

(provide 'ods)
