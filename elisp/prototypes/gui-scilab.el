(require 'gui)

(defun gui-extract-scilab-positions (gui)
  "Calculate the envelopes of the rectangles in GUI."
  (if (eql (car gui) :frame)
    (loop for x in (third gui)
	  append (gui-extract-scilab-positions x))
    (if (stringp (car gui))
      (destructuring-bind (name (left top) (width height)) gui
	(list (format "%s = [%d, %d, %d, %d];"
		(concat "pos" (upcase-initials name))
		(round left) (round top)
		(round width) (round height)))))))
;;(gui-extract-scilab-positions (gui-geometry (os-gui))) 

(cl-defun gui-scilab-positions (gui &optional
				      (frame-position '(0 0))
				      (frame-name "frame")
				      (prefix "pos"))
  "Format the rectangle envelopes in GUI as Scilab assignments.
Also add a similar statement for the GUI's envelope. For
instance, a GUI containing only a rectangle with a name "Qwe",
and with a calculated envelope ((0 1) (2 3)) is converted to the
statements

posQwe = [0, 1, 2, 3];
posFrame = [0, 1, 2, 3];

With the optional PREFIX and FRAME-NAME you can define another
prefix for the Scilab variables and frame name, respectively. The
optional FRAME-POSITION shifts the entire system from origo.
Setting the three optional parameters to \"geo\", \"Bild\",
and (10 10), respectively, the two above statements become

geoQwe = [10, 11, 12, 13];
geoBild = [10, 11, 12, 13];"
  (concat* (append
	    (list (format "%s = [%d, %d, %d, %d];"
		    frame-name
		    (first frame-position) (second frame-position)
		    (first (last-elt gui)) (second (last-elt gui))))
	    (gui-extract-scilab-positions gui))
    :in "\n"))
;;(gui-scilab-positions (gui-geometry (os-gui))) 

(cl-defun gui-export-scilab-positions (gui filename &optional
						      (frame-position '(0 0))
						      (frame-prefix "posFrame")
						      (prefix "pos"))
  "Write the rectangle envelopes in GUI to FILENAME in Scilab format.
For a description of the export format and the optional
parameters, see `gui-scilab-positions'."
  (string-to-file (gui-scilab-positions gui frame-position frame-prefix)
		  filename))
;;(gui-export-scilab-positions (gui-geometry (os-gui)) "~/cvs/sources/SciLab/toolboxes/OsstrupenViewer/macros/OsCtrlPositions.sce" '(100 100)) 

(provide 'gui-scilab)
