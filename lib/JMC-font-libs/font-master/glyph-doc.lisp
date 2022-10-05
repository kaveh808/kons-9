(in-package :glyph)
(defun document (package doc-strings)
  (let ((package (find-package package)))
    (dolist (doc-string doc-strings)
      (destructuring-bind (fn . doc) doc-string
	(setf (documentation (find-symbol (symbol-name fn) package) 'function) doc)))))

(document :glyph
          '(;;Identifiers
            (index
             .
             "The index used to look up the glyph in the font tables.")
            (name
             .
             "Returns the postscript name, if any, of the glyph.")
            (font
             .
             "The font the glyph belongs to.")
           
            ;;glyph measurements
            (em
             .
             "The size of the grid within which, at least historically, the glyph resides. Used to scale fonts for display.")
            (bounding-box
             .
             "Returns a vector of x-min y-min x-max y-max bounding the glyph.")
            (x-min
	     .
	     "Distance from origin to left side of bounding box.") ;only horizontal origin?
	    (x-max
	     .
	     "Distance from origin to right side of bounding box.")
	    (y-min
	     .
	     "Distance from origin to bottom of bounding box.")
	    (y-max
	     .
	     "Distance from origin to top of bounding box.")            
            (width
	     .
	     "Horizontal extent of a glyph. (- x-max m-min)")
	    (height
	     .
	     "Vertical extent of a glyph.(- y-max y-min")
            (size
             .
             "(width . height)")
            
            ;;environment measurements
            (left-side-bearing
             .
	     "The horizontal distance from the horizontal origin to the glyph's left bounding box edge. Mostly positive values for horizontal scripts. Also known as bearingY in some systems.")
	    (right-side-bearing
	     .
	     "In horizontal scripts describes the
distance from the bounding box's right edge to the advance width.")
	    (top-side-bearing
	     .
	     "The vertical distance from the vertical origin to the top of the glyph's bbox. It is usually positive for horizontal layouts, and negative for vertical ones. AKA bearingY.")
	    (bottom-side-bearing
	     .
	     "The vertical distance from the bottom of a glyph's bounding box to the advance height.")

	   

	    (advance-width
	     .
	     "Distance to advance horizontally after rendering this glyph. Does not take into account kerning nor tracking.
In FreeType and other systems this is called advanceX.")
	    (advance-height
	     .
	     "Distance to advance vertically after rendering this glyph.  Does not take into account kerning nor tracking.
In FreeType and other systems this is also called advanceY.")

	    (origin
	     .
	     "Returns (cons dx dy) from the top left corner to glyph origin.");say this isn't so blitting centric.

	    (kerning
	     .
	     "Returns the kerning distance from glyph1 to glyph2.")
	    (kerning-pairs
	     .
	     "Returns a list of all kerning pairs for the given glyph.")
            (data
             .
             "Returns the raw data for a glyph as parsed by the specific font reader.")
            (paths
             .
             "Returns the glyph data as a cl-vectors path.")
            (raster
             .
             "Returns three values, the glyph in raster format, optionally at given pixels per em, a cons of width height and a cons of offset x y.")
            (anchors
             .
             "Returns all the anchor points for a glyph.")))
