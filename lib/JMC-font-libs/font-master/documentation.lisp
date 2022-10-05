(in-package :font)
(defun document (package doc-strings)
  (let ((package (find-package package)))
    (dolist (doc-string doc-strings)
      (destructuring-bind (fn . doc) doc-string
	(setf (documentation (find-symbol (symbol-name fn) package) 'function) doc)))))

(document 'font '((open
                   .
                   "Given a pathname or string returns a parsed font object. Given an already open font object it should return the same object.")
                  (%open
                   .
                   "For implementors to add loading support. Type should be a keyword of the font type, e.g. :TTF")
                  (close
                   .
                   "Closes a font after use.  This is implementation defined whether it does anything to clean up.")
                  (em
                   .
                   "The size of the grid within which, at least historically, all glyphs in the font reside. Used to scale fonts for display.")
                  ;;copied from freetype?
                  (ascent .
		   "The distance from the baseline to the highest or upper grid coordinate used to place an outline point.
              It is a positive value, due to the grid's orientation with the Y axis upwards.")
		  (descent . "The distance from the baseline to the lowest grid coordinate used to place an outline point. In FreeType, this is a negative value, due to the grid's orientation. Note that in some font formats this is a positive value.")
		  (line-gap . "The distance that must be placed between two lines of text. The baseline-to-baseline distance should be computed as

linespace = ascent - descent + linegap 
Also called external-leading")
		  (bounding-box . "This is an imaginary box that encloses all glyphs from the font, usually as tightly as possible. It is represented by four parameters, namely xMin, yMin, xMax, and yMax, that can be computed for any outline. Their values can be in font units if measured in the original outline, or in integer (or fractional) pixel units when measured on scaled outlines.")
		  (internal-leading . "This concept comes directly from the world of traditional typography. It represents the amount of space within the leading which is reserved for glyph features that lay outside of the EM square (like accentuation). It usually can be computed as

internal leading = ascent - descent - EM_size ")
                  (external-leading
                   .
                   "Measure of space between two lines of text. Same as line gap for the most part.")
                  ;;from glyph docs
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
                                        ;if we just export the same symbol we cant add different docs, if we make two separate symbols reify? has to be changed.
                  (cap-height
                   .
                   "The height to which flat topped Capital letters reach. Rounded characters usually overshoot this.")
                  (x-height
                   .
                   "The height to which flat topped lowercase letters reach. Rounded characters usually overshoot this.")
                  (name
                   .
                   "The name of the font, which may differ from the postscript name.")
                  (postscript-name
                   .
                   "The name by which the font is referenced by postscript programs.")
                  (family
                   .
                   "The collection of related typefaces the font belongs to.")
                  (subfamily
                   .
                   "It's place in the family.")
                  (fixed-width
                   .
                   "Returns the width of monospaced fonts or nil.")
                  (weight
                   .
                   "Returns the weight of a font as (number . keyword-descriptor) e.g. (550 . :medium)")
                  (aspect
                   .
                   "Calculates the ratio of width to height. Useful in the substitution of fonts.")
                  (metrics
                   .
                   "Returns a struct containing the shared metrics across fonts.")
                  (code-points
                   .
                   "Returns a list of all code points in font.")

                  (underline
                   .
                   "Returns a cons of (location . size) to be used with font.")
                  (baseline
                   .
                   "Returns the (x . y) position of the default baseline for normal font use.  This varies with scripts though western script is usually at y=0")
                  (baselines
                   .
                   "Returns the available baselines for the font.")
                  (glyph
                   .
                   "Given a character or code point returns the appropriate glyph.")
                  (glyph-count
                   .
                   "The number of glyphs in the font.")
                  (glyphs
                   .
                   "An outpouring of all the glyphs in a font.")
                  (tables
                   .
                   "A list of all tables in a font.")
                  (raw-table
                   .
                   "Returns the unprocessed data of a table.")
                  (table
                   .
                   "Returns the wanted table.")
                  (index
                   .
                   "Given a character or a code point returns the index used to identify said object.")
                  (index-table
                   .
                   "Returns whatever lies at the given index in the table.")
                  (realize
                   .
                   "Takes a font, usually vector font, and returns a concrete font at a given point size.")
                  (with-attributes
                   .
                   "Specify a list of generic font functions to use and they become symbol macros within the macro. Probably not very useful.")))
