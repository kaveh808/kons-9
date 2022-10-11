;;;; Font specific
(defmethod font:open (font))
(defmethod font:%open (font type))
(defmethod font:close (font))

(defmethod font:name (font))
(defmethod font:family (font))
(defmethod font:subfamily (font))
(defmethod font:weight (font))

(defmethod font:ascent (font &optional ppem))
(defmethod font:descent (font &optional ppem))
(defmethod font:internal-leading (font &optional ppem))
(defmethod font:external-leading (font &optional ppem))
(defmethod font:line-gap (font &optional ppem))

(defmethod font:height (font &optional ppem))
(defmethod font:cap-height (font &optional ppem))
(defmethod font:x-height (font &optional ppem))

(defmethod font:baseline (font &optional ppem))
(defmethod font:baselines (font &optional ppem))
(defmethod font:center-baseline (font &optional ppem))
(defmethod font:top-baseline (font &optional ppem))

(defmethod font:underline (font &optional ppem))
(defmethod font:fixed-width (font))

(defmethod font:kerning (font char1 char2 &optional ppem))
(defmethod font:kerning-pairs (font char &optional ppem))

(defmethod font:glyph-count (font))
(defmethod font:glyphs (font))
(defmethod font:glyph (char font))
(defmethod font:index (thing font))

(defmethod font:code-points (font))
(defmethod font:character-sets (font))

(defmethod font:paths (object font &key offset &allow-other-keys))
(defmethod font:tables (font))
(defmethod font:raw-table (table font))
(defmethod font:table (string font))
(defmethod font:index-table (index table font))
(defmethod font:aspect (font))

(defmethod font:internal-leading (font &optional ppem))
(defmethod font:external-leading (font &optional ppem))
(defmethod font:metrics (font))


;;;; Glyph specific

(defmethod font:index (glyph))
(defmethod font:character (glyph))
(defmethod font:code-point (glyph))
(defmethod font:name (glyph))
(defmethod font:font (glyph))
(defmethod font:em (glyph))


(defmethod font:bounding-box (glyph &optional ppem))
(defmethod font:x-min (glyph &optional ppem))
(defmethod font:x-max (glyph &optional ppem))
(defmethod font:y-min (glyph &optional ppem))
(defmethod font:y-max (glyph &optional ppem))
(defmethod font:vertical-origin (glyph &optional ppem))
(defmethod font:horizontal-origin (glyph &optional ppem))

(defmethod font:width (glyph &optional ppem))
(defmethod font:height (glyph &optional ppem))
(defmethod font:size (glyph  &optional ppem))


(defmethod font:left-side-bearing (glyph &optional ppem)) 
(defmethod font:right-side-bearing (glyph &optional ppem))
(defmethod font:top-side-bearing (glyph &optional ppem))
(defmethod font:bottom-side-bearing (glyph &optional ppem))       

(defmethod font:advance-width (glyph &optional ppem))
(defmethod font:advance-height (glyph &optional ppem))

(defmethod font:kerning (glyph1 glyph2 &optional ppem))
(defmethod font:kerning-pairs (glyph))
(defmethod font:data (glyph));raw
(defmethod font:paths (glyph &key offset &allow-other-keys)) ;cl-vector paths
(defmethod font:raster (glyph &optional ppem))
(defmethod font:anchors (glyph  &optional ppem))
