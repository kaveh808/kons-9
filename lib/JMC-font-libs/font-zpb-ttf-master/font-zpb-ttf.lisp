(in-package :font-zpb-ttf)
;;;; zpb-ttf

(defmethod font:%open (font (type (eql :ttf)))
  (zpb-ttf:open-font-loader font))
(defmethod font:open ((font zpb-ttf::font-loader))
  font);opening an open font loader doesn't work in zpb-ttf, issue already raised.
(defmethod font:close ((font zpb-ttf::font-loader))
  (zpb-ttf:close-font-loader font))

(defmethod font:family ((font zpb-ttf::font-loader))
  (zpb-ttf:family-name font))
(defmethod font:subfamily ((font zpb-ttf::font-loader))
  (zpb-ttf:subfamily-name font))
(defmethod font:name ((font zpb-ttf::font-loader))
  (zpb-ttf:full-name font))

(defmethod font:em ((font zpb-ttf::font-loader))
  (zpb-ttf:units/em font))
(defmethod font:ascent ((font zpb-ttf::font-loader) &optional ppem)
  (glyph:reify? (zpb-ttf:ascender font) (font:em font) ppem))
(defmethod font:descent ((font zpb-ttf::font-loader) &optional ppem)
  (glyph:reify? (zpb-ttf:descender font) (font:em font) ppem))
(defmethod font:bounding-box ((font zpb-ttf::font-loader) &optional ppem)
  (glyph:reify? (zpb-ttf:bounding-box font) (font:em font) ppem))
(defmethod font:line-gap ((font zpb-ttf::font-loader) &optional ppem)
  (glyph:reify?  (zpb-ttf:line-gap font) (font:em font) ppem))
(defmethod font:underline ((font zpb-ttf::font-loader) &optional ppem)
  (list (glyph:reify? (zpb-ttf:underline-position font) (font:em font) ppem)
        (glyph:reify? (zpb-ttf:underline-thickness font) (font:em font) ppem)))
(defmethod font:fixed-width ((font zpb-ttf::font-loader));specify to return size if fixed.
  (when (zpb-ttf:fixed-pitch-p font)
    (let ((bbox (zpb-ttf:bounding-box (zpb-ttf:find-glyph #\a font))))
      (- (aref bbox 2) (aref bbox 0)))))

(defmethod font:glyph (char-or-code (font zpb-ttf::font-loader))
  (zpb-ttf:find-glyph char-or-code font))

(defmethod font:kerning ((font zpb-ttf::font-loader) left right &optional ppem)
  (glyph:reify? (zpb-ttf:kerning-offset left right font) (font:em font) ppem))

(defmethod font:index ((glyph zpb-ttf::glyph) (font zpb-ttf::font-loader))
  (zpb-ttf::font-index glyph))
(defmethod font:tables ((font zpb-ttf::font-loader))
  (let ((result ()))
    (maphash (lambda (key value)
               (declare (ignore key))
               (push (zpb-ttf::name value) result))
             (zpb-ttf::tables font))
    (nreverse result)))
(defmethod font:raw-table ((table string) (font zpb-ttf::font-loader))
  (let* ((info (zpb-ttf::table-info table font))
         (result (make-array (zpb-ttf::size info) :element-type '(unsigned-byte 8))))
    (zpb-ttf::seek-to-table info font)
    (read-sequence result (zpb-ttf::input-stream font))
    result))
(defmethod font:code-points ((font zpb-ttf::font-loader))
  (remove nil (map 'list (lambda (i) (unless (minusp i)i)) (zpb-ttf::invert-character-map font))))
(defmethod font:glyphs ((font zpb-ttf::font-loader))
  ;;(zpb-ttf::glyph-cache font) ;easiest would be to prefill the cache but...
  (mapcar (lambda (i) (font:glyph i font)) (font:code-points font))) ;should this return vector? 
(defmethod font:glyph-count ((font zpb-ttf::font-loader))
  (zpb-ttf:glyph-count font))

