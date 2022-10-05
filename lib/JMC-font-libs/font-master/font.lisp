(defpackage #:font
  (:use :cl)
  (:shadowing-import-from glyph reify? bounding-box x-min x-max y-min y-max em)
  (:shadow cl:open cl:close)
  (:export #:name
           #:postscript-name
           #:subfamily
           #:family
           #:fixed-width
           #:weight
           #:aspect
           #:metrics
           #:code-points
           
           #:em
           #:ascent
           #:descent
           #:line-gap
           #:internal-leading
           #:external-leading
           #:cap-height
           #:x-height
           
           #:bounding-box
           #:x-min
           #:x-max
           #:y-min
           #:y-max
           #:height
           
           #:underline
           #:baseline
           #:baselines
           
           #:open
           #:%open
           #:close
          
           #:glyph-count
           #:glyph
           #:glyphs
           #:kerning
           #:kerning-pairs
            
           #:tables
           #:raw-table
           #:table
           #:index
           #:index-table
                      
           #:paths
           #:extents
           #:character-sets))

(in-package #:font)

(defmacro with (font &body body)
  `(let ((font (open ,font)))
     (unwind-protect
          (progn
            ,@body)
       (close font))))

(defgeneric open (font)
  (:method ((pathname pathname))(%open pathname (intern (string-upcase (pathname-type pathname)) :keyword)))
  (:method ((string string))(%open string (intern (string-upcase (pathname-type string)) :keyword))))
(defgeneric %open (font type)
  (:method (font type) (error "No implementation for fonts of type ~a" type)))
(defgeneric close (font)
  (:method (font) "Font isn't really closed, but was it really ever open in the first place?"))

(defgeneric name (font))
(defgeneric family (font))
(defgeneric subfamily (font)) ;should this be sub-family contrary to the world?
(defgeneric weight (font))

(defgeneric ascent (font &optional ppem))
(defgeneric descent (font &optional ppem))
(defgeneric internal-leading (font &optional ppem))
(defgeneric external-leading (font &optional ppem))
(defgeneric line-gap (font &optional ppem))

(defgeneric height (font &optional ppem)
  (:method ((font t) &optional ppem)
    (with font
          (+ (ascent font ppem) (abs (descent font ppem)))))) ;should descent mandate positive?
(defgeneric cap-height (font &optional ppem)
  (:method (font &optional ppem) (glyph:height (glyph #\M font) ppem)))
(defgeneric x-height (font &optional ppem)
  (:method (font &optional ppem) (glyph:height (glyph #\x font) ppem)))

(defgeneric baseline (font &optional ppem))
(defgeneric baselines (font &optional ppem))
(defgeneric center-baseline (font &optional ppem))
(defgeneric top-baseline (font &optional ppem))

(defgeneric underline (font &optional ppem))
(defgeneric fixed-width (font))

(defgeneric kerning (font char1 char2 &optional ppem))
(defgeneric kerning-pairs (font char &optional ppem))


(defgeneric glyph-count (font))
(defgeneric glyphs (font))
(defgeneric glyph (char font))
(defgeneric index (thing font)
  (:method ((thing character) font) (glyph:index (font:glyph thing font))))

(defgeneric code-points (font))
(defgeneric character-sets (font));better name? scripts? script languages?

(defgeneric paths (object font &key offset &allow-other-keys))
(defgeneric tables (font))
(defgeneric raw-table (table font))
(defgeneric table (string font))
(defgeneric index-table (index table font))
(defgeneric metrics (font))
(defgeneric realize (font size dpi-x &optional dpi-y))


;;;; Methods

(defgeneric aspect (font)
  (:method (font)
    (with font
      (/ (x-height font) (cap-height font)))))

(defmethod internal-leading (font &optional ppem)
  (with font
    (reify? (- (em font) (+ (ascent font) (abs (descent font)))) font ppem))) ;this doesn't work properly because history and people working around program limitations.
(defmethod external-leading (font &optional ppem)
  (reify? (line-gap font) font ppem))
(defmethod glyph:em (glyph)
  (em (glyph:font glyph)))
(defmethod paths ((char character) font &key (offset (cons 0 0))ppem inverted)
  (glyph:paths (font:glyph char font):offset offset :ppem ppem :inverted inverted))

(defmethod paths ((string string) font &key (offset (cons 0 0)) (kerning t) ppem inverted)
  "basic because we have access to nothing here. Override from FONTS."
  (let* ((font (font:open font))
         (paths 
           (destructuring-bind (x . y) offset
             (loop :for char :across string
                   :for previous := nil :then glyph
                   :for glyph := (font:glyph char font)
                   :for cursor := x :then (+ cursor (glyph:advance-width previous ppem))
                   :for paths := (glyph:paths glyph :offset (cons cursor y) :ppem ppem :inverted inverted)
                   :when paths :collect it
                     :when (and kerning previous)
                       :do (incf cursor (glyph:kerning previous glyph ppem))))))
    paths))




;;;useless
;;style? weight + italic oblique condensed extended underline overstrike, outline shadow

(defmacro with-attributes (attributes font &body body)
  "e.g. (font:with-attributes (name family-name) font (list name family-name))"
  `(symbol-macrolet (,@ (loop :for at :in attributes
                              :collect `(,at (,(find-symbol (symbol-name at) (find-package 'font)) ,font))))
     ,@body))
