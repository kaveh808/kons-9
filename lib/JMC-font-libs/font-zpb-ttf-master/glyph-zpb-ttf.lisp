;;;zpb-ttf
(in-package :font-zpb-ttf)

(defmethod glyph:index ((glyph zpb-ttf::glyph))
  (zpb-ttf:font-index glyph))
(defmethod glyph:name ((glyph zpb-ttf::glyph))
  (aref (zpb-ttf::postscript-glyph-names (glyph:font glyph)) (glyph:index glyph)))
(defmethod glyph:character ((glyph zpb-ttf::glyph))
  (code-char (glyph:code-point glyph)))
(defmethod glyph:code-point ((glyph zpb-ttf::glyph))
  (zpb-ttf:code-point glyph))

;(defmethod glyph:character ((glyph zpb-ttf::glyph)))


(defmethod glyph:bounding-box ((glyph zpb-ttf::glyph) &optional ppem)
  (let ((bbox (zpb-ttf:bounding-box glyph)))
    (if (null ppem)
        bbox
        (let ((em (glyph:em glyph)))
          (map 'vector (lambda (x) (* x (/ ppem em))) bbox)))))
(defmethod glyph:x-min ((glyph zpb-ttf::glyph)&optional ppem)
  (glyph:reify? (aref (zpb-ttf:bounding-box glyph) 0) glyph ppem))
(defmethod glyph:y-min ((glyph zpb-ttf::glyph)&optional ppem)
  (glyph:reify? (aref (zpb-ttf:bounding-box glyph) 1) glyph ppem))
(defmethod glyph:x-max ((glyph zpb-ttf::glyph)&optional ppem)
  (glyph:reify? (aref (zpb-ttf:bounding-box glyph)2) glyph ppem))
(defmethod glyph:y-max ((glyph zpb-ttf::glyph)&optional ppem)
  (glyph:reify? (aref (zpb-ttf:bounding-box glyph)3) glyph ppem))

(defmethod glyph:advance-width  ((glyph zpb-ttf::glyph)&optional ppem)
  (glyph:reify? (zpb-ttf:advance-width glyph) glyph ppem))
(defmethod glyph:advance-height ((glyph zpb-ttf::glyph)&optional ppem)
  (declare (ignore ppem))
 ; (zpb-ttf:advance-height glyph) why isn't this in zpb? get someone to add it
  0)

(defmethod glyph:right-side-bearing ((glyph zpb-ttf::glyph)&optional ppem)
  (glyph:reify? (zpb-ttf:right-side-bearing glyph) glyph ppem))
(defmethod glyph:left-side-bearing ((glyph zpb-ttf::glyph)&optional ppem)
  (glyph:reify? (zpb-ttf:left-side-bearing glyph) glyph ppem))
;;same with top-side bottom-side, someone needs to decode the vertical metrics

(defmethod glyph:kerning ((glyph1 zpb-ttf::glyph) glyph2 &optional ppem)
  (let ((font (glyph:font glyph1)))
    (glyph:reify? (zpb-ttf:kerning-offset glyph1 (font:glyph glyph2 font) font) glyph1 ppem)))
(defmethod glyph:kerning ((glyph1 zpb-ttf::glyph)(glyph2 zpb-ttf::glyph) &optional ppem)
  (let ((font (glyph:font glyph1)))
    (glyph:reify? (zpb-ttf:kerning-offset glyph1 glyph2 font) glyph1 ppem)))

(defmethod glyph:data ((glyph zpb-ttf::glyph))
  (zpb-ttf:contours glyph))

(defun paths (glyph &key (offset (cons 0 0)) (scale-x 1) (scale-y scale-x) (auto-orient :cw))
  (net.tuxee.paths-ttf:paths-from-glyph glyph :offset offset :scale-x scale-x :scale-y scale-y :auto-orient auto-orient))

;;cripple to only square pixels, how will we deal with 50 year old technology?
(defmethod glyph:paths ((glyph zpb-ttf::glyph) &key ppem (offset (cons 0 0)) inverted)
  (let ((scale-x (if (null ppem) 1.0 (/ ppem (glyph:em glyph)))))
    (paths glyph :offset offset :scale-x scale-x :scale-y (if inverted (- scale-x)scale-x))))

(defmethod glyph:font ((glyph zpb-ttf::glyph))
  (zpb-ttf::font-loader glyph))



;;;; This should probably be in FONTS
(defmethod glyph:raster ((glyph zpb-ttf::glyph)&optional ppem )
  (let* ((bbox (glyph:bounding-box glyph ppem))
         (x-min (floor (glyph:x-min bbox)))
         (width (- (ceiling (glyph:x-max bbox))  x-min))
         (y-min (floor (glyph:y-min bbox)))
         (height  (-  (ceiling (glyph:y-max bbox)) y-min))
         (paths (paths:path-translate (glyph:paths glyph :ppem ppem) (cons (- x-min) (- y-min))))
         (result (make-array (* height width) :element-type '(unsigned-byte 8)))
         (state (aa:make-state)))
    (vectors:update-state state paths)
    (aa:cells-sweep state (lambda (x y a) (when (and (<= 0 x width)
                                                     (<= 0 y height))
                                            (setf (aref result (+ x (* y width)))
                                                  (let ((value (mod a 512)))
                                                    (min 255 (if (< value 256) value (- 512 value))))))))
    (values result
            (cons width height)
            (cons x-min y-min))))

;; (defun gl-test (char font &optional (location (cons 0 0)) (ppem 160))
;;   (let ((font (font:open font)))
;;     (gl:pixel-store :unpack-alignment 1 )
;;     (gl:window-pos (car location ) (cdr location))
;;     (multiple-value-bind (data size)(glyph:raster (font:glyph char font) ppem)
;;       (gl:draw-pixels (car size) (cdr size ) :luminance :unsigned-byte data))))

;; (defun test-render (&optional (ppem 100) (font (fonts:find-match "deja")))
;;   (declare (optimize debug))
;;   (let ((font (font:open font)))
;;     (loop  :for string :in (list "abcdefgh"
;;                                  "ijklmnop"
;;                                  "qrstuvwx"
;;                                  "yzABCDEF"
;;                                  "GHIJKLMN"
;;                                  "OPQRSTUV"
;;                                  "WXYZ1234")
;;            :for y :from 0 :by ppem
;;            :do (loop :for char :across string
;;                      :for x :from 0 :by ppem
;;                      :do (font-zpb-ttf::gl-test char font (cons x y) ppem)))))

;; (defun get-all-rasters (&optional (ppem 100) (font (fonts:find-match "deja")))
;;   (let ((font (font:open font )))
;;     (loop :for char :across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890~`\|[]{}()!@#$%^&*_+-=;':<>?,./\""
;;           :collect (glyph:raster (font:glyph char font) ppem))))



