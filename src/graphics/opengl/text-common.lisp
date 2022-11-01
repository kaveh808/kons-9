(in-package :kons-9)

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (ql:quickload :font)
;;   (ql:quickload :cl-paths-ttf)
;;   (ql:quickload :font-zpb-ttf)
;;   )

(defconstant +index-type+ :unsigned-short)

#+NOTYET(declaim (inline clampf))
(defun clampf (number)
  "Clamp real number to single-float limits."
  (declare (type real number))
  (block nil
    (when (typep number 'single-float)
      (return number))
    (when (< (cl:the double-float #.(/ least-negative-single-float 2.0d0))
	     number
	     (cl:the double-float #.(/ least-positive-single-float 2.0d0)))
      (return 0.0f0))
    (when (minusp number)
      (when (> number (cl:the single-float least-negative-single-float))
        (return least-negative-single-float))
      (when (< number (cl:the single-float most-negative-single-float))
        (return most-negative-single-float))
      (return (coerce number 'single-float)))
    (when (< number (cl:the single-float least-positive-single-float))
      (return least-positive-single-float))
    (when (> number (cl:the single-float most-positive-single-float))
      (return most-positive-single-float))
    (coerce number 'single-float)))

(defvar *font-hash-table* (make-hash-table :test #'equalp))

(defvar *current-font*)

(defvar *draw-list*)

(cffi:defcstruct standard-draw-list-vertex
  (pos :float :count 2)
  (uv :float :count 2)
  (red :unsigned-char)
  (green :unsigned-char)
  (blue :unsigned-char)
  (alpha :unsigned-char))

(defstruct draw-cmd
  (elem-count)
  (clip-rect-x)
  (clip-rect-y)
  (clip-rect-width)
  (clip-rect-height)
  (texture-id)
  (vtx-offset 0)
  (idx-offset 0))

(defstruct draw-list
  (cmd-buffer)
  (vtx-current-idx)
  (idx-buffer)
  (idx-buffer-cap)
  (idx-buffer-size)
  (vtx-buffer)
  (vtx-buffer-cap)
  (vtx-buffer-size))

(defun initialize-draw-list (draw-list)
  (declare (type draw-list draw-list))
  (setf (draw-list-cmd-buffer draw-list) (make-array 500 :adjustable t :fill-pointer 0))
  (setf (draw-list-idx-buffer draw-list) (cffi:foreign-alloc +index-type+ :count 512))
  (setf (draw-list-idx-buffer-cap draw-list) 512)
  (setf (draw-list-idx-buffer-size draw-list) 0)
  (setf (draw-list-vtx-buffer draw-list) (cffi:foreign-alloc '(:struct standard-draw-list-vertex) :count 512))
  (setf (draw-list-vtx-buffer-cap draw-list) 512)
  (setf (draw-list-vtx-current-idx draw-list) 0)
  (setf (draw-list-vtx-buffer-size draw-list) 0)
  draw-list)

(defun reinitialize-draw-list (draw-list)
  (setf (fill-pointer (draw-list-cmd-buffer draw-list)) 0)
  (setf (draw-list-vtx-current-idx draw-list) 0)
  (setf (draw-list-idx-buffer-size draw-list) 0)
  (setf (draw-list-vtx-buffer-size draw-list) 0)
  draw-list)

(defun add-draw-cmd (draw-list elem-count texture-id clip-rect-x clip-rect-y clip-rect-width clip-rect-height)
  (let ((new-draw-cmd (make-draw-cmd :clip-rect-x clip-rect-x
				     :clip-rect-y clip-rect-y
				     :clip-rect-width clip-rect-width
				     :clip-rect-height clip-rect-height
				     :texture-id texture-id
				     :elem-count elem-count
				     :idx-offset (truncate (draw-list-idx-buffer-size draw-list)
							   (load-time-value
							    (cffi:foreign-type-size +index-type+))))))
    
    (vector-push-extend new-draw-cmd (draw-list-cmd-buffer draw-list))
    new-draw-cmd))

(defstruct font
  (name)
  (size)
  (ascender)
  (descender)
  (text-height)
  (glyphs (make-hash-table)))

(defun get-font-glyph (font char)
  (gethash char (font-glyphs font)))

(defstruct glyph-sprite
  (char)
  (left)
  (top)
  (advance)
  (height)
  (width)
  (texture)
  (u0 0)
  (v0 0)
  (u1 1)
  (v1 1)
  (hori-bearing-x)
  (hori-bearing-y)
  (hori-advance))

(defun make-list-from-to (from to)
  (loop for i from from to to
     collect i))

(defun max-width-height (font size)
  (let ((glyphs (font:glyphs font))
	(max-width 0)
	(max-height 0))
    (loop for g in glyphs
       do (let ((bbox (glyph:bounding-box g size)))
	    (setq max-width (max (- (aref bbox 2) (aref bbox 0)) max-width)
		  max-height (max (- (aref bbox 3) (aref bbox 1)) max-height)))
       finally (return (values (ceiling max-width) (ceiling max-height))))))

(defun max-width-height2 (font size)
  ;; this function is necessary because the glyph metrics do not agree with the bitmap metrics
  (let ((glyphs (font:glyphs font))
	(max-width 0)
	(max-height 0))
    (loop for g in glyphs
       do (multiple-value-bind (raster size offset) (glyph:raster g size)
	    (declare (ignore raster offset))
	    (setq max-width (max (car size) max-width)
		  max-height (max (cdr size) max-height)))
       finally (return (values (ceiling max-width) (ceiling max-height))))))

(defun pack-font (&optional (font-pathname (default-font *drawing-settings*)) (ppem 12))
  (let* ((font-loader (zpb-ttf::open-font-loader-from-file font-pathname)))
    (multiple-value-bind (cell-width cell-height) (max-width-height2 font-loader ppem)
	(let ((sprites ()))
	  (let* ((count (- #x7e #x20) #+NOTYET (zpb-ttf:glyph-count font-loader))
		 (edge-size (ceiling (sqrt count)))
		 (glyphs (mapcar #'(lambda (code)
				     (zpb-ttf:find-glyph (code-char code) font-loader))
				 (make-list-from-to #x20 #x7e))
		   ;; all glyphs are a little expensive right now, so we're limiting to ascii characters
		   #+NOTYET(font:glyphs font-loader))
		 (buffer (make-array (* edge-size edge-size cell-width cell-height)
				     :element-type '(unsigned-byte 8)
				     :initial-element #x00))
		 (bitmap (make-array (list edge-size cell-height
					   edge-size cell-width)
				     :displaced-to buffer
				     :element-type '(unsigned-byte 8)))
		 (bitmap-width (* edge-size cell-width))
		 (bitmap-height (* edge-size cell-height)))

	  (block pack
	    (loop for row from (1- edge-size) downto 0
	       do (loop for col from 0 below edge-size
		     do (let ((glyph (pop glyphs)))
			  (unless glyph (return-from pack))
			  (let* ((bbox (glyph:bounding-box glyph ppem))
				 (advance (glyph:advance-width glyph ppem))
				 (y-max (floor (glyph:y-max bbox))))
			    (multiple-value-bind (raster size offset) (glyph:raster glyph ppem)
			      (loop for j from 0 below (cdr size)
				   do (loop for i from 0 below (car size)
					 do (setf (aref bitmap row j col i) (aref raster (+ (* j (car size)) i)))))
			      (let* ((x (* col cell-width))
				     (y (* row cell-height))
				     (u0 (/ x bitmap-width))
				     (v0 (/ (+ y (cdr size)) bitmap-height))
				     (u1 (/ (+ x (car size)) bitmap-width))
				     (v1 (/ y bitmap-height)))

				(push (make-glyph-sprite
				       :char (code-char (glyph:code-point glyph))
				       :advance advance
				       :left (car offset)
				       :top y-max
				       :width (car size)
				       :height (cdr size)
				       :u0 (clampf u0)
				       :v0 (clampf v0)
				       :u1 (clampf u1)
				       :v1 (clampf v1))
				      sprites))))))))
	  (values buffer edge-size cell-width cell-height (nreverse sprites)))))))



(cffi:defcfun ("memcpy" memcpy) :void (dest :pointer) (src :pointer) (size :uint64))

#+NOTYET(declaim (inline copy-pointer))
(defun copy-pointer (pointer)
  (sb-sys:int-sap (sb-sys:sap-int pointer)))

(defun maybe-resize-standard-vertex-buffer (buffer buffer-size current-capacity proposed-index)
  (if (< proposed-index (1- current-capacity))
      (values buffer current-capacity nil)
      (let* ((new-capacity (* 2 current-capacity))
	     (new-buffer (cffi:foreign-alloc '(:struct standard-draw-list-vertex) :count new-capacity)))
	(memcpy new-buffer buffer buffer-size)
	(values new-buffer new-capacity t))))

(defun maybe-resize-index-buffer (buffer buffer-size current-capacity proposed-index)
  (if (< proposed-index (1- current-capacity))
      (values buffer current-capacity nil)
      (let* ((new-capacity (* 2 current-capacity))
	     (new-buffer (cffi:foreign-alloc +index-type+ :count new-capacity)))
	(memcpy new-buffer buffer buffer-size)
	(values new-buffer new-capacity t))))

(defun prim-reserve (draw-list index-count vertex-count)
  (when (and (zerop index-count)
	     (zerop vertex-count))
    (return-from prim-reserve nil))
  (unless (zerop vertex-count)
    (let ((proposed-vtx-index (+ (draw-list-vtx-current-idx draw-list) vertex-count))
	  (old-buffer (draw-list-vtx-buffer draw-list)))
      (multiple-value-bind (buffer capacity updated?)
	  (maybe-resize-standard-vertex-buffer (draw-list-vtx-buffer draw-list)
					       (draw-list-vtx-buffer-size draw-list)
					       (draw-list-vtx-buffer-cap draw-list)
					       proposed-vtx-index)
	(setf (draw-list-vtx-buffer draw-list) buffer
	      (draw-list-vtx-buffer-cap draw-list) capacity)
	(when updated?
	  (cffi:foreign-free old-buffer)))))

  (unless (zerop index-count)
    (let ((proposed-idx-index (+ (truncate (draw-list-idx-buffer-size draw-list)
					   (load-time-value (cffi:foreign-type-size +index-type+)))
				 index-count))
	  (old-buffer (draw-list-idx-buffer draw-list)))
      (multiple-value-bind (buffer capacity updated?)
	  (maybe-resize-index-buffer (draw-list-idx-buffer draw-list)
				     (draw-list-idx-buffer-size draw-list)
				     (draw-list-idx-buffer-cap draw-list)
				     proposed-idx-index)
	(setf (draw-list-idx-buffer draw-list) buffer
	      (draw-list-idx-buffer-cap draw-list) capacity)
	(when updated?
	  (cffi:foreign-free old-buffer)))))
  t)

(defun prim-rect-uv (draw-list a-x a-y c-x c-y uv-a-x uv-a-y uv-c-x uv-c-y col)
  (declare (type single-float a-x a-y c-x c-y uv-a-x uv-a-y uv-c-x uv-c-y))
  (declare (type (integer 0 #xffffffff) col))
  (let* ((b-x c-x)
	 (b-y a-y)
	 (d-x a-x)
	 (d-y c-y)
	 (uv-b-x uv-c-x)
	 (uv-b-y uv-a-y)
	 (uv-d-x uv-a-x)
	 (uv-d-y uv-c-y))
    (with-slots (vtx-current-idx idx-buffer vtx-buffer idx-buffer-size vtx-buffer-size) draw-list
      (let* ((vtx-ptr (copy-pointer vtx-buffer))
	     (vtx-write-ptr (cffi:incf-pointer vtx-ptr vtx-buffer-size))
	     (vtx-write-ptr-pos)
	     (vtx-write-ptr-uv)
	     (vtx-write-ptr-col)
	     (idx vtx-current-idx)
	     (idx-ptr (copy-pointer idx-buffer))
	     (idx-write-ptr (cffi:incf-pointer idx-ptr idx-buffer-size)))

	(flet ((set-vtx-write-ptrs (index)
		 (setq vtx-write-ptr-pos
		       (cffi:mem-aptr vtx-write-ptr '(:struct standard-draw-list-vertex) index))
		 (setq vtx-write-ptr-uv
		       (cffi:foreign-slot-pointer vtx-write-ptr-pos '(:struct standard-draw-list-vertex) 'uv))
		 (setq vtx-write-ptr-col
		       (cffi:foreign-slot-pointer vtx-write-ptr-pos '(:struct standard-draw-list-vertex) 'red)))
	   
	       (write-color ()
		 (setf (cffi:mem-aref vtx-write-ptr-col :unsigned-char 0) (logand #x000000ff (ash col -24))
		       (cffi:mem-aref vtx-write-ptr-col :unsigned-char 1) (logand #x000000ff (ash col -16))
		       (cffi:mem-aref vtx-write-ptr-col :unsigned-char 2) (logand #x000000ff (ash col -8))
		       (cffi:mem-aref vtx-write-ptr-col :unsigned-char 3) (logand #x000000ff (ash col 0)))))
	     
		   
	  ;; set up indexes
	  (setf (cffi:mem-aref idx-write-ptr +index-type+ 0) idx)
	  (setf (cffi:mem-aref idx-write-ptr +index-type+ 1) (1+ idx))
	  (setf (cffi:mem-aref idx-write-ptr +index-type+ 2) (+ idx 2))

	  (setf (cffi:mem-aref idx-write-ptr +index-type+ 3) idx)
	  (setf (cffi:mem-aref idx-write-ptr +index-type+ 4) (+ idx 2))
	  (setf (cffi:mem-aref idx-write-ptr +index-type+ 5) (+ idx 3))

	  ;; set up vertexes
	  ;; first vertex
	  (set-vtx-write-ptrs 0)

	  (setf (cffi:mem-aref vtx-write-ptr-pos :float 0) a-x
		(cffi:mem-aref vtx-write-ptr-pos :float 1) a-y)

	  (setf (cffi:mem-aref vtx-write-ptr-uv :float 0) uv-a-x
		(cffi:mem-aref vtx-write-ptr-uv :float 1) uv-a-y)

	  (write-color)      

	  ;; second vertex
	  (set-vtx-write-ptrs 1)
        
	  (setf (cffi:mem-aref vtx-write-ptr-pos :float 0) b-x
		(cffi:mem-aref vtx-write-ptr-pos :float 1) b-y)

	  (setf (cffi:mem-aref vtx-write-ptr-uv :float 0) uv-b-x
		(cffi:mem-aref vtx-write-ptr-uv :float 1) uv-b-y)

	  (write-color)
      
	  ;; third vertex
	  (set-vtx-write-ptrs 2)
        
	  (setf (cffi:mem-aref vtx-write-ptr-pos :float 0) c-x
		(cffi:mem-aref vtx-write-ptr-pos :float 1) c-y)

	  (setf (cffi:mem-aref vtx-write-ptr-uv :float 0) uv-c-x
		(cffi:mem-aref vtx-write-ptr-uv :float 1) uv-c-y)

	  (write-color)

	  ;; fourth vertex
	  (set-vtx-write-ptrs 3)
        
	  (setf (cffi:mem-aref vtx-write-ptr-pos :float 0) d-x
		(cffi:mem-aref vtx-write-ptr-pos :float 1) d-y)

	  (setf (cffi:mem-aref vtx-write-ptr-uv :float 0) uv-d-x
		(cffi:mem-aref vtx-write-ptr-uv :float 1) uv-d-y)

	  (write-color)

	  ;; finally, update counts
	  (incf idx-buffer-size (* 6 #.(cffi:foreign-type-size +index-type+)))
	  (incf vtx-buffer-size (* 4 #.(cffi:foreign-type-size '(:struct standard-draw-list-vertex))))
	  (incf vtx-current-idx 4)      

	  (values))))))

(defun font-render-char (font draw-list pos-x pos-y col char)
  (with-slots (ascender descender text-height size) font
    (let ((glyph (get-font-glyph font char)))
      (when (null glyph) (return-from font-render-char (values nil 0.0)))
      (with-slots (texture left top width height advance u0 v0 u1 v1) glyph
	(when (prim-reserve draw-list 6 4)
	  (let* ((x (clampf (+ pos-x left)))
		 (y (clampf (- pos-y top)))
		 (width (clampf (+ x width)))
		 (height (clampf (+ y height)))
		 (u0 (clampf u0))
		 (v0 (clampf v0))
		 (u1 (clampf u1))
		 (v1 (clampf v1)))

	    (add-draw-cmd draw-list 6 texture x y width height)
	  
	    (prim-rect-uv draw-list
			  x y
			  width height
			  u0 v0
			  u1 v1
			  col)
	    (values t advance)))))))

(defun font-render-text (font draw-list pos-x pos-y col text)
  (loop for char across text
     with advance = 0.0
     with offset-x = 0.0
     with success? = nil
     do (multiple-value-setq (success? advance)
	  (font-render-char font draw-list (+ pos-x offset-x) pos-y col char))
       (unless success?
	 (multiple-value-setq (success? advance)
	   (font-render-char font draw-list (+ pos-x offset-x) pos-y col #\?)))
       (incf offset-x advance)))

(defun text-engine-begin-frame ()
  (reinitialize-draw-list *draw-list*)
  (values))

(defun render-text (pos-x pos-y text &key (color #x000000ff))
  (when (not (ui-is-clipped? pos-x pos-y
                             (+ pos-x (ui-text-width text)) (+ pos-y *ui-font-height*)))
    (font-render-text *current-font* *draw-list* pos-x pos-y color text)))

(defun test-text ()
  (progn
  (render-text 100 100 "The quick brown fox jumps over the lazy dog." :color #x0000ffff)
  (render-text 125 125 "The quick brown fox jumps over the lazy dog." :color #xff0000ff)
  (render-text 150 150 "The quick brown fox jumps over the lazy dog." :color #x00ff00ff)))

(defun text-engine-end-frame ()
  (render-draw-lists (list *draw-list*) 0 0 (elt *window-size* 0) (elt *window-size* 1))
  (values))
	       
(defun run-1 ()
  (sb-int:set-floating-point-modes :traps nil)
  (kons-9::show-window kons-9::*scene*))
