(in-package :kons-9)

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cl-freetype2))

(eval-when (:compile-toplevel :load-toplevel)
  (use-package :ft2))

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

(defparameter *framebuffer-scale* #+darwin 2 #-darwin 1)

(defconstant +index-type+ :unsigned-short)

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
  (vtx-offset :unused)
  (idx-offset :unused))

(defstruct draw-list
  (cmd-buffer)
  (idx-buffer)
  (idx-buffer-cap)
  (vtx-buffer)
  (vtx-buffer-cap)
  (vtx-current-idx)
  (vtx-write-ptr)
  (idx-write-ptr)
  (clip-rect-stack)
  (texture-id-stack))

#+NOTYET(declaim (inline copy-pointer))
(defun copy-pointer (pointer)
  (sb-sys:int-sap (sb-sys:sap-int pointer)))

(defun initialize-draw-list (draw-list)
  (declare (type draw-list draw-list))
  (setf (draw-list-cmd-buffer draw-list) (make-array 500 :adjustable t :fill-pointer 0))
  (setf (draw-list-idx-buffer draw-list) (cffi:foreign-alloc +index-type+ :count 512))
  (setf (draw-list-idx-buffer-cap draw-list) 512)
  (setf (draw-list-vtx-buffer draw-list) (cffi:foreign-alloc '(:struct standard-draw-list-vertex) :count 512))
  (setf (draw-list-vtx-buffer-cap draw-list) 512)
  (setf (draw-list-vtx-current-idx draw-list) 0)
  (setf (draw-list-vtx-write-ptr draw-list) (copy-pointer (draw-list-vtx-buffer draw-list)))
  (setf (draw-list-idx-write-ptr draw-list) (copy-pointer (draw-list-idx-buffer draw-list)))
  draw-list)

(defun reinitialize-draw-list (draw-list)
  (setf (fill-pointer (draw-list-cmd-buffer draw-list)) 0)
  (setf (draw-list-vtx-current-idx draw-list) 0)
  (setf (draw-list-vtx-write-ptr draw-list) (copy-pointer (draw-list-vtx-buffer draw-list)))
  (setf (draw-list-idx-write-ptr draw-list) (copy-pointer (draw-list-idx-buffer draw-list)))
  draw-list)

(defun add-draw-cmd (draw-list elem-count texture-id clip-rect-x clip-rect-y clip-rect-width clip-rect-height)
  (let ((new-draw-cmd (make-draw-cmd :clip-rect-x clip-rect-x
				     :clip-rect-y clip-rect-y
				     :clip-rect-width clip-rect-width
				     :clip-rect-height clip-rect-height
				     :texture-id texture-id
				     :elem-count elem-count)))
    
    (vector-push-extend new-draw-cmd (draw-list-cmd-buffer draw-list))
    new-draw-cmd))

(defvar *font-hash-table* (make-hash-table :test #'equalp))

(defvar *current-font*)

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
  

(%gl::defglfun ("glTexParameteri" glTexParameteri_foo) :void (texture :int) (pname :int) (param :int))
(%gl::defglfun ("glPixelStorei" glPixelStorei_foo) :void (arg1 :int) (arg2 :int))
(%gl::defglfun ("glTexImage2D" glTexImage2D_foo) :void (target :int) (level :int)
	      (internal-format :int) (width :int) (height :int)
	      (border :int) (format :int) (type :int)
	      (data :pointer))
(%gl::defglfun ("glGenTextures" glGenTextures_foo) :void (n :int) (ptextures :pointer))
(%gl::defglfun ("glBindTexture" glBindTexture_foo) :void (target :int) (texture :int))
(%gl::defglfun ("glEnable" glEnable_foo) :void (target :int))
(%gl::defglfun ("glDrawElements" glDrawElements_foo) :void (mode :int) (count :int) (type :int) (indices :pointer))

(defconstant GL_LINE_STRIP #x3)
(defconstant GL_TRIANGLES #x4)
(defconstant GL_TEXTURE_WRAP_S #x2802)
(defconstant GL_TEXTURE_WRAP_T #x2803)
(defconstant GL_CLAMP #x2900)
(defconstant GL_CLAMP_TO_EDGE #x812f)
(defconstant GL_TEXTURE_MIN_FILTER #x2801)
(defconstant GL_TEXTURE_MAG_FILTER #x2800)
(defconstant GL_LINEAR #x2601)
(defconstant GL_UNPACK_ROW_LENGTH #xcf2)
(defconstant GL_UNPACK_ALIGNMENT #xcf5)
(defconstant GL_TEXTURE_2D #x0DE1)
(defconstant GL_RGB #x1907)
(defconstant GL_RGBA #x1908)
(defconstant GL_BGRA #x80E1)
(defconstant GL_FLOAT #x1406)
(defconstant GL_UNSIGNED_BYTE #x1401)
(defconstant GL_UNSIGNED_SHORT #x1403)
(defconstant GL_UNSIGNED_INT #x1405)
(defconstant GL_UNSIGNED_INT_8_8_8_8 #x8035)
(defconstant +index-type-enum+ GL_UNSIGNED_SHORT)
(defconstant GL_BLEND #x0be2)
(defconstant GL_SCISSOR_TEST #x0c11)
(defconstant GL_VERTEX_ARRAY #x8074)

(defmacro with-texture-from-bitmap ((buffer-var rows-var cols-var pitch-var bitmap) &body body)
  (let ((bitmap-sym (gensym)))
    `(let* ((,bitmap-sym ,bitmap)
	    (,rows-var (freetype2-types:ft-bitmap-rows ,bitmap-sym))
	    (,cols-var (freetype2-types:ft-bitmap-width ,bitmap-sym))
	    (,pitch-var (freetype2-types:ft-bitmap-pitch ,bitmap-sym))
	    (buffer-size (* ,cols-var ,rows-var (load-time-value (cffi:foreign-type-size :unsigned-int)))))

       (unless (and (eq (ft2-types:ft-bitmap-pixel-mode ,bitmap-sym) :gray)
		    (eq (freetype2-types:ft-bitmap-num-grays ,bitmap-sym) 256))
	 (error "unsupported pixel type"))

       (if (zerop buffer-size)
	   (let ((,buffer-var (sb-sys::int-sap 0)))
	     ,@body)
	   (cffi:with-foreign-object (,buffer-var :unsigned-char buffer-size)
	     (let* ((src (freetype2-types:ft-bitmap-buffer ,bitmap-sym))
		    (dest -1))

	       #+NIL
	       (loop for i from 0 below buffer-size
		  do (setf (cffi:mem-aref ,buffer-var :unsigned-char i) #xff)) ;; make a straight white texture for now

	       (loop for j from 0 below ,rows-var
		  do 
		    (loop for i from 0 below ,cols-var
		       do (setf (cffi:mem-aref ,buffer-var :unsigned-char (incf dest)) #xff
				(cffi:mem-aref ,buffer-var :unsigned-char (incf dest)) #xff
				(cffi:mem-aref ,buffer-var :unsigned-char (incf dest)) #xff
				(cffi:mem-aref ,buffer-var :unsigned-char (incf dest)) (cffi:mem-aref src :unsigned-char i)))
		    (cffi:incf-pointer src ,pitch-var)))
	   
	     ,@body)))))

(defun make-list-from-to (from to)
  (loop for i from from to to
     collect i))

(defun ensure-font (&optional (pathname "/System/Library/Fonts/Monaco.ttf") (size 12))
  (print (gl:gl-version))
  (with-open-face (face pathname)
    (ft2:set-pixel-sizes face 0 (* *framebuffer-scale* size))
    (let* ((name (if (pathname-type pathname)
		     (concatenate 'string (pathname-name pathname) "." (pathname-type pathname))
		     (pathname-name pathname)))
	   (ascender (ft2:face-ascender-pixels face))
	   (descender (ft2:face-descender-pixels face))
	   (text-height (ft2-types:ft-face-height face))
	   (font (make-font :name name :size size :ascender ascender :descender descender :text-height text-height)))
      (setf (gethash (list name size) *font-hash-table*) font)
      (flet ((create-glyph-sprite (font char)
	       (let* ((texture)
		      (index (get-char-index face char)))
		 (load-glyph face index)
		 (let ((glyphslot (freetype2-types:ft-face-glyph face)))
		   (render-glyph glyphslot)
		   (let* ((bitmap (ft2-types:ft-glyphslot-bitmap glyphslot))
			  ;;(advance (ft2:get-advance face char))
			  (left (freetype2-types:ft-glyphslot-bitmap-left glyphslot))
			  (top (freetype2-types:ft-glyphslot-bitmap-top glyphslot))
			  (metrics (ft2-types:ft-glyphslot-metrics glyphslot))
			  (hori-advance (ash (ft2-types:ft-glyph-metrics-hori-advance metrics) -6))
			  (hori-bearing-x (ash (ft2-types:ft-glyph-metrics-hori-bearing-x metrics) -6))
			  (hori-bearing-y (ash (ft2-types:ft-glyph-metrics-hori-bearing-y metrics) -6))
			  (width (ash (ft2-types:ft-glyph-metrics-width metrics) -6))
			  (height (ash (ft2-types:ft-glyph-metrics-height metrics) -6)))
		     
		     (cffi:with-foreign-objects ((p-texture :int))
         		(glGenTextures_foo 1 p-texture)
		      (setq texture (cffi:mem-aref p-texture :int)))
	      
		     (glBindTexture_foo GL_TEXTURE_2D texture)
		     
		     (glTexParameteri_foo GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
		     (glTexParameteri_foo GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
		     (glTexParameteri_foo GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
		     (glTexParameteri_foo GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
		     (gl:tex-env :texture-env :texture-env-mode :modulate)

		     (with-texture-from-bitmap (buffer rows cols pitch bitmap)
		       (glPixelStorei_foo GL_UNPACK_ROW_LENGTH pitch)
		       ;;(glPixelStorei_foo GL_UNPACK_ALIGNMENT 1)
		       ;; GL_UNSIGNED_INT_8_8_8_8
		       (glTexImage2D_foo GL_TEXTURE_2D 0 GL_RGBA cols rows 0 GL_RGBA GL_UNSIGNED_BYTE buffer)

		       (setf (gethash char (font-glyphs font)) (make-glyph-sprite
								:char char
								:texture texture
								:left left
								:top top
								:width width
								:height height
								:advance hori-advance
								:hori-advance hori-advance
								:hori-bearing-x hori-bearing-x
								:hori-bearing-y hori-bearing-y
								:u0 0
								:v0 0
								:u1 1
								:v1 1))))))))
								
	(loop for code in (append (make-list-from-to #x20 #x7E)) ;; printable ascii				  
	   do;; (when (char= (code-char code) #\space) (break))
	     (create-glyph-sprite font (code-char code)))
	font))))


(cffi:defcfun ("memcpy" memcpy) :void (dest :pointer) (src :pointer) (size :uint64))

(defun maybe-resize-standard-vertex-buffer (buffer current-capacity-in-vertices proposed-index)
  (if (< proposed-index (1- current-capacity-in-vertices))
      (values buffer current-capacity-in-vertices)
      (let* ((new-capacity (* 2 current-capacity-in-vertices))
	     (new-buffer (cffi:foreign-alloc '(:struct standard-draw-list-vertex) :count new-capacity)))
	(memcpy new-buffer buffer (* current-capacity-in-vertices
				     (load-time-value (cffi:foreign-type-size '(:struct standard-draw-list-vertex)))))
	(cffi:foreign-free buffer)
	(values new-buffer new-capacity))))

(defun maybe-resize-index-buffer (buffer current-capacity proposed-index)
  (if (< proposed-index (1- current-capacity))
      (values buffer current-capacity)
      (let* ((new-capacity (* 2 current-capacity))
	     (index-type-size (cffi:foreign-type-size +index-type+))
	     (new-buffer (cffi:foreign-alloc +index-type+ :count new-capacity)))
	(memcpy new-buffer buffer (* current-capacity index-type-size))
	(cffi:foreign-free buffer)
	(values new-buffer new-capacity))))

(defun prim-reserve (draw-list index-count vertex-count)
  (when (and (zerop index-count)
	     (zerop vertex-count))
    (return-from prim-reserve nil))
  (let ((proposed-vtx-index (+ (draw-list-vtx-current-idx draw-list) vertex-count)))
    (multiple-value-bind (buffer capacity)
	(maybe-resize-standard-vertex-buffer (draw-list-vtx-buffer draw-list)
					     (draw-list-vtx-buffer-cap draw-list)
					     proposed-vtx-index)
      (setf (draw-list-vtx-buffer draw-list) buffer
	    (draw-list-vtx-buffer-cap draw-list) capacity)))
  
  (let ((proposed-idx-index (+ (draw-list-vtx-current-idx draw-list) vertex-count)))
    (multiple-value-bind (buffer capacity)
	(maybe-resize-index-buffer (draw-list-idx-buffer draw-list)
				   (draw-list-idx-buffer-cap draw-list)
				   proposed-idx-index)
      (setf (draw-list-idx-buffer draw-list) buffer
	    (draw-list-idx-buffer-cap draw-list) capacity)))
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
	 (uv-d-y uv-c-y)
	 (idx (draw-list-vtx-current-idx draw-list))
	 (vtx-write-ptr (draw-list-vtx-write-ptr draw-list))
	 (vtx-write-ptr-pos vtx-write-ptr)
	 (vtx-write-ptr-uv)
	 (vtx-write-ptr-col)
	 (idx-write-ptr (draw-list-idx-write-ptr draw-list)))

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
      (cffi:incf-pointer (draw-list-idx-write-ptr draw-list) (* 6 #.(cffi:foreign-type-size +index-type+)))
      (cffi:incf-pointer (draw-list-vtx-write-ptr draw-list) (* 4 #.(cffi:foreign-type-size '(:struct standard-draw-list-vertex))))
      (incf (draw-list-vtx-current-idx draw-list) 4)
      

      (values))))

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

(defun render-draw-list (draw-list fb-width fb-height clip-off-x clip-off-y clip-scale-x clip-scale-y)
  (let ((vertex-size (load-time-value (cffi:foreign-type-size '(:struct standard-draw-list-vertex)))))
    (let* ((vtx-buffer (draw-list-vtx-buffer draw-list))
	   (vtx-pos-ptr (copy-pointer vtx-buffer))
	   (vtx-tex-ptr (copy-pointer vtx-buffer))
	   (vtx-col-ptr (copy-pointer vtx-buffer))
	   (idx-buffer (copy-pointer (draw-list-idx-buffer draw-list))))

      (cffi:incf-pointer vtx-pos-ptr
			 (load-time-value (cffi:foreign-slot-offset '(:struct standard-draw-list-vertex) 'pos)))
      
      (cffi:incf-pointer vtx-tex-ptr
			 (load-time-value (cffi:foreign-slot-offset '(:struct standard-draw-list-vertex) 'uv)))

      (cffi:incf-pointer vtx-col-ptr
			 (load-time-value (cffi:foreign-slot-offset '(:struct standard-draw-list-vertex) 'red)))

      (%gl:vertex-pointer 2 :float vertex-size vtx-pos-ptr)
		
      (%gl:tex-coord-pointer 2 :float vertex-size vtx-tex-ptr)
		
      (%gl:color-pointer 4 :unsigned-byte vertex-size vtx-col-ptr)

      (loop for cmd across (draw-list-cmd-buffer draw-list) ;; 1 cmd per quad right now
	 do (let* ((clip-rect-x (* (- (draw-cmd-clip-rect-x cmd) clip-off-x) clip-scale-x))
		   (clip-rect-y (* (- (draw-cmd-clip-rect-y cmd) clip-off-y) clip-scale-y))
		   (clip-rect-width (* (- (draw-cmd-clip-rect-width cmd) clip-off-x) clip-scale-x))
		   (clip-rect-height (* (- (draw-cmd-clip-rect-height cmd) clip-off-y) clip-scale-y)))
			
	      (when (and (< clip-rect-x fb-width)
			 (< clip-rect-y fb-height)
			 (>= clip-rect-width 0.0)
			 (>= clip-rect-height 0.0))
		 
		#+NIL(gl:scissor clip-rect-x (- fb-height clip-rect-height)
			    (- clip-rect-width clip-rect-x) (- clip-rect-height clip-rect-y))
			  
		(glBindTexture_foo GL_TEXTURE_2D (draw-cmd-texture-id cmd))

		(glDrawElements_foo GL_TRIANGLES
				    (draw-cmd-elem-count cmd)
				    +index-type-enum+
				    idx-buffer)))

	   (cffi:incf-pointer idx-buffer (* (draw-cmd-elem-count cmd) #.(cffi:foreign-type-size +index-type+)))))))

(defun setup-render-state (fb-width fb-height display-pos-x display-pos-y display-size-x display-size-y)
  ;; this glEnable call is not working, invalid enum
  ;;(gl:disable :blend)
  (gl:enable :blend)
  ;;(glEnable_foo GL_BLEND)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:disable :cull-face)
  (gl:disable :lighting)
  (gl:disable :color-material)
  (gl:enable :scissor-test)
  (gl:enable-client-state :vertex-array)
  (gl:enable-client-state :texture-coord-array)
  (gl:enable-client-state :color-array)
  (glEnable_foo GL_TEXTURE_2D)
  (gl:polygon-mode :front-and-back :fill)
  (gl:tex-env :texture-env :texture-env-mode :modulate)
  (gl:viewport 0 0 fb-width fb-height)
  (gl:matrix-mode :projection)
  (gl:push-matrix)
  (gl:load-identity)
  (gl:ortho display-pos-x (+ display-pos-x display-size-x)
	    (+ display-pos-y display-size-y) display-pos-y
	    -1.0 1.0)
  (gl:matrix-mode :modelview)
  (gl:push-matrix)
  (gl:load-identity))

(defun restore-render-state (last-texture last-polygon-mode last-viewport last-scissor-box last-tex-env-mode)
  (gl:disable-client-state :color-array)
  (gl:disable-client-state :texture-coord-array)
  (gl:disable-client-state :vertex-array)
  (glBindTexture_foo GL_TEXTURE_2D last-texture)
  (gl:matrix-mode :modelview)
  (gl:pop-matrix)
  (gl:matrix-mode :projection)
  (gl:pop-matrix)
  (gl:pop-attrib)
  (gl:polygon-mode :front (elt last-polygon-mode 0))
  (gl:polygon-mode :back (elt last-polygon-mode 1))
  (gl:viewport (elt last-viewport 0) (elt last-viewport 1) (elt last-viewport 2) (elt last-viewport 3))
  (gl:scissor (elt last-scissor-box 0) (elt last-scissor-box 1) (elt last-scissor-box 2) (elt last-scissor-box 3))
  #+NIL
  (gl:tex-env :texture-env :gl-texture-env-mode last-tex-env-mode))

(defun render-draw-lists (draw-lists display-pos-x display-pos-y display-size-x display-size-y)
  (let ((fb-width (* display-size-x *framebuffer-scale*))
	(fb-height (* display-size-y *framebuffer-scale*)))
    
    (when (or (zerop fb-width) (zerop fb-height))
      (return-from render-draw-lists nil))
    
    (let ((last-texture (gl:get-integer :texture-binding-2d))
	  (last-polygon-mode (gl:get-integer :polygon-mode))
	  (last-viewport (gl:get-integer :viewport))
	  (last-scissor-box (gl:get-integer :scissor-box))
	  (last-tex-env-mode #+NIL(%gl:get-tex-env-iv :texture-env :texture-env-mode)))

      (gl:push-attrib :enable-bit :color-buffer-bit :transform-bit)

      (setup-render-state fb-width fb-height display-pos-x display-pos-y display-size-x display-size-y)

      (let ((clip-off-x display-pos-x)
	    (clip-off-y display-pos-y)
	    (clip-scale-x *framebuffer-scale*)
	    (clip-scale-y *framebuffer-scale*))
	(unwind-protect (loop for draw-list in draw-lists
			   do (render-draw-list draw-list fb-width fb-height clip-off-x clip-off-y clip-scale-x clip-scale-y))

	  (restore-render-state last-texture last-polygon-mode last-viewport last-scissor-box last-tex-env-mode))))))




(defun initial-text-engine-setup ()
  (setq *current-font* (ensure-font))
  (setq *draw-list* (initialize-draw-list (make-draw-list)))
  (values))

(defun text-engine-begin-frame ()
  (gl:clear-color 1 1 1 1)
  (gl:clear :color-buffer-bit)
  (reinitialize-draw-list *draw-list*)
  (values))

(defun render-text (pos-x pos-y text &key (color #x000000ff))
  (font-render-text *current-font* *draw-list* pos-x pos-y color text))

(defun test-text ()
  (render-text 100 100 "The quick brown fox jumps over the lazy dog." :color #x0000ffff))

(defun text-engine-end-frame ()
  (render-draw-lists (list *draw-list*) 0 0 *window-x-size* *window-y-size*)
  (values))
	       
(defun run-1 ()
  (sb-int:set-floating-point-modes :traps nil)
  (kons-9::show-window kons-9::*scene*))

(defun print-vertex-buffer (draw-list)
  (let ((vtx-buffer (draw-list-vtx-buffer draw-list)))
    (loop for i from 0 below 4
       do
	 (print "----") (print i)
	 (print (cffi:mem-aref vtx-buffer :float 0))
	 (print (cffi:mem-aref vtx-buffer :float 1))
	 (print (cffi:mem-aref vtx-buffer :float 2))
	 (print (cffi:mem-aref vtx-buffer :float 3))
	 (let ((col-ptr (cffi:foreign-slot-pointer vtx-buffer '(:struct standard-draw-list-vertex) 'red)))
	   (print (cffi:mem-aref col-ptr :unsigned-int)))
	 (finish-output)
	 (cffi:incf-pointer vtx-buffer (cffi:foreign-type-size '(:struct standard-draw-list-vertex))))))

(defun print-vertex (vertex-buffer index)
  (let ((vertex (cffi:mem-aptr vertex-buffer '(:struct standard-draw-list-vertex) index)))
    (print (cffi:mem-aref vertex :float 0))
    (print (cffi:mem-aref vertex :float 1))
    (print (cffi:mem-aref vertex :float 2))
    (print (cffi:mem-aref vertex :float 3))
    (let ((col-ptr (cffi:foreign-slot-pointer vertex '(:struct standard-draw-list-vertex) 'red)))
      (print (cffi:mem-aref col-ptr :unsigned-int)))
    (finish-output)))
    
(defun print-index-buffer (draw-list)
  (let ((idx-buffer (draw-list-idx-buffer draw-list))
	(vtx-buffer (draw-list-vtx-buffer draw-list)))
    (loop for i from 0 below 24 by 6
       do (print "++++") (print i)
	 (print-vertex vtx-buffer (cffi:mem-aref idx-buffer +index-type+ 0))
	 (print-vertex vtx-buffer (cffi:mem-aref idx-buffer +index-type+ 1))
	 (print-vertex vtx-buffer (cffi:mem-aref idx-buffer +index-type+ 2))
	 (print-vertex vtx-buffer (cffi:mem-aref idx-buffer +index-type+ 3))
	 (print-vertex vtx-buffer (cffi:mem-aref idx-buffer +index-type+ 4))
	 (print-vertex vtx-buffer (cffi:mem-aref idx-buffer +index-type+ 5))
	 (finish-output)
	 (cffi:incf-pointer idx-buffer (* 6 (cffi:foreign-type-size +index-type+))))))
	 
