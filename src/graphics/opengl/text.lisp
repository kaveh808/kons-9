(in-package :kons-9)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :gl-sampler-binding *features*)
  (pushnew :gl-polygon-mode *features*)
  (pushnew :gl-clip-origin *features*))

(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cl-freetype2))

(eval-when (:compile-toplevel :load-toplevel)
  (use-package :ft2))

(defstruct gl-config
  (gl-version 0)
  (glsl-version #+darwin 150 #-darwin 410)
  (shader-handle 0)
  (vert-handle 0)
  (frag-handle 0)
  (attrib-location-tex 0)
  (attrib-location-proj-mtx 0)
  (attrib-location-vtx-pos 0)
  (attrib-location-vtx-uv 0)
  (attrib-location-vtx-color 0)
  (vbo-handle 0)
  (elements-handle 0))

(defvar *gl-config* (make-gl-config))

(defun create-device-objects ()
  (let* ((last-texture (gl:get-integer :texture-binding-2d))
	 ;;(last-array-buffer (gl:get-integer :array-buffer))
	 (last-vertex-array (gl:get-integer :vertex-array))
	 (vertex-shader (if (<= 130 (gl-config-glsl-version *gl-config*) 299)
			    *vertex-shader-glsl-130-source-code*
			    (if (<= 300 (gl-config-glsl-version *gl-config*)) 
				*vertex-shader-glsl-410-core-source-code*
				nil)))
	 (fragment-shader (if (<= 130 (gl-config-glsl-version *gl-config*) 299)
			      *fragment-shader-glsl-130-source-code*
			      (if (<= 300 (gl-config-glsl-version *gl-config*)) 
				  *fragment-shader-glsl-410-core-source-code*
				  nil))))
    (with-slots (shader-handle vert-handle frag-handle
		 attrib-location-tex attrib-location-proj-mtx
		 attrib-location-vtx-pos attrib-location-vtx-uv
		 attrib-location-vtx-color
		 vbo-handle elements-handle)
	*gl-config*

      (setf vert-handle (gl:create-shader :vertex-shader))
      (gl:shader-source vert-handle vertex-shader)
      (gl:compile-shader vert-handle)
      (check-shader vert-handle "vertex shader")
      
      (setf frag-handle (gl:create-shader :fragment-shader))
      (gl:shader-source frag-handle fragment-shader)
      (gl:compile-shader frag-handle)
      (check-shader frag-handle "fragment shader")

      ;;(when (and vert-handle frag-handle)
	
	
	
	

	(setf shader-handle (gl:create-program))
	(gl:attach-shader shader-handle vert-handle)
	(gl:attach-shader shader-handle frag-handle)
	(gl:link-program shader-handle)

	(setf attrib-location-tex (gl:get-uniform-location shader-handle "Texture"))
	(setf attrib-location-proj-mtx (gl:get-uniform-location shader-handle "ProjMtx"))
	(setf attrib-location-vtx-pos (glgetattriblocation_foo shader-handle "Position"))
	(setf attrib-location-vtx-uv (glgetattriblocation_foo shader-handle "UV"))
	(setf attrib-location-vtx-color (glgetattriblocation_foo shader-handle "Color"))
	
	(setf vbo-handle (elt (gl:gen-buffers 1) 0))
	(setf elements-handle (elt (gl:gen-buffers 1) 0))
	
	(setq *current-font* (ensure-font))
	
	(glBindTexture_foo GL_TEXTURE_2D last-texture)
	;;(gl:bind-buffer :array-buffer last-array-buffer)
	(gl:bind-vertex-array last-vertex-array)

	t)));;)
	    
	    
	

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

(defparameter *vertex-shader-glsl-410-core-source-code*
  (with-output-to-string (s)
    (format s "#version 410~%")
    (format s "layout (location = 0) in vec2 Position;~%")
    (format s "layout (location = 1) in vec2 UV;~%")
    (format s "layout (location = 2) in vec4 Color;~%")
    (format s "uniform mat4 ProjMtx;~%")
    (format s "out vec2 Frag_UV;~%")
    (format s "out vec4 Frag_Color;~%")
    (format s "void main()~%")
    (format s "{~%")
    (format s "    Frag_UV = UV;~%")
    (format s "    Frag_Color = Color;~%")
    (format s "    gl_Position = ProjMtx * vec4(Position.xy,0,1);~%")
    (format s "}~%")))

(defparameter *fragment-shader-glsl-410-core-source-code*
  (with-output-to-string (s)
    (format s "#version 410~%")
    (format s "in vec2 Frag_UV;~%")
    (format s "in vec4 Frag_Color;~%")
    (format s "uniform sampler2D Texture;~%")
    (format s "layout (location = 0) out vec4 Out_Color;~%")
    (format s "void main()~%")
    (format s "{~%")
    (format s "    Out_Color = Frag_Color * texture(Texture, Frag_UV.st);~%")
    (format s "}~%")))

(defparameter *vertex-shader-glsl-130-source-code*
  (with-output-to-string (s)
    (format s "uniform mat4 ProjMtx;~%")
    (format s "in vec2 Position;~%")
    (format s "in vec2 UV;~%")
    (format s "in vec4 Color;~%")
    (format s "out vec2 Frag_UV;~%")
    (format s "out vec4 Frag_Color;~%")
    (format s "void main()~%")
    (format s "{~%")
    (format s "    Frag_UV = UV;~%")
    (format s "    Frag_Color = Color;~%")
    (format s "    gl_Position = ProjMtx * vec4(Position.xy,0,1);~%")
    (format s "}~%")))

(defparameter *fragment-shader-glsl-130-source-code*
  (with-output-to-string (s)
    (format s "precision mediump float;~%")
    (format s "layout (location = 0) in vec2 Position;~%")
    (format s "layout (location = 1) in vec2 UV;~%")
    (format s "layout (location = 2) in vec4 Color;~%")
    (format s "uniform mat4 ProjMtx;~%")
    (format s "out vec2 Frag_UV;~%")
    (format s "out vec4 Frag_Color;~%")
    (format s "void main()~%")
    (format s "{~%")
    (format s "    Frag_UV = UV;~%")
    (format s "    Frag_Color = Color;~%")
    (format s "    gl_Position = ProjMtx * vec4(Position.xy,0,1);~%")
    (format s "}~%")))


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

#+NOTYET(declaim (inline copy-pointer))
(defun copy-pointer (pointer)
  (sb-sys:int-sap (sb-sys:sap-int pointer)))

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
				     :idx-offset (truncate (draw-list-idx-buffer-size draw-list) (load-time-value
												  (cffi:foreign-type-size +index-type+))))))
    
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
(%gl::defglfun ("glDrawElements" glDrawElements_foo) :void (mode :int) (count :int) (type :int) (indices :uint64))

(%gl::defglfun ("glGetUniformLocation" glGetUniformLocation_foo) :int (shader-program :unsigned-int) (name :string))
(%gl::defglfun ("glGetAttribLocation" glGetAttribLocation_foo) :int (shader-program :unsigned-int) (name :string))
(%gl::defglfun ("glBufferData" glBufferData_foo) :void
  (target :int)
  (size :unsigned-int)
  (data :pointer)
  (usage :int))

(%gl::defglfun ("glDrawElementsBaseVertex" glDrawElementsBaseVertex_foo) :void
  (mode :int)
  (count :int)
  (type :int)
  (indices :uint64)
  (base-vertex :int))

(%gl::defglfun ("glPolygonMode" glPolygonMode_foo) :void (arg1 :int) (arg2 :int))

(%gl::defglfun ("glBlendEquationSeparate" glBlendEquationSeparate_foo) :void (arg1 :int) (arg2 :int))
(%gl::defglfun ("glBlendFuncSeparate" glBlendFuncSeparate_foo) :void (arg1 :int) (arg2 :int) (arg3 :int) (arg4 :int))


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
(defconstant GL_ARRAY_BUFFER #x8892)
(defconstant GL_ELEMENT_ARRAY_BUFFER #x8893)
(defconstant GL_STREAM_DRAW #x88E0)
(defconstant GL_FRONT_AND_BACK #x0408)

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

(defun ensure-font (&optional (pathname #+darwin "/System/Library/Fonts/Monaco.ttf" #+linux "/usr/share/fonts/TTF/DejaVuSansMono.ttf") (size 12))
  (let ((last-texture (gl:get-integer :texture-binding-2d)))
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
		do ;; (when (char= (code-char code) #\space) (break))
		   (create-glyph-sprite font (code-char code)))
	  (glBindTexture_foo GL_TEXTURE_2D last-texture)
	  font)))))


(cffi:defcfun ("memcpy" memcpy) :void (dest :pointer) (src :pointer) (size :uint64))

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

(defun check-shader (handle desc)
  (let ((status (gl:get-shader handle :compile-status))
	(log-length (gl:get-shader handle :info-log-length)))

    (unless status
      (warn (concatenate 'string desc " failed to compile")))

    (when (> log-length 1)
      (error (gl:get-shader-info-log handle)))
    t))
      
      
(defun render-draw-list (draw-list fb-width fb-height clip-off-x clip-off-y clip-scale-x clip-scale-y)
  (loop for cmd across (draw-list-cmd-buffer draw-list) ;; 1 cmd per quad right now
	do (let* ((clip-rect-x (* (- (draw-cmd-clip-rect-x cmd) clip-off-x) clip-scale-x))
		  (clip-rect-y (* (- (draw-cmd-clip-rect-y cmd) clip-off-y) clip-scale-y))
		  (clip-rect-width (* (- (draw-cmd-clip-rect-width cmd) clip-off-x) clip-scale-x))
		  (clip-rect-height (* (- (draw-cmd-clip-rect-height cmd) clip-off-y) clip-scale-y)))
			
	     (when (and (< clip-rect-x fb-width)
			(< clip-rect-y fb-height)
			(>= clip-rect-width 0.0)
			(>= clip-rect-height 0.0))
		 
	       (gl:scissor clip-rect-x (- fb-height clip-rect-height)
			   (- clip-rect-width clip-rect-x) (- clip-rect-height clip-rect-y))
		 
	       (glBindTexture_foo GL_TEXTURE_2D (draw-cmd-texture-id cmd))

	       (if (<= 320 (gl-config-gl-version *gl-config*))
		   (%gl:draw-elements-base-vertex :triangles
						  (draw-cmd-elem-count cmd)
						  +index-type+
						  (* (draw-cmd-idx-offset cmd) (load-time-value (cffi:foreign-type-size +index-type+)))
						  (draw-cmd-vtx-offset cmd))

		   (glDrawElements_foo GL_TRIANGLES
				       (draw-cmd-elem-count cmd)
				       +index-type-enum+
				       (* (draw-cmd-idx-offset cmd) (load-time-value (cffi:foreign-type-size +index-type+)))))))))

(defun setup-render-state (draw-list
			   fb-width fb-height
			   display-pos-x display-pos-y display-size-x display-size-y
			   vertex-array-object)
  (let (#+NIL(current-clip-origin #+nil (gl:get-integer :clip-origin))
	(clip-origin-lower-left nil))

    #+NIL
    (when (eq current-clip-origin :upper-left)
      (setq clip-origin-lower-left nil))
    
    (gl:enable :blend)

    (when (> (gl-config-gl-version *gl-config*) 300)
      (gl:blend-equation :func-add))
    
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:disable :cull-face)
    (gl:disable :depth-test)

    (gl:enable :scissor-test)
    #+gl-polygon-mode
    (gl:polygon-mode :front-and-back :fill)
    
    (when (<= (gl-config-gl-version *gl-config*) 300)
      (gl:disable :lighting)
      (gl:disable :color-material)
      (gl:enable-client-state :vertex-array)
      (gl:enable-client-state :texture-coord-array)
      (gl:enable-client-state :color-array)
      (glEnable_foo GL_TEXTURE_2D)
      (gl:tex-env :texture-env :texture-env-mode :modulate))
    
    (gl:viewport 0 0 fb-width fb-height)
    
    (when (<= (gl-config-gl-version *gl-config*) 300)
      (gl:matrix-mode :projection)
      (gl:push-matrix)
      (gl:load-identity)
      (gl:ortho display-pos-x (+ display-pos-x display-size-x)
		(+ display-pos-y display-size-y) display-pos-y
		-1.0 1.0)
      (gl:matrix-mode :modelview)
      (gl:push-matrix)
      (gl:load-identity))

    (when (> (gl-config-gl-version *gl-config*) 300)
      (let ((l (clampf display-pos-x))
	    (r (clampf (+ display-pos-x display-size-x)))
	    (top (clampf display-pos-y))
	    (bot (clampf (+ display-pos-y display-size-y))))
	
	(when clip-origin-lower-left
	  (let ((tmp top))
	    (setq top bot
		  bot tmp)))
		
	(cffi:with-foreign-objects ((p-ortho-projection :float 16))
	  (setf (cffi:mem-aref p-ortho-projection :float 0) (/ 2.0f0 (- r l))
		(cffi:mem-aref p-ortho-projection :float 1) 0.0f0
		(cffi:mem-aref p-ortho-projection :float 2) 0.0f0
		(cffi:mem-aref p-ortho-projection :float 3) 0.0f0
		(cffi:mem-aref p-ortho-projection :float 4) 0.0f0
		(cffi:mem-aref p-ortho-projection :float 5) (/ 2.0f0 (- top bot))
		(cffi:mem-aref p-ortho-projection :float 6) 0.0f0
		(cffi:mem-aref p-ortho-projection :float 7) 0.0f0
		(cffi:mem-aref p-ortho-projection :float 8) 0.0f0
		(cffi:mem-aref p-ortho-projection :float 9) 0.0f0
		(cffi:mem-aref p-ortho-projection :float 10) -1.0f0
		(cffi:mem-aref p-ortho-projection :float 11) 0.0f0
		(cffi:mem-aref p-ortho-projection :float 12) (/ (+ r l) (- l r))
		(cffi:mem-aref p-ortho-projection :float 13) (/ (+ top bot) (- bot top))
		(cffi:mem-aref p-ortho-projection :float 14) 0.0f0
		(cffi:mem-aref p-ortho-projection :float 15) 1.0f0)
      
	  (with-slots (gl-version shader-handle
		       attrib-location-tex attrib-location-proj-mtx
		       attrib-location-vtx-pos attrib-location-vtx-uv attrib-location-vtx-color
		       vbo-handle elements-handle)
	      *gl-config*
	    
	    (gl:use-program shader-handle)
	    (%gl:uniform-1i attrib-location-tex 0)
	    
	    (%gl:uniform-matrix-4fv attrib-location-proj-mtx 1 nil p-ortho-projection)

	    #+gl-sampler-binding
	    (gl:bind-sampler 0 0)

	    (gl:bind-vertex-array vertex-array-object)

	    (let* ((vertex-size (load-time-value (cffi:foreign-type-size '(:struct standard-draw-list-vertex)))))

	      (when (< gl-version 300)
		(let* ((vtx-buffer (draw-list-vtx-buffer draw-list))
		       (vtx-pos-ptr (copy-pointer vtx-buffer))
		       (vtx-tex-ptr (copy-pointer vtx-buffer))
		       (vtx-col-ptr (copy-pointer vtx-buffer)))

		  (cffi:incf-pointer vtx-pos-ptr
		      (load-time-value (cffi:foreign-slot-offset '(:struct standard-draw-list-vertex) 'pos)))
		  
		  (cffi:incf-pointer vtx-tex-ptr
		      (load-time-value (cffi:foreign-slot-offset '(:struct standard-draw-list-vertex) 'uv)))
		  
		  (cffi:incf-pointer vtx-col-ptr
		      (load-time-value (cffi:foreign-slot-offset '(:struct standard-draw-list-vertex) 'red)))
		  
		  (%gl:vertex-pointer 2 :float vertex-size vtx-pos-ptr)
		  (%gl:tex-coord-pointer 2 :float vertex-size vtx-tex-ptr)
		  (%gl:color-pointer 4 :unsigned-byte vertex-size vtx-col-ptr)))
    
	      (when (>= gl-version 300)
		(gl:bind-buffer :array-buffer vbo-handle)
		(gl:bind-buffer :element-array-buffer elements-handle)
		(gl:enable-vertex-attrib-array attrib-location-vtx-pos)
		(gl:enable-vertex-attrib-array attrib-location-vtx-uv)
		(gl:enable-vertex-attrib-array attrib-location-vtx-color)	
		(gl:vertex-attrib-pointer attrib-location-vtx-pos 2 :float nil vertex-size
					  (load-time-value (cffi:foreign-slot-offset '(:struct standard-draw-list-vertex) 'pos)))
		(gl:vertex-attrib-pointer attrib-location-vtx-uv 2 :float nil vertex-size
					  (load-time-value (cffi:foreign-slot-offset '(:struct standard-draw-list-vertex) 'uv)))
		(gl:vertex-attrib-pointer attrib-location-vtx-color 4 :unsigned-byte t vertex-size
					  (load-time-value (cffi:foreign-slot-offset '(:struct standard-draw-list-vertex) 'red)))))))))))

(defun render-draw-lists (draw-lists display-pos-x display-pos-y display-size-x display-size-y)
  (let ((fb-width (* display-size-x *framebuffer-scale*))
	(fb-height (* display-size-y *framebuffer-scale*)))
    
    (when (or (zerop fb-width) (zerop fb-height))
      (return-from render-draw-lists nil))
    
    (let ((last-active-texture (gl:get-integer :active-texture)))
      
      (gl:active-texture :texture0)

      (let ((last-program (gl:get-integer :current-program))
	    (last-texture (gl:get-integer :texture-binding-2d))
	    #+gl-sampler-binding(last-sampler (gl:get-integer :sampler-binding))
	    (last-array-buffer (gl:get-integer :array-buffer-binding))
	    (last-vertex-array-object (gl:get-integer :vertex-array-binding))
	    (last-polygon-mode (gl:get-integer :polygon-mode))
	    (last-viewport (gl:get-integer :viewport))
	    (last-scissor-box (gl:get-integer :scissor-box))
	    (last-enabled-blend? (%gl:is-enabled :blend))
	    (last-enabled-cull-face? (%gl:is-enabled :cull-face))
	    (last-enabled-depth-test? (%gl:is-enabled :depth-test))
	    (last-enabled-scissor-test? (%gl:is-enabled :scissor-test))
	    (last-blend-src-rgb (gl:get-integer :blend-src-rgb))
	    (last-blend-dst-rgb (gl:get-integer :blend-dst-rgb))
	    (last-blend-src-alpha (gl:get-integer :blend-src-alpha))
	    (last-blend-dst-alpha (gl:get-integer :blend-dst-alpha))
	    (last-blend-equation-rgb (gl:get-integer :blend-equation-rgb))
	    (last-blend-equation-alpha (gl:get-integer :blend-equation-alpha)))

	(when (< (gl-config-gl-version *gl-config*) 300)
	  (gl:push-attrib :enable-bit :color-buffer-bit :transform-bit))

	(let ((vertex-array-object (elt (gl:gen-vertex-arrays 1) 0))
	      (clip-off-x display-pos-x)
	      (clip-off-y display-pos-y)
	      (clip-scale-x *framebuffer-scale*)
	      (clip-scale-y *framebuffer-scale*))
	  
	  (unwind-protect (loop for draw-list in draw-lists
				do (setup-render-state draw-list fb-width fb-height
						       display-pos-x display-pos-y display-size-x display-size-y
						       vertex-array-object)

				   (let ((vertex-buffer-size (draw-list-vtx-buffer-size draw-list))
					 (index-buffer-size (draw-list-idx-buffer-size draw-list)))
				     
				     (glbufferdata_foo GL_ARRAY_BUFFER
						       vertex-buffer-size
						       (draw-list-vtx-buffer draw-list)
						       GL_STREAM_DRAW)

				     (glbufferdata_foo GL_ELEMENT_ARRAY_BUFFER
						       index-buffer-size
						       (draw-list-idx-buffer draw-list)
						       GL_STREAM_DRAW))
				   
				   (render-draw-list draw-list fb-width fb-height clip-off-x clip-off-y clip-scale-x clip-scale-y))
	    (flet ((puke (int)
		     (declare (ignorable int))
		     #+NIL(progn
		       (print int)
		       (finish-output))))

	      (gl:delete-vertex-arrays (list vertex-array-object))
	      (puke 0)
	      (gl:use-program last-program)
	      (puke 1)
	      (glbindtexture_foo GL_TEXTURE_2D last-texture)
	      (puke 2)
	      #+gl-sampler-binding(gl:bind-sampler 0 last-sampler)
	      (puke 3)
	      (gl:active-texture last-active-texture)
	      (puke 4)
	      (gl:bind-vertex-array last-vertex-array-object)
	      (puke 5)
	      (gl:bind-buffer :array-buffer last-array-buffer)
	      (puke 6)
	      (glBlendEquationSeparate_foo last-blend-equation-rgb last-blend-equation-alpha)
	      (glBlendFuncSeparate_foo last-blend-src-rgb last-blend-dst-rgb last-blend-src-alpha last-blend-dst-alpha)
	      (puke 7)
	      (if last-enabled-blend? (gl:enable :blend) (gl:disable :blend))
	      (if last-enabled-cull-face? (gl:enable :cull-face) (gl:disable :cull-face))
	      (if last-enabled-depth-test? (gl:enable :depth-test) (gl:disable :depth-test))
	      (if last-enabled-scissor-test? (gl:enable :scissor-test) (gl:disable :scissor-test))
	      (glpolygonmode_foo GL_FRONT_AND_BACK (elt last-polygon-mode 0))
	      (puke 11)
	      (gl:viewport (elt last-viewport 0) (elt last-viewport 1) (elt last-viewport 2) (elt last-viewport 3))
	      (puke 12)
	      (gl:scissor (elt last-scissor-box 0) (elt last-scissor-box 1) (elt last-scissor-box 2) (elt last-scissor-box 3))
	      (puke 13))))))))

(defun initial-text-engine-setup ()
  (let ((major (gl:get-integer :major-version))
	(minor (gl:get-integer :minor-version)))
    (setf (gl-config-gl-version *gl-config*) (+ (* major 100) (* minor 10))))
  
  (format t "~%OpenGL Version: ~A" (gl:gl-version))
  (finish-output)
  
  (if (<= 300 (gl-config-gl-version *gl-config*))
      (create-device-objects)
      (warn "unsupported gl-version for fonts"))
  
  (setq *draw-list* (initialize-draw-list (make-draw-list)))
  (values))

(defun text-engine-begin-frame ()
  (reinitialize-draw-list *draw-list*)
  (values))

(defun render-text (pos-x pos-y text &key (color #x000000ff))
  (font-render-text *current-font* *draw-list* pos-x pos-y color text))

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

	 
