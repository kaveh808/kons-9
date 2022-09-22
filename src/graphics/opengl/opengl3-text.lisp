(in-package :kons-9)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :gl-sampler-binding *features* :test #'equalp)
  (pushnew :gl-polygon-mode *features* :test #'equalp))

(defstruct gl-config
  (gl-version 0)
  (glsl-version 410)
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

      (setf shader-handle (gl:create-program))
      (gl:attach-shader shader-handle vert-handle)
      (gl:attach-shader shader-handle frag-handle)
      (gl:link-program shader-handle)

      (setf attrib-location-tex (gl:get-uniform-location shader-handle "Texture"))
      (setf attrib-location-proj-mtx (gl:get-uniform-location shader-handle "ProjMtx"))
      (setf attrib-location-vtx-pos (gl:get-attrib-location shader-handle "Position"))
      (setf attrib-location-vtx-uv (gl:get-attrib-location shader-handle "UV"))
      (setf attrib-location-vtx-color (gl:get-attrib-location shader-handle "Color"))
	
      (setf vbo-handle (elt (gl:gen-buffers 1) 0))
      (setf elements-handle (elt (gl:gen-buffers 1) 0))
	
      (setq *current-font* (ensure-font))
	
      (glBindTexture_foo GL_TEXTURE_2D last-texture)
      ;;(gl:bind-buffer :array-buffer last-array-buffer)
      (gl:bind-vertex-array last-vertex-array)

      t)))
	    
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
    (format s "#version 130~%")
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
    (format s "#version 130~%")
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

		   (progn (glDrawElements_foo GL_TRIANGLES
					      (draw-cmd-elem-count cmd)
					      +index-type-enum+
					      (draw-list-idx-buffer draw-list))
			  (cffi:incf-pointer (draw-list-idx-buffer draw-list)
					     (* (draw-cmd-idx-offset cmd) (load-time-value (cffi:foreign-type-size +index-type+))))))))))

(defun setup-render-state (draw-list
			   fb-width fb-height
			   display-pos-x display-pos-y display-size-x display-size-y
			   vertex-array-object)
  (let (#+NILclip-origin-lower-left nil))

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

	#+NIL
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
					  (load-time-value (cffi:foreign-slot-offset '(:struct standard-draw-list-vertex) 'red))))))))))

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
				     
               (%gl:buffer-data :array-buffer
                                vertex-buffer-size
                               (draw-list-vtx-buffer draw-list)
                               :stream-draw )

               (%gl:buffer-data :element-array-buffer
                                index-buffer-size
                                (draw-list-idx-buffer draw-list)
                                :stream-draw ))
				   
			       (render-draw-list draw-list fb-width fb-height clip-off-x clip-off-y clip-scale-x clip-scale-y))
	    
	    (gl:delete-vertex-arrays (list vertex-array-object))

	    (gl:use-program last-program)

	    (glbindtexture_foo GL_TEXTURE_2D last-texture)

	    #+gl-sampler-binding(gl:bind-sampler 0 last-sampler)

	    (gl:active-texture last-active-texture)

	    (gl:bind-vertex-array last-vertex-array-object)

	    (gl:bind-buffer :array-buffer last-array-buffer)

	    (gl:blend-equation-separate last-blend-equation-rgb last-blend-equation-alpha)
	    (gl:blend-func-separate last-blend-src-rgb last-blend-dst-rgb last-blend-src-alpha last-blend-dst-alpha)

	    (if last-enabled-blend? (gl:enable :blend) (gl:disable :blend))
	    (if last-enabled-cull-face? (gl:enable :cull-face) (gl:disable :cull-face))
	    (if last-enabled-depth-test? (gl:enable :depth-test) (gl:disable :depth-test))
	    (if last-enabled-scissor-test? (gl:enable :scissor-test) (gl:disable :scissor-test))
	    (glpolygonmode_foo GL_FRONT_AND_BACK (elt last-polygon-mode 0))

	    (gl:viewport (elt last-viewport 0) (elt last-viewport 1) (elt last-viewport 2) (elt last-viewport 3))

	    (gl:scissor (elt last-scissor-box 0) (elt last-scissor-box 1) (elt last-scissor-box 2) (elt last-scissor-box 3))))))))

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
