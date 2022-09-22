(in-package :kons-9)

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
		 
		(gl:scissor clip-rect-x (- fb-height clip-rect-height)
			    (- clip-rect-width clip-rect-x) (- clip-rect-height clip-rect-y))
			  
		(glBindTexture_foo GL_TEXTURE_2D (draw-cmd-texture-id cmd))

		(glDrawElements_foo GL_TRIANGLES
				    (draw-cmd-elem-count cmd)
				    +index-type-enum+
				    idx-buffer)))

	   (cffi:incf-pointer idx-buffer (* (draw-cmd-elem-count cmd) #.(cffi:foreign-type-size +index-type+)))))))

(defun setup-render-state (fb-width fb-height display-pos-x display-pos-y display-size-x display-size-y)
  (gl:enable :blend)
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
