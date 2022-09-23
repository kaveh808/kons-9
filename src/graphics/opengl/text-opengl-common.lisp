(in-package :kons-9)

;; these maybe be able to be gotten rid of at some point
;; I recall having a problem with the :texture-2d enum in cl-opengl not being right
;; maybe I'm trippin'
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

(defun ensure-font (&optional (pathname (default-font *drawing-settings*)) (ppem 12))
  (multiple-value-bind (grayscale edge-size glyph-max-width glyph-max-height sprites) (pack-font pathname ppem)

    (let* ((texture)
	   (name (if (pathname-type pathname)
		     (concatenate 'string (pathname-name pathname) "." (pathname-type pathname))
		     (pathname-name pathname)))
	   (font (make-font :name name :size ppem :text-height glyph-max-height)))

      (cffi:with-foreign-objects ((p-texture :int))
        (glGenTextures_foo 1 p-texture)
	(setq texture (cffi:mem-aref p-texture :int)))
	      
      (glBindTexture_foo GL_TEXTURE_2D texture)
    
      (glTexParameteri_foo GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
      (glTexParameteri_foo GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
      (glTexParameteri_foo GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
      (glTexParameteri_foo GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
      (gl:tex-env :texture-env :texture-env-mode :modulate)

      (let ((buffer (make-array (* edge-size edge-size glyph-max-width glyph-max-height 4)
				:element-type '(unsigned-byte 8)))
	    (dest -1)
	    (src -1))
	
	(loop for j from 0 below (* edge-size glyph-max-height)
	   do (loop for i from 0 below (* edge-size glyph-max-width)
		 do (setf (aref buffer (incf dest)) #xff
			  (aref buffer (incf dest)) #xff
			  (aref buffer (incf dest)) #xff
			  (aref buffer (incf dest)) (aref grayscale (incf src)))))
	
	(glPixelStorei_foo GL_UNPACK_ROW_LENGTH (* edge-size glyph-max-width))
	(glPixelStorei_foo GL_UNPACK_ALIGNMENT 1)
	(sb-sys:with-pinned-objects (buffer)
	  (glTexImage2D_foo GL_TEXTURE_2D 0 GL_RGBA
			    (* edge-size glyph-max-width) (* edge-size glyph-max-height)
			    0 GL_RGBA GL_UNSIGNED_BYTE (sb-sys:vector-sap buffer))))

      (dolist (sprite sprites)
	(setf (gethash (glyph-sprite-char sprite) (font-glyphs font))
	      (progn
		(setf (glyph-sprite-texture sprite) texture)
		sprite)))

      (setf (gethash (list name ppem) *font-hash-table*) font))))
