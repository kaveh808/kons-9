(in-package :asdf)
(defsystem "font"
  :author "Johannes Martinez Calzada"
  :description "Basic font generic functions."
  :licence "lgpl"
  :version "0.0.1"
  :components ((:file "glyph")
               (:file "font")
               (:file "glyph-doc")
	       (:file "documentation")))
