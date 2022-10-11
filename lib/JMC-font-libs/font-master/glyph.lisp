(defpackage #:glyph
  (:use :cl)
  (:shadow cl:character)
  (:export #:index
           #:character
           #:code-point
           #:name
           #:em
           #:font
           
	   #:location
	   #:x-offset
	   #:y-offset
	   #:size
           
           ;; generic functions
           ;; bbox
           #:bounding-box
           #:x-min
           #:x-max
           #:y-min
           #:y-max
           
           ;;bearings
           #:left-side-bearing
           #:right-side-bearing
           #:top-side-bearing
           #:bottom-side-bearing

           ;;
           #:advance-width
           #:advance-height
           #:anchors

           #:width
           #:height
           #:origin
           #:kerning
           #:kerning-pairs
           #:data
           #:paths
           #:raster

           #:reify?))

(in-package :glyph)
(declaim (inline reify?))
(defun reify? (value thing ppem)
  (if (and ppem
           (null (zerop value)))
      (* ppem (/ value (em thing)))
      value))

;;; Not necessarily font specific
(defgeneric index (glyph))
(defgeneric character (glyph))
(defgeneric code-point (glyph))
;(defgeneric (glyph))
(defgeneric name (glyph))
(defgeneric font (glyph))
(defgeneric em (glyph))


(defgeneric bounding-box (glyph &optional ppem))
(defgeneric x-min (glyph &optional ppem)
  (:method (glyph &optional ppem) (reify? (aref (bounding-box glyph) 0) glyph ppem))
  (:method ((array array) &optional ppem)(declare (ignore ppem))(aref array 0)))
(defgeneric x-max (glyph &optional ppem)
  (:method (glyph &optional ppem) (reify? (aref (bounding-box glyph) 2) glyph ppem))
  (:method ((array array) &optional ppem)(declare (ignore ppem))(aref array 2)))
(defgeneric y-min (glyph &optional ppem)
  (:method (glyph &optional ppem) (reify? (aref (bounding-box glyph) 1) glyph ppem))
  (:method ((array array) &optional ppem)(declare (ignore ppem))(aref array 1)))
(defgeneric y-max (glyph &optional ppem)
  (:method (glyph &optional ppem) (reify? (aref (bounding-box glyph) 3) glyph ppem))
  (:method ((array array) &optional ppem)(declare (ignore ppem))(aref array 3)))
(defgeneric vertical-origin (glyph &optional ppem)
  (:method (glyph &optional ppem) (let ((x (reify? (+ (left-side-bearing glyph)
                                                      (/ (width glyph) 2)) glyph  ppem))
                                        (y (reify? (+(top-side-bearing glyph)
                                                     (y-max glyph)) glyph  ppem)))
                                    (cons x y))))
(defgeneric horizontal-origin (glyph &optional ppem))

(defgeneric width (glyph &optional ppem)
  (:method (glyph &optional ppem)       ;(reify?  (em glyph) ppem)
    (reify? (- (x-max glyph) (x-min glyph)) glyph  ppem)))
(defgeneric height (glyph &optional ppem)
  (:method (glyph  &optional ppem) (reify? (- (y-max glyph) (y-min glyph)) glyph  ppem)))
(defgeneric size (glyph  &optional ppem)
  (:method (glyph &optional ppem) (cons (width glyph ppem) (height glyph ppem))))


(defgeneric left-side-bearing (glyph &optional ppem)
  (:method (glyph &optional ppem) (x-min glyph ppem))) 
(defgeneric right-side-bearing (glyph &optional ppem)
  (:method (glyph &optional ppem)  (- (advance-width glyph ppem) (left-side-bearing glyph ppem) (width glyph ppem))))
(defgeneric top-side-bearing (glyph &optional ppem)
  (:method (glyph &optional ppem)  (- (cdr (vertical-origin glyph ppem))
                                      (y-max glyph ppem))))
(defgeneric bottom-side-bearing (glyph &optional ppem)
  (:method (glyph &optional ppem)  (- (advance-height glyph ppem)
                                      (top-side-bearing glyph ppem) ;problems if negative
                                      (height glyph ppem)) ; or bearings inside bbox.
  ))       

(defgeneric advance-width (glyph &optional ppem)
  (:method (glyph &optional ppem) (declare (ignore ppem)) 0))
(defgeneric advance-height (glyph &optional ppem)
  (:method (glyph &optional ppem) (declare (ignore ppem)) 0))


(defgeneric kerning (glyph1 glyph2 &optional ppem))
(defgeneric kerning-pairs (glyph))
(defgeneric data (glyph));raw
(defgeneric paths (glyph &key ppem offset &allow-other-keys)) ;cl-vector paths
(defgeneric raster (glyph &optional ppem))
(defgeneric anchors (glyph  &optional ppem))


