(in-package #:kons-9)

;;;; marching cubes algorithm ==================================================
;;;; downloaded from web, original copyright included below

;;----------------------------------------------------------------------
;; 
;; Copyright 1997 John Wiseman
;;
;; File:	polygonize.lisp
;; Created:	11 September 1997
;; Author:	John Wiseman 
;; 
;; Description:	This is a Lisp implementation (transcription, practically)
;;		of the marching cubes algorithm, based on the C
;;		implementation by Paul Bourke.
;;		(See <http://www.mhri.edu.au/~pdb/modelling/polygonise/>)
;;
;;
;;		POLYGONIZE-ISOSURFACE (array value)
;;
;;		Given a 3D array and a value, returns a list of triangles
;;		comprising the isosurface.
;;
;;
;;		TRIANGLE-LIST->QD3D-MODEL (triangles)
;;
;;		An MCL-specific function that converts a list of triangles
;;		to a QuickDraw 3D model.  Uses the MCL/QuickDraw 3D
;;		interface.
;;
;;
;; To Do:	Get rid of the 3D array.
;;
;; Changes:     
;; 
;;----------------------------------------------------------------------


;; An XYZ is a 3D point.  We save a couple bytes by just making it a
;; vector.

(defstruct (xyz (:type vector))
  x y z)


;; A TRIANGLE has 3 points.

(defstruct (triangle (:type vector))
  p1 p2 p3)



;; A grid cell is a closure with accessors that make it look like a
;; structure. (This is the only even slightly interesting difference
;; between this and the C implementation.)

(declaim (inline grid-cell-p-ref grid-cell-val-ref))

(defun grid-cell-p-ref (cell index)
  (funcall cell index nil))

(defun grid-cell-val-ref (cell index)
  (funcall cell nil index))



;; The edge table maps vertices under the isosurface to intersecting
;; edges.  An 8 bit index is used to do lookups.

(defparameter *edge-table*
  #(#x0   #x109 #x203 #x30a #x406 #x50f #x605 #x70c
    #x80c #x905 #xa0f #xb06 #xc0a #xd03 #xe09 #xf00
    #x190 #x99  #x393 #x29a #x596 #x49f #x795 #x69c
    #x99c #x895 #xb9f #xa96 #xd9a #xc93 #xf99 #xe90
    #x230 #x339 #x33  #x13a #x636 #x73f #x435 #x53c
    #xa3c #xb35 #x83f #x936 #xe3a #xf33 #xc39 #xd30
    #x3a0 #x2a9 #x1a3 #xaa  #x7a6 #x6af #x5a5 #x4ac
    #xbac #xaa5 #x9af #x8a6 #xfaa #xea3 #xda9 #xca0
    #x460 #x569 #x663 #x76a #x66  #x16f #x265 #x36c
    #xc6c #xd65 #xe6f #xf66 #x86a #x963 #xa69 #xb60
    #x5f0 #x4f9 #x7f3 #x6fa #x1f6 #xff  #x3f5 #x2fc
    #xdfc #xcf5 #xfff #xef6 #x9fa #x8f3 #xbf9 #xaf0
    #x650 #x759 #x453 #x55a #x256 #x35f #x55  #x15c
    #xe5c #xf55 #xc5f #xd56 #xa5a #xb53 #x859 #x950
    #x7c0 #x6c9 #x5c3 #x4ca #x3c6 #x2cf #x1c5 #xcc 
    #xfcc #xec5 #xdcf #xcc6 #xbca #xac3 #x9c9 #x8c0
    #x8c0 #x9c9 #xac3 #xbca #xcc6 #xdcf #xec5 #xfcc
    #xcc  #x1c5 #x2cf #x3c6 #x4ca #x5c3 #x6c9 #x7c0
    #x950 #x859 #xb53 #xa5a #xd56 #xc5f #xf55 #xe5c
    #x15c #x55  #x35f #x256 #x55a #x453 #x759 #x650
    #xaf0 #xbf9 #x8f3 #x9fa #xef6 #xfff #xcf5 #xdfc
    #x2fc #x3f5 #xff  #x1f6 #x6fa #x7f3 #x4f9 #x5f0
    #xb60 #xa69 #x963 #x86a #xf66 #xe6f #xd65 #xc6c
    #x36c #x265 #x16f #x66  #x76a #x663 #x569 #x460
    #xca0 #xda9 #xea3 #xfaa #x8a6 #x9af #xaa5 #xbac
    #x4ac #x5a5 #x6af #x7a6 #xaa  #x1a3 #x2a9 #x3a0
    #xd30 #xc39 #xf33 #xe3a #x936 #x83f #xb35 #xa3c
    #x53c #x435 #x73f #x636 #x13a #x33  #x339 #x230
    #xe90 #xf99 #xc93 #xd9a #xa96 #xb9f #x895 #x99c
    #x69c #x795 #x49f #x596 #x29a #x393 #x99  #x190
    #xf00 #xe09 #xd03 #xc0a #xb06 #xa0f #x905 #x80c
    #x70c #x605 #x50f #x406 #x30a #x203 #x109 #x0))


  
(defparameter *tri-table*
  (make-array
   '(256 16)
   :initial-contents '((-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 8 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 1 9 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (1 8 3 9 8 1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (1 2 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 8 3 1 2 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (9 2 10 0 2 9 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (2 8 3 2 10 8 10 9 8 -1 -1 -1 -1 -1 -1 -1)
                       (3 11 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 11 2 8 11 0 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (1 9 0 2 3 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (1 11 2 1 9 11 9 8 11 -1 -1 -1 -1 -1 -1 -1)
                       (3 10 1 11 10 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 10 1 0 8 10 8 11 10 -1 -1 -1 -1 -1 -1 -1)
                       (3 9 0 3 11 9 11 10 9 -1 -1 -1 -1 -1 -1 -1)
                       (9 8 10 10 8 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (4 7 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (4 3 0 7 3 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 1 9 8 4 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (4 1 9 4 7 1 7 3 1 -1 -1 -1 -1 -1 -1 -1)
                       (1 2 10 8 4 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (3 4 7 3 0 4 1 2 10 -1 -1 -1 -1 -1 -1 -1)
                       (9 2 10 9 0 2 8 4 7 -1 -1 -1 -1 -1 -1 -1)
                       (2 10 9 2 9 7 2 7 3 7 9 4 -1 -1 -1 -1)
                       (8 4 7 3 11 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (11 4 7 11 2 4 2 0 4 -1 -1 -1 -1 -1 -1 -1)
                       (9 0 1 8 4 7 2 3 11 -1 -1 -1 -1 -1 -1 -1)
                       (4 7 11 9 4 11 9 11 2 9 2 1 -1 -1 -1 -1)
                       (3 10 1 3 11 10 7 8 4 -1 -1 -1 -1 -1 -1 -1)
                       (1 11 10 1 4 11 1 0 4 7 11 4 -1 -1 -1 -1)
                       (4 7 8 9 0 11 9 11 10 11 0 3 -1 -1 -1 -1)
                       (4 7 11 4 11 9 9 11 10 -1 -1 -1 -1 -1 -1 -1)
                       (9 5 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (9 5 4 0 8 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 5 4 1 5 0 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (8 5 4 8 3 5 3 1 5 -1 -1 -1 -1 -1 -1 -1)
                       (1 2 10 9 5 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (3 0 8 1 2 10 4 9 5 -1 -1 -1 -1 -1 -1 -1)
                       (5 2 10 5 4 2 4 0 2 -1 -1 -1 -1 -1 -1 -1)
                       (2 10 5 3 2 5 3 5 4 3 4 8 -1 -1 -1 -1)
                       (9 5 4 2 3 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 11 2 0 8 11 4 9 5 -1 -1 -1 -1 -1 -1 -1)
                       (0 5 4 0 1 5 2 3 11 -1 -1 -1 -1 -1 -1 -1)
                       (2 1 5 2 5 8 2 8 11 4 8 5 -1 -1 -1 -1)
                       (10 3 11 10 1 3 9 5 4 -1 -1 -1 -1 -1 -1 -1)
                       (4 9 5 0 8 1 8 10 1 8 11 10 -1 -1 -1 -1)
                       (5 4 0 5 0 11 5 11 10 11 0 3 -1 -1 -1 -1)
                       (5 4 8 5 8 10 10 8 11 -1 -1 -1 -1 -1 -1 -1)
                       (9 7 8 5 7 9 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (9 3 0 9 5 3 5 7 3 -1 -1 -1 -1 -1 -1 -1)
                       (0 7 8 0 1 7 1 5 7 -1 -1 -1 -1 -1 -1 -1)
                       (1 5 3 3 5 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (9 7 8 9 5 7 10 1 2 -1 -1 -1 -1 -1 -1 -1)
                       (10 1 2 9 5 0 5 3 0 5 7 3 -1 -1 -1 -1)
                       (8 0 2 8 2 5 8 5 7 10 5 2 -1 -1 -1 -1)
                       (2 10 5 2 5 3 3 5 7 -1 -1 -1 -1 -1 -1 -1)
                       (7 9 5 7 8 9 3 11 2 -1 -1 -1 -1 -1 -1 -1)
                       (9 5 7 9 7 2 9 2 0 2 7 11 -1 -1 -1 -1)
                       (2 3 11 0 1 8 1 7 8 1 5 7 -1 -1 -1 -1)
                       (11 2 1 11 1 7 7 1 5 -1 -1 -1 -1 -1 -1 -1)
                       (9 5 8 8 5 7 10 1 3 10 3 11 -1 -1 -1 -1)
                       (5 7 0 5 0 9 7 11 0 1 0 10 11 10 0 -1)
                       (11 10 0 11 0 3 10 5 0 8 0 7 5 7 0 -1)
                       (11 10 5 7 11 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (10 6 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 8 3 5 10 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (9 0 1 5 10 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (1 8 3 1 9 8 5 10 6 -1 -1 -1 -1 -1 -1 -1)
                       (1 6 5 2 6 1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (1 6 5 1 2 6 3 0 8 -1 -1 -1 -1 -1 -1 -1)
                       (9 6 5 9 0 6 0 2 6 -1 -1 -1 -1 -1 -1 -1)
                       (5 9 8 5 8 2 5 2 6 3 2 8 -1 -1 -1 -1)
                       (2 3 11 10 6 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (11 0 8 11 2 0 10 6 5 -1 -1 -1 -1 -1 -1 -1)
                       (0 1 9 2 3 11 5 10 6 -1 -1 -1 -1 -1 -1 -1)
                       (5 10 6 1 9 2 9 11 2 9 8 11 -1 -1 -1 -1)
                       (6 3 11 6 5 3 5 1 3 -1 -1 -1 -1 -1 -1 -1)
                       (0 8 11 0 11 5 0 5 1 5 11 6 -1 -1 -1 -1)
                       (3 11 6 0 3 6 0 6 5 0 5 9 -1 -1 -1 -1)
                       (6 5 9 6 9 11 11 9 8 -1 -1 -1 -1 -1 -1 -1)
                       (5 10 6 4 7 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (4 3 0 4 7 3 6 5 10 -1 -1 -1 -1 -1 -1 -1)
                       (1 9 0 5 10 6 8 4 7 -1 -1 -1 -1 -1 -1 -1)
                       (10 6 5 1 9 7 1 7 3 7 9 4 -1 -1 -1 -1)
                       (6 1 2 6 5 1 4 7 8 -1 -1 -1 -1 -1 -1 -1)
                       (1 2 5 5 2 6 3 0 4 3 4 7 -1 -1 -1 -1)
                       (8 4 7 9 0 5 0 6 5 0 2 6 -1 -1 -1 -1)
                       (7 3 9 7 9 4 3 2 9 5 9 6 2 6 9 -1)
                       (3 11 2 7 8 4 10 6 5 -1 -1 -1 -1 -1 -1 -1)
                       (5 10 6 4 7 2 4 2 0 2 7 11 -1 -1 -1 -1)
                       (0 1 9 4 7 8 2 3 11 5 10 6 -1 -1 -1 -1)
                       (9 2 1 9 11 2 9 4 11 7 11 4 5 10 6 -1)
                       (8 4 7 3 11 5 3 5 1 5 11 6 -1 -1 -1 -1)
                       (5 1 11 5 11 6 1 0 11 7 11 4 0 4 11 -1)
                       (0 5 9 0 6 5 0 3 6 11 6 3 8 4 7 -1)
                       (6 5 9 6 9 11 4 7 9 7 11 9 -1 -1 -1 -1)
                       (10 4 9 6 4 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (4 10 6 4 9 10 0 8 3 -1 -1 -1 -1 -1 -1 -1)
                       (10 0 1 10 6 0 6 4 0 -1 -1 -1 -1 -1 -1 -1)
                       (8 3 1 8 1 6 8 6 4 6 1 10 -1 -1 -1 -1)
                       (1 4 9 1 2 4 2 6 4 -1 -1 -1 -1 -1 -1 -1)
                       (3 0 8 1 2 9 2 4 9 2 6 4 -1 -1 -1 -1)
                       (0 2 4 4 2 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (8 3 2 8 2 4 4 2 6 -1 -1 -1 -1 -1 -1 -1)
                       (10 4 9 10 6 4 11 2 3 -1 -1 -1 -1 -1 -1 -1)
                       (0 8 2 2 8 11 4 9 10 4 10 6 -1 -1 -1 -1)
                       (3 11 2 0 1 6 0 6 4 6 1 10 -1 -1 -1 -1)
                       (6 4 1 6 1 10 4 8 1 2 1 11 8 11 1 -1)
                       (9 6 4 9 3 6 9 1 3 11 6 3 -1 -1 -1 -1)
                       (8 11 1 8 1 0 11 6 1 9 1 4 6 4 1 -1)
                       (3 11 6 3 6 0 0 6 4 -1 -1 -1 -1 -1 -1 -1)
                       (6 4 8 11 6 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (7 10 6 7 8 10 8 9 10 -1 -1 -1 -1 -1 -1 -1)
                       (0 7 3 0 10 7 0 9 10 6 7 10 -1 -1 -1 -1)
                       (10 6 7 1 10 7 1 7 8 1 8 0 -1 -1 -1 -1)
                       (10 6 7 10 7 1 1 7 3 -1 -1 -1 -1 -1 -1 -1)
                       (1 2 6 1 6 8 1 8 9 8 6 7 -1 -1 -1 -1)
                       (2 6 9 2 9 1 6 7 9 0 9 3 7 3 9 -1)
                       (7 8 0 7 0 6 6 0 2 -1 -1 -1 -1 -1 -1 -1)
                       (7 3 2 6 7 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (2 3 11 10 6 8 10 8 9 8 6 7 -1 -1 -1 -1)
                       (2 0 7 2 7 11 0 9 7 6 7 10 9 10 7 -1)
                       (1 8 0 1 7 8 1 10 7 6 7 10 2 3 11 -1)
                       (11 2 1 11 1 7 10 6 1 6 7 1 -1 -1 -1 -1)
                       (8 9 6 8 6 7 9 1 6 11 6 3 1 3 6 -1)
                       (0 9 1 11 6 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (7 8 0 7 0 6 3 11 0 11 6 0 -1 -1 -1 -1)
                       (7 11 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (7 6 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (3 0 8 11 7 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 1 9 11 7 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (8 1 9 8 3 1 11 7 6 -1 -1 -1 -1 -1 -1 -1)
                       (10 1 2 6 11 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (1 2 10 3 0 8 6 11 7 -1 -1 -1 -1 -1 -1 -1)
                       (2 9 0 2 10 9 6 11 7 -1 -1 -1 -1 -1 -1 -1)
                       (6 11 7 2 10 3 10 8 3 10 9 8 -1 -1 -1 -1)
                       (7 2 3 6 2 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (7 0 8 7 6 0 6 2 0 -1 -1 -1 -1 -1 -1 -1)
                       (2 7 6 2 3 7 0 1 9 -1 -1 -1 -1 -1 -1 -1)
                       (1 6 2 1 8 6 1 9 8 8 7 6 -1 -1 -1 -1)
                       (10 7 6 10 1 7 1 3 7 -1 -1 -1 -1 -1 -1 -1)
                       (10 7 6 1 7 10 1 8 7 1 0 8 -1 -1 -1 -1)
                       (0 3 7 0 7 10 0 10 9 6 10 7 -1 -1 -1 -1)
                       (7 6 10 7 10 8 8 10 9 -1 -1 -1 -1 -1 -1 -1)
                       (6 8 4 11 8 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (3 6 11 3 0 6 0 4 6 -1 -1 -1 -1 -1 -1 -1)
                       (8 6 11 8 4 6 9 0 1 -1 -1 -1 -1 -1 -1 -1)
                       (9 4 6 9 6 3 9 3 1 11 3 6 -1 -1 -1 -1)
                       (6 8 4 6 11 8 2 10 1 -1 -1 -1 -1 -1 -1 -1)
                       (1 2 10 3 0 11 0 6 11 0 4 6 -1 -1 -1 -1)
                       (4 11 8 4 6 11 0 2 9 2 10 9 -1 -1 -1 -1)
                       (10 9 3 10 3 2 9 4 3 11 3 6 4 6 3 -1)
                       (8 2 3 8 4 2 4 6 2 -1 -1 -1 -1 -1 -1 -1)
                       (0 4 2 4 6 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (1 9 0 2 3 4 2 4 6 4 3 8 -1 -1 -1 -1)
                       (1 9 4 1 4 2 2 4 6 -1 -1 -1 -1 -1 -1 -1)
                       (8 1 3 8 6 1 8 4 6 6 10 1 -1 -1 -1 -1)
                       (10 1 0 10 0 6 6 0 4 -1 -1 -1 -1 -1 -1 -1)
                       (4 6 3 4 3 8 6 10 3 0 3 9 10 9 3 -1)
                       (10 9 4 6 10 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (4 9 5 7 6 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 8 3 4 9 5 11 7 6 -1 -1 -1 -1 -1 -1 -1)
                       (5 0 1 5 4 0 7 6 11 -1 -1 -1 -1 -1 -1 -1)
                       (11 7 6 8 3 4 3 5 4 3 1 5 -1 -1 -1 -1)
                       (9 5 4 10 1 2 7 6 11 -1 -1 -1 -1 -1 -1 -1)
                       (6 11 7 1 2 10 0 8 3 4 9 5 -1 -1 -1 -1)
                       (7 6 11 5 4 10 4 2 10 4 0 2 -1 -1 -1 -1)
                       (3 4 8 3 5 4 3 2 5 10 5 2 11 7 6 -1)
                       (7 2 3 7 6 2 5 4 9 -1 -1 -1 -1 -1 -1 -1)
                       (9 5 4 0 8 6 0 6 2 6 8 7 -1 -1 -1 -1)
                       (3 6 2 3 7 6 1 5 0 5 4 0 -1 -1 -1 -1)
                       (6 2 8 6 8 7 2 1 8 4 8 5 1 5 8 -1)
                       (9 5 4 10 1 6 1 7 6 1 3 7 -1 -1 -1 -1)
                       (1 6 10 1 7 6 1 0 7 8 7 0 9 5 4 -1)
                       (4 0 10 4 10 5 0 3 10 6 10 7 3 7 10 -1)
                       (7 6 10 7 10 8 5 4 10 4 8 10 -1 -1 -1 -1)
                       (6 9 5 6 11 9 11 8 9 -1 -1 -1 -1 -1 -1 -1)
                       (3 6 11 0 6 3 0 5 6 0 9 5 -1 -1 -1 -1)
                       (0 11 8 0 5 11 0 1 5 5 6 11 -1 -1 -1 -1)
                       (6 11 3 6 3 5 5 3 1 -1 -1 -1 -1 -1 -1 -1)
                       (1 2 10 9 5 11 9 11 8 11 5 6 -1 -1 -1 -1)
                       (0 11 3 0 6 11 0 9 6 5 6 9 1 2 10 -1)
                       (11 8 5 11 5 6 8 0 5 10 5 2 0 2 5 -1)
                       (6 11 3 6 3 5 2 10 3 10 5 3 -1 -1 -1 -1)
                       (5 8 9 5 2 8 5 6 2 3 8 2 -1 -1 -1 -1)
                       (9 5 6 9 6 0 0 6 2 -1 -1 -1 -1 -1 -1 -1)
                       (1 5 8 1 8 0 5 6 8 3 8 2 6 2 8 -1)
                       (1 5 6 2 1 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (1 3 6 1 6 10 3 8 6 5 6 9 8 9 6 -1)
                       (10 1 0 10 0 6 9 5 0 5 6 0 -1 -1 -1 -1)
                       (0 3 8 5 6 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (10 5 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (11 5 10 7 5 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (11 5 10 11 7 5 8 3 0 -1 -1 -1 -1 -1 -1 -1)
                       (5 11 7 5 10 11 1 9 0 -1 -1 -1 -1 -1 -1 -1)
                       (10 7 5 10 11 7 9 8 1 8 3 1 -1 -1 -1 -1)
                       (11 1 2 11 7 1 7 5 1 -1 -1 -1 -1 -1 -1 -1)
                       (0 8 3 1 2 7 1 7 5 7 2 11 -1 -1 -1 -1)
                       (9 7 5 9 2 7 9 0 2 2 11 7 -1 -1 -1 -1)
                       (7 5 2 7 2 11 5 9 2 3 2 8 9 8 2 -1)
                       (2 5 10 2 3 5 3 7 5 -1 -1 -1 -1 -1 -1 -1)
                       (8 2 0 8 5 2 8 7 5 10 2 5 -1 -1 -1 -1)
                       (9 0 1 5 10 3 5 3 7 3 10 2 -1 -1 -1 -1)
                       (9 8 2 9 2 1 8 7 2 10 2 5 7 5 2 -1)
                       (1 3 5 3 7 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 8 7 0 7 1 1 7 5 -1 -1 -1 -1 -1 -1 -1)
                       (9 0 3 9 3 5 5 3 7 -1 -1 -1 -1 -1 -1 -1)
                       (9 8 7 5 9 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (5 8 4 5 10 8 10 11 8 -1 -1 -1 -1 -1 -1 -1)
                       (5 0 4 5 11 0 5 10 11 11 3 0 -1 -1 -1 -1)
                       (0 1 9 8 4 10 8 10 11 10 4 5 -1 -1 -1 -1)
                       (10 11 4 10 4 5 11 3 4 9 4 1 3 1 4 -1)
                       (2 5 1 2 8 5 2 11 8 4 5 8 -1 -1 -1 -1)
                       (0 4 11 0 11 3 4 5 11 2 11 1 5 1 11 -1)
                       (0 2 5 0 5 9 2 11 5 4 5 8 11 8 5 -1)
                       (9 4 5 2 11 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (2 5 10 3 5 2 3 4 5 3 8 4 -1 -1 -1 -1)
                       (5 10 2 5 2 4 4 2 0 -1 -1 -1 -1 -1 -1 -1)
                       (3 10 2 3 5 10 3 8 5 4 5 8 0 1 9 -1)
                       (5 10 2 5 2 4 1 9 2 9 4 2 -1 -1 -1 -1)
                       (8 4 5 8 5 3 3 5 1 -1 -1 -1 -1 -1 -1 -1)
                       (0 4 5 1 0 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (8 4 5 8 5 3 9 0 5 0 3 5 -1 -1 -1 -1)
                       (9 4 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (4 11 7 4 9 11 9 10 11 -1 -1 -1 -1 -1 -1 -1)
                       (0 8 3 4 9 7 9 11 7 9 10 11 -1 -1 -1 -1)
                       (1 10 11 1 11 4 1 4 0 7 4 11 -1 -1 -1 -1)
                       (3 1 4 3 4 8 1 10 4 7 4 11 10 11 4 -1)
                       (4 11 7 9 11 4 9 2 11 9 1 2 -1 -1 -1 -1)
                       (9 7 4 9 11 7 9 1 11 2 11 1 0 8 3 -1)
                       (11 7 4 11 4 2 2 4 0 -1 -1 -1 -1 -1 -1 -1)
                       (11 7 4 11 4 2 8 3 4 3 2 4 -1 -1 -1 -1)
                       (2 9 10 2 7 9 2 3 7 7 4 9 -1 -1 -1 -1)
                       (9 10 7 9 7 4 10 2 7 8 7 0 2 0 7 -1)
                       (3 7 10 3 10 2 7 4 10 1 10 0 4 0 10 -1)
                       (1 10 2 8 7 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (4 9 1 4 1 7 7 1 3 -1 -1 -1 -1 -1 -1 -1)
                       (4 9 1 4 1 7 0 8 1 8 7 1 -1 -1 -1 -1)
                       (4 0 3 7 4 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (4 8 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (9 10 8 10 11 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (3 0 9 3 9 11 11 9 10 -1 -1 -1 -1 -1 -1 -1)
                       (0 1 10 0 10 8 8 10 11 -1 -1 -1 -1 -1 -1 -1)
                       (3 1 10 11 3 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (1 2 11 1 11 9 9 11 8 -1 -1 -1 -1 -1 -1 -1)
                       (3 0 9 3 9 11 1 2 9 2 11 9 -1 -1 -1 -1)
                       (0 2 11 8 0 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (3 2 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (2 3 8 2 8 10 10 8 9 -1 -1 -1 -1 -1 -1 -1)
                       (9 10 2 0 9 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (2 3 8 2 8 10 0 1 8 1 10 8 -1 -1 -1 -1)
                       (1 10 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (1 3 8 9 1 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 9 1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (0 3 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
                       (-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1))))


(defun cube-index (grid isolevel)
  (let ((index 0))
    (dotimes (i 8)
      (when (< (grid-cell-val-ref grid i)
               isolevel)
        (incf index (expt 2 i))))
    index))


;; Given a grid cell and an isolevel, calculate the triangular
;; facets required to represent the isosurface through the cell.
;; Return the number of triangular facets, the array "triangles"
;; will be loaded up with the vertices at most 5 triangular facets.
;; 0 will be returned if the grid cell is either totally above
;; of totally below the isolevel.

(defun polygonize (grid isolevel)
  ;; Determine the index into the edge table which
  ;; tells us which vertices are inside of the surface
  (let* ((cube-index (cube-index grid isolevel))
         (edge (aref *edge-table* cube-index)))
    ;; Cube is entirely in/out of the surface
    (if (= edge 0)
      '()
      ;; Find the vertices where the surface intersects the cube
      (let ((vertices (make-array '(12))))
        (macrolet ((maybe-v-interp (bit p1 p2)
                     `(when (not (zerop (logand edge ,(expt 2 bit))))
                        (setf (svref vertices ,bit)
                              (vertex-interp isolevel
                                             (grid-cell-p-ref grid ,p1)
                                             (grid-cell-p-ref grid ,p2)
                                             (grid-cell-val-ref grid ,p1)
                                             (grid-cell-val-ref grid ,p2))))))
          (maybe-v-interp 0 0 1)
          (maybe-v-interp 1 1 2)
          (maybe-v-interp 2 2 3)
          (maybe-v-interp 3 3 0)
          (maybe-v-interp 4 4 5)
          (maybe-v-interp 5 5 6)
          (maybe-v-interp 6 6 7)
          (maybe-v-interp 7 7 4)
          (maybe-v-interp 8 0 4)
          (maybe-v-interp 9 1 5)
          (maybe-v-interp 10 2 6)
          (maybe-v-interp 11 3 7)
          ;; Create the triangles
          (do ((i 0 (+ i 3))
               (triangles '()
                          (cons (make-triangle :p1 (svref vertices
                                                          (aref *tri-table* cube-index i))
                                               :p2 (svref vertices
                                                          (aref *tri-table* cube-index (+ i 1)))
                                               :p3 (svref vertices
                                                          (aref *tri-table* cube-index (+ i 2))))
                                triangles)))
              ((= (aref *tri-table* cube-index i) -1)
               triangles)))))))


     
                
;; Linearly interpolate the position where an isosurface cuts
;; an edge between two vertices, each with their own scalar value

(defun vertex-interp (isolevel p1 p2 val1 val2)
  (cond ((< (abs (- isolevel val1)) .00001)
         p1)
        ((< (abs (- isolevel val2)) .00001)
         p2)
        ((< (abs (- val1 val2)) .00001)
         p1)
        (T
         (let ((mu (/ (- isolevel val1) (- val2 val1))))
           (make-xyz :x (+ (xyz-x p1) (* mu (- (xyz-x p2) (xyz-x p1))))
                     :y (+ (xyz-y p1) (* mu (- (xyz-y p2) (xyz-y p1))))
                     :z (+ (xyz-z p1) (* mu (- (xyz-z p2) (xyz-z p1)))))))))

(defparameter *index-mod-vector* #(#(0 0 0) #(1 0 0) #(1 1 0) #(0 1 0)
                                      #(0 0 1) #(1 0 1) #(1 1 1) #(0 1 1)))


(defun polygonize-isosurface (array isolevel)
  "Given a 3D array and a value, returns a list of triangles
   comprising the isosurface."
  (let ((i) (j) (k))
    (let ((cell-closure
           #'(lambda (point-index value-index)
               (if point-index
                 (let ((mod (svref *index-mod-vector* point-index)))
                   (make-xyz :x (+ i (svref mod 0))
                             :y (+ j (svref mod 1))
                             :z (+ k (svref mod 2))))
                 (let ((mod (svref *index-mod-vector* value-index)))
                   (aref array
                         (+ i (svref mod 0))
                         (+ j (svref mod 1))
                         (+ k (svref mod 2))))))))
      (let ((triangles '()))
        (destructuring-bind (max-x max-y max-z) (array-dimensions array)
          (dotimes (x (- max-x 1))
            (dotimes (y (- max-y 1))
              (dotimes (z (- max-z 1))
                (setq i x
                      j y
                      k z)
                (let ((tris (polygonize cell-closure isolevel)))
                  (when tris
                    (setq triangles (nconc tris triangles))))))))
        triangles))))

;;;; isosurface ================================================================

(defclass-kons-9 isosurface (polyhedron)
  ((field nil)
   (threshold 1.0)
   (point-source-use-volume? t)))

(defmethod printable-data ((self isosurface))
  (strcat (call-next-method) (format nil ", threshold ~a" (threshold self))))

;; TODO -- implement, using parent method (polyhedron) for now in generate-isosurface
;; (defmethod copy-instance-data ((dst isosurface) (src isosurface))
;;   (error "COPY-INSTANCE-DATA not implemented for ISOSURFACE"))

(defmethod generate-isosurface ((iso isosurface))
  (let ((triangles (polygonize-isosurface (scalar-array (field iso)) (threshold iso))))
    (when (> (length triangles) 0)
      (set-triangle-arrays iso triangles)
      ;; merge points and then set values in own arrays so we don't need to
      ;; create a new mesh
      (let ((merged-polyh (merge-points iso)))
        (copy-instance-data iso merged-polyh))
      ;; POLYGONIZE-ISOSURFACE assumes 1 unit per field cell, starting at origin
      ;; we rescale to fit field bounds
      (fit-points-to-field iso)))
  iso)

(defmethod fit-points-to-field ((iso isosurface))
  (with-accessors ((x-dim x-dim) (y-dim y-dim) (z-dim z-dim) (lo bounds-lo) (hi bounds-hi))
      (field iso)
    (let* ((size (p! (1- x-dim) (1- y-dim) (1- z-dim)))
           (dim (p-from-to lo hi))
           (scale (p/ dim size)))
      (do-array (i p (points iso))
        (setf (aref (points iso) i) (p+ (p* p scale) lo)))))
  iso)

(defmethod iso-field-points ((iso isosurface))
  (threshold-cell-points (field iso) (threshold iso)))

(defmethod iso-field-cell-size ((iso isosurface))
  (with-accessors ((x-dim x-dim) (y-dim y-dim) (z-dim z-dim) (lo bounds-lo) (hi bounds-hi))
      (field iso)
    (let* ((size (p! (1- x-dim) (1- y-dim) (1- z-dim)))
           (dim (p-from-to lo hi)))
      (p/ dim size))))

;;;; point-source-protocol =====================================================

(defmethod provides-point-source-protocol? ((iso isosurface))
  t)

(defmethod source-points ((iso isosurface))
  (if (point-source-use-volume? iso)
      (iso-field-points iso)
      (call-next-method)))

