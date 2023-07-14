(in-package #:kons-9)

;;; Common Lisp port of the Moller-Trumbore ray-triangle intersection algorithm.
;;; The original C code (raytri.c) authored by Tomas Moller was found here:
;;; https://fileadmin.cs.lth.se/cs/Personal/Tomas_Akenine-Moller/raytri/raytri.c

;;; Function `intersect_triangle` from raytri.c has been ported below as
;;; `intersect/triangle`.  Other optimized variations of this function still
;;; remain to be ported to CL. Read more about the optimized variations in
;;; Moller's insightful article here:
;;; https://fileadmin.cs.lth.se/cs/Personal/Tomas_Akenine-Moller/raytri/


;; Ray-Triangle Intersection Test Routines
;; Different optimizations of my and Ben Trumbore's
;; code from journals of graphics tools (JGT)
;; http://www.acm.org/jgt/
;; by Tomas Moller, May 2000

;; Copyright 2020 Tomas Akenine-Moller

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(defun intersect/triangle (tri ray)
  (let ((epsilon 0.000001))
    (let ((orig (origin.geometry.ray:origin ray))
          (dir (origin.geometry.ray:direction ray))
          (vert0 (origin.geometry.triangle::a tri))
          (vert1 (origin.geometry.triangle::b tri))
          (vert2 (origin.geometry.triangle::c tri)))
      (let* (
             ;; find vectors for two edges sharing vert0
             (edge1 (p:- vert1 vert0))
             (edge2 (p:- vert2 vert0))
             ;; begin calculating determinant - also used to calculate U
             (pvec (p:cross dir edge2))
             ;; if determinant is near zero, ray lies in plane of triangle
             (det (p:dot edge1 pvec)))
        (when (and (> det (- epsilon)) (< det epsilon))
          (return-from intersect/triangle nil))
        (let* ((inv-det (/ 1.0 det))
               ;; calculate distance from vert0 to ray origin
               (tvec (p:- orig vert0))
               ;; calculate U and test bounds
               (u (* (p:dot tvec pvec) inv-det)))
          (when (or (< u 0.0) (> u 1.0))
            (return-from intersect/triangle nil))
          (let* (;;prepare to test V
                 (qvec (p:cross tvec edge1))
                 ;; calculate V parameter and test bounds
                 (v (* (p:dot dir qvec) inv-det)))
            (when (or (< v 0.0) (> (+ u v) 1.0))
              (return-from intersect/triangle nil))
            (let (;; calculate t, ray intersects triangle
                  ;; (using `t_` since `t` clashes in Common Lisp)
                  (t_ (* (p:dot edge2 qvec) inv-det)))
              ;; The original C code returns 1 if intersection occurs and 0
              ;; otherwise, while the actual values computed by the function,
              ;; namely t, u, v are returned indirectly via pointer arguments
              ;; whoose contents are populated by the function.

              ;; Instead, we return `(values t u v)` if intersection occurs and
              ;; nil otherwise.
              (values t_ u v))))))))

