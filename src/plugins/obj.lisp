(in-package #:kons-9)

;;;; obj format import ======================================================

(defun import-obj (filename)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (read-obj stream)))

(defun read-obj (stream)
  (let ((vertices (make-array 0 :adjustable t :fill-pointer t))
        (faces  (make-array 0 :adjustable t :fill-pointer t)))
    (loop for line = (read-line stream nil)
          while line do (parse-obj-line line vertices faces))
    (make-instance 'polyhedron :points vertices :faces faces)))

;;; discard texture and normal refs
(defun obj-decode-vref (str vertices)
  (let ((i (read-from-string (substitute #\space #\/ str))))
    (if (> i 0)
        (1- i)
        (+ (first (array-dimensions vertices)) i))))

(defun parse-obj-line (line vertices faces)
  (when (and (> (length line) 0)        ;skip blank lines
             (not (char-equal #\# (aref (string-trim '(#\space) line) 0)))) ; skip comments
    (let ((s (make-string-input-stream line)))
      (unwind-protect
           (let ((key (read s nil nil)))
             (cond ((eq 'v key)
                    (vector-push-extend (p! (read s) (read s) (read s)) vertices))
                   ((eq 'f key)
                    (let ((vrefs '()))
                      (loop for str = (read-obj-vref-string s)
                            while str do (push (obj-decode-vref str vertices) vrefs))
                      (vector-push-extend (reverse vrefs) faces)))
                   (t nil))))
      (close s))))

(defun is-whitespace-p (c)
  (or (char= c #\tab) (char= c #\space)))

(defun read-obj-vref-string (&optional (stream *standard-input*))
  (let ((letters '()))
    (loop
      for c = (read-char stream nil nil)
      do (cond ((null c)
                (if (null letters)
                    (return nil)
                    (return (coerce (nreverse letters) 'string))))
               ((is-whitespace-p c)
                (when (not (null letters))
                  (return (coerce (nreverse letters) 'string))))
               (t
                (push c letters))))))

;;;; obj format export ======================================================

(defun export-obj (shape filename)
  (with-open-file (stream filename :direction :output)
    (write-obj shape stream)))

(defmethod write-obj ((polyh polyhedron) stream)
  (do-array (i p (points polyh))
    (format stream "v ~A ~A ~A~%" (p:x p) (p:y p) (p:z p)))
  (do-array (i f (faces polyh))
    (format stream "f ")
    (dolist (vref f)
      (format stream "~A " (1+ vref)))
    (format stream "~%")))

