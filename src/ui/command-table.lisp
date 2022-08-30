(in-package #:kons-9)

;;;; command-table =============================================================

(defclass command-table-entry ()
  ((key :accessor key :initarg :key :initform nil)
   (command-fn :accessor command-fn :initarg :command-fn :initform nil)
   (help-string :accessor help-string :initarg :help-string :initform nil)))

(defun make-command-table-entry (key func doc)
  (make-instance 'command-table-entry :key key :command-fn func :help-string doc))

;;;; command-table =============================================================

(defclass command-table ()
  ((entries :accessor entries :initarg :entries :initform (make-array 0 :adjustable t :fill-pointer t))
   (mouse-help-string :accessor mouse-help-string :initarg :mouse-help-string :initform nil)))

(defmethod add-entry ((table command-table) key func doc)
  (vector-push-extend (make-instance 'command-table-entry :key key :command-fn func :help-string doc)
                      (entries table)))
  
(defmethod do-command ((table command-table) key-press)
  (let* ((entry (find key-press (entries table) :test 'eq :key #'key))
         (command-fn (if entry (command-fn entry) nil)))
    (when command-fn
      (funcall command-fn))))

(defmethod print-command-table-help ((table command-table))
  (when (mouse-help-string table)
    (format t "Mouse: ~a~%" (mouse-help-string table)))
  (loop for entry across (entries table)
        do (format t "~a: ~a~%" (string-downcase (key entry)) (help-string entry))))
