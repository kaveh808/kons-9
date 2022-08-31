(in-package #:kons-9)

;;;; macros ====================================================================

(defmacro ct-subtable (key title c-table-fn)
  `(add-entry table
              ,key
              (lambda () (push ,c-table-fn (command-tables *default-scene-view*)))
              (strcat "Show " ,title " menu")))

(defmacro ct-make-shape (key help expr)
  `(add-entry table
              ,key
              (lambda () (add-shape *scene* ,expr))
              (strcat "Create " ,help)))

(defmacro ct-entry (key help &rest expr)
  `(add-entry table
              ,key
              (lambda () ,@expr)
              ,help))


;;;; command-table =============================================================

(defclass command-table-entry ()
  ((key :accessor key :initarg :key :initform nil)
   (command-fn :accessor command-fn :initarg :command-fn :initform nil)
   (help-string :accessor help-string :initarg :help-string :initform nil)))

(defun make-command-table-entry (key func doc)
  (make-instance 'command-table-entry :key key :command-fn func :help-string doc))

;;;; command-table =============================================================

(defclass command-table ()
  ((title :accessor title :initarg :title :initform nil)
   (entries :accessor entries :initarg :entries :initform (make-array 0 :adjustable t :fill-pointer t))
   (mouse-help-string :accessor mouse-help-string :initarg :mouse-help-string :initform nil)))

(defmethod initialize-instance :after ((table command-table) &rest initargs)
  (declare (ignore initargs))
  (add-entry table
             :h
             (lambda () (print-command-table-help table))
             "Print this help message."))

(defmethod add-entry ((table command-table) key func doc)
  (vector-push-extend (make-instance 'command-table-entry :key key :command-fn func :help-string doc)
                      (entries table)))
  
(defmethod do-command ((table command-table) key-press)
  (let* ((entry (find key-press (entries table) :test 'eq :key #'key))
         (command-fn (if entry (command-fn entry) nil)))
    (when command-fn
      (funcall command-fn))))

(defmethod print-command-table-help ((table command-table))
  (format t "~%~a table key commands:~%" (title table))
  (when (mouse-help-string table)
    (format t "Mouse: ~a~%" (mouse-help-string table)))
  (loop for entry across (entries table)
        do (format t "~a: ~a~%" (string-downcase (key entry)) (help-string entry))))
