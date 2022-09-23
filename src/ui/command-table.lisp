(in-package #:kons-9)

;;;; macros ====================================================================

(defmacro ct-subtable (key-binding title c-table-fn)
  `(add-entry table
              ,key-binding
              (lambda () (push ,c-table-fn (command-tables *default-scene-view*)))
              (strcat "Show " ,title " menu")))

(defmacro ct-make-shape (key-binding help expr)
  `(add-entry table
              ,key-binding
              (lambda () (add-shape *scene* ,expr))
              (strcat "Create " ,help)))

(defmacro ct-entry (key-binding help &rest expr)
  `(add-entry table
              ,key-binding
              (lambda () ,@expr)
              ,help))


;;;; command-table =============================================================

(defclass-kons-9 command-table-entry ()
  ((key-binding nil)
   (command-fn nil)
   (help-string nil)))

(defun make-command-table-entry (key-binding func doc)
  (make-instance 'command-table-entry :key-binding key-binding :command-fn func :help-string doc))

;;;; command-table =============================================================

(defclass-kons-9 command-table ()
  ((title nil)
   (entries (make-array 0 :adjustable t :fill-pointer t))
   (mouse-help-string "Drag: orbit, [option/alt] track left/right and up/down, [control] track in/out.")))


(defmethod initialize-instance :after ((table command-table) &rest initargs)
  (declare (ignore initargs))
  ;; (add-entry table
  ;;            :h
  ;;            (lambda () (print-command-table-help table))
  ;;            "Print this help message."))
  )

(defmethod add-entry ((table command-table) key-binding func doc)
  (vector-push-extend (make-instance 'command-table-entry
                                     :key-binding key-binding :command-fn func :help-string doc)
                      (entries table)))

(defmethod do-command ((table command-table) key-press)
  (let* ((entry (find key-press (entries table) :test 'eq :key #'key-binding))
         (command-fn (if entry (command-fn entry) nil)))
    (when command-fn
      (funcall command-fn))))

(defmethod print-command-table-help ((table command-table))
  (format t "~%~a table key-binding commands:~%" (title table))
  (when (mouse-help-string table)
    (format t "Mouse: ~a~%" (mouse-help-string table)))
  (loop for entry across (entries table)
        do (format t "~a: ~a~%" (string-downcase (key-binding entry)) (help-string entry))))
