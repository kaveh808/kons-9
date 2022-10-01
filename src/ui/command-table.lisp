(in-package #:kons-9)

;;;; macros ====================================================================

(defmacro ct-subtable (key-binding title c-table-fn)
  `(add-entry table
              ,key-binding
              (lambda () (make-active-command-table ,c-table-fn))
              (strcat ,title " Menu")))

(defmacro ct-make-shape (key-binding help expr)
  `(add-entry table
              ,key-binding
              (lambda () (execute (add-shape *scene* ,expr))) ;do transaction
              (strcat "Create " ,help)))

(defmacro ct-entry (key-binding help &rest expr)
  `(add-entry table
              ,key-binding
              (lambda () ,@expr)
              ,help))


;;;; command-table-entry =======================================================

(defclass-kons-9 command-table-entry ()
  ((key-binding nil)
   (command-fn nil)
   (help-string nil)))

(defun make-command-table-entry (key-binding func doc)
  (make-instance 'command-table-entry :key-binding key-binding :command-fn func :help-string doc))

;;;; command-table =============================================================

(defclass-kons-9 command-table ()
  ((title nil)
   (entries (make-array 0 :adjustable t :fill-pointer t))))

(defmethod add-entry ((table command-table) key-binding func doc)
  (vector-push-extend (make-command-table-entry key-binding func doc)
                      (entries table)))

(defmethod do-command ((table command-table) key-press)
  (let* ((entry (find key-press (entries table) :test 'eq :key #'key-binding))
         (command-fn (if entry (command-fn entry) nil)))
    (when command-fn
      (funcall command-fn))))
