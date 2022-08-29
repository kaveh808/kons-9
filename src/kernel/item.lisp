(in-package #:kons-9)

;;;; item ======================================================================

;;; root class for scene entities
(defclass item ()
  ())

(defmethod print-object ((self item) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream (printable-data self))))

(defmethod printable-data ((self item))
  "")
