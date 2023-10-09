(in-package #:kons-9)

;;;; json ======================================================================

(defun load-json (filename)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (shasht:read-json stream)))

;;;; json-mixin ================================================================

(defclass-kons-9 json-mixin ()
  ((hash-data nil)))                      ;JSON hash

(defmethod get-json-attr ((self json-mixin) attr)
  (gethash attr (hash-data self)))

