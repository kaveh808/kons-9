
(require 's)

(defun convert-ccl-ffi-call (ff-str)
  (s-with ff-str
    (s-replace "#_glu" "glu-")
    (s-replace "#_gl" "gl-")
    (s-chop-suffixes '("3f" "fv" "i" "f"))
    (s-dashed-words)
    ;;dashed words replaces : with -
    ;;otherwise I'd just do this at the beginning
    (s-replace "gl-" "gl:")
    (s-replace "glu-" "glu:")))

(defun convert-ccl-ffi-enum (ff-str)
  (s-with ff-str
    (s-chop-prefix "#$GL_")
    (s-dashed-words)
    (s-prepend ":")))

(defun replace-using-fn (pattern replace-fn)
  (interactive "sPattern to replace \naFunction to transform matched text: ")
  (goto-char (point-min))
  (unless (re-search-forward pattern nil t 1) (message "Pattern: %s not found in buffer!" pattern))

  (goto-char (point-min))
  (while (re-search-forward pattern nil t 1)
    (let* ((old-text (match-string 0))
           (new-text (funcall replace-fn old-text)))
      (message "%s ==> %s" old-text new-text)
      (replace-match new-text t t))))

(defun convert-ccl-ffi-calls ()
  (interactive)
  (replace-using-fn "#_gl\\w+" #'convert-ccl-ffi-call))

(defun convert-ccl-ffi-enums ()
  (interactive)
  (replace-using-fn "#$GL_[A-za-z_]+" #'convert-ccl-ffi-enum))

(defun convert-ccl-ffi ()
  (interactive)
  (convert-ccl-ffi-calls)
  (convert-ccl-ffi-enums))

(provide 'convert-ccl-ffi)
