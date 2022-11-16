#| Doctor for Quicklisp Installation |#
(in-package #:cl-user)

(defun quicklisp-is-not-installed ()
  (format t "~&Error: Quicklisp is not installed.")
  (format t "~&~%A solution is to install Quicklisp by following the instructions found under

  https://www.quicklisp.org/beta/~%")
  (quit :unix-status 1))


(unless (find-package "QUICKLISP-CLIENT")
  (quicklisp-is-not-installed))
