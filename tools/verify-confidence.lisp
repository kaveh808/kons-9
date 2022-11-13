#| Doctor for #155 https://github.com/kaveh808/kons-9/issues/155 |#
(in-package #:cl-user)

(defun quicklisp-local-projects ()
  (car (last ql:*local-project-directories*)))

(defun confidence-is-not-installed ()
  (format t "~&Error: Confidence is not installed.")
  (format t "~&~%A solution is to clone Confidence
in a directory that is examined by Quicklisp. For instance using the following
shell commands

  cd ~A
  git clone git@github.com:melusina-org/cl-confidence.git~%"
	  (quicklisp-local-projects))
  (quit :unix-status 1))

(defun confidence-is-outdated ()
  (format t "~&Error: Your current version of Confidence from Quicklisp is too old.")
  (format t "~&~%A temporary solution is to clone an up-to-date version of Confidence
in a directory that is examined by Quicklisp. For instance using the following
shell commands

  cd ~A
  git clone git@github.com:melusina-org/cl-confidence.git

Please visit https://github.com/kaveh808/kons-9/issues/155 to follow
the discussion about this issue.~%"  (quicklisp-local-projects))
  (quit :unix-status 1))

(defun current-confidence-quicklisp-version ()
  (let ((pathname
	  (ql:where-is-system "org.melusina.confidence")))
    (when pathname
      (car (last (pathname-directory pathname))))))

(let ((bad-confidence-quicklisp-versions
	'("cl-confidence-20220707-git"))
      (current-confidence-quicklisp-version
	(current-confidence-quicklisp-version)))
  (unless current-confidence-quicklisp-version
    (confidence-is-not-installed))
  (when (member current-confidence-quicklisp-version
		bad-confidence-quicklisp-versions
		:test #'string-equal )
    (confidence-is-outdated)))
