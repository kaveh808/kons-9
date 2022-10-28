#| Doctor for #155 https://github.com/kaveh808/kons-9/issues/155 |#
(in-package #:cl-user)

(let ((bad-confidence-quicklisp-versions
	'("cl-confidence-20220707-git"))
      (current-confidence-quicklisp-verion
	(first
	 (last
	  (pathname-directory
	   (ql:where-is-system "org.melusina.confidence"))))))
  (when (member current-confidence-quicklisp-verion
		bad-confidence-quicklisp-versions
		:test #'string-equal )
    (format t "~&Error: Your current version of Confidence from Quicklisp is too old.")
    (format t "~&~%A temporary solution is to clone an up-to-date version of Confidence
in a directory that is examined by Quicklisp. For instance using the following
shell commands

  cd ~A
  git clone git@github.com:melusina-org/cl-confidence.git

The first line needs to be adapted if Quicklisp has been installed in an
exotic location.

Please visit https://github.com/kaveh808/kons-9/issues/155 to follow
the discussion about this issue." "~/quicklisp/local-projects")
    (quit :unix-status 1)))
