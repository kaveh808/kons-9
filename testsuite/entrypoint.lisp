(in-package #:kons-9/testsuite)

(defun list-available-tests ()
  (format t "~&~%Available testcases:")
  (loop :for testcase :in (list-testcases "KONS-9/TESTSUITE")
	:do (format t "~& ~S" testcase))
  (format t "~&")
  (unless *testcase-interactive-p*
    (uiop:quit 0)))

(define-testcase run-all-tests ()
  "Run all available tests."
  (testsuite-utils)
  (testsuite-point-cloud)
  (testsuite-ray-triangle))
