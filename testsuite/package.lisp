(defpackage #:kons-9/testsuite
   (:local-nicknames (#:confidence #:org.melusina.confidence)
		     (#:p #:origin.vec3))
   (:use #:common-lisp)
   (:import-from #:org.melusina.confidence
    #:define-assertion
    #:define-testcase
    #:assert-string=
    #:assert-condition
    #:assert-eq
    #:assert-list-equal
    #:assert-float-is-essentially-equal
    #:assert-t
    #:assert-t*
    #:list-testcases
    #:*testcase-interactive-p*)
   (:export
    #:list-available-tests
    ))
