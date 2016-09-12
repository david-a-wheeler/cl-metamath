(defpackage #:cl-metamath
  (:documentation "Metamath library/verifier in Common Lisp.")
  (:use #:cl #:iterate)
  (:import-from #:alexandria define-constant if-let)
  (:export #:main
           #:load-mmfile
           #:process-metamath-file
           #:*author*
           #:*license*))

(in-package #:cl-metamath)
