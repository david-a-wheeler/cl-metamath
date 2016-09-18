(defpackage #:cl-metamath
  (:documentation "Metamath library/verifier in Common Lisp.")
  (:use #:cl #:iterate)
  (:import-from #:alexandria define-constant if-let
    hash-table-plist hash-table-alist hash-table-keys)
  (:export #:main
           #:load-mmfile
           #:process-metamath-file
           #:*author*
           #:*license*))

(in-package #:cl-metamath)
