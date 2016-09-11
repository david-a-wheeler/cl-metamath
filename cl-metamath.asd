;;;; cl-metamath.asd

(asdf:defsystem #:cl-metamath
  :description "Metamath library/verifier in Common Lisp"
           :version "0.0.1"
  :depends-on (#:readable ; Easy-to-read program notation
               #:uiop ; Basic I/O; part of ASDF
               #:iterate ; Improved iterator
               #:alexandria) ; Collection of basic useful helpers
  :serial t
  :components ((:file "package")
               (:file "cl-metamath")))
