;;;; cl-random-lisp-code.asd

(asdf:defsystem #:cl-random-lisp-code
  :serial t
  :description "Generate random programs on the subset of CL"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:iterate #:cl-interpol)
  :components ((:file "package")
               (:file "cl-random-lisp-code")))

