;;;; cl-random-lisp-code.asd

(defpackage :cl-random-lisp-code-system
  (:use :cl :asdf))

(in-package cl-random-lisp-code-system)

(asdf:defsystem #:cl-random-lisp-code
  :serial t
  :description "Generate random programs on the subset of CL"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:iterate #:cl-interpol #:alexandria #:cl-indeterminism #:lol-re)
  :components ((:file "package")
               (:file "cl-random-lisp-code")))

(asdf:defsystem :cl-random-lisp-code-tests
  :description "Tests for CL-RANDOM-LISP-CODE."
  :licence "GPL"
  :serial t
  :depends-on (:cl-random-lisp-code :fiveam :iterate)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-random-lisp-code))))
  (load-system :cl-random-lisp-code-tests)
  (funcall (intern "RUN-TESTS" :cl-random-lisp-code-tests)))
