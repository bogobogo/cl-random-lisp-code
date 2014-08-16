(in-package :cl-user)

(defpackage :cl-random-lisp-code-tests
  (:use :alexandria :cl :cl-random-lisp-code :fiveam :iterate)
  (:export #:run-tests))

(in-package :cl-random-lisp-code-tests)

(def-suite random-lisp-code)
(in-suite random-lisp-code)

(defun run-tests ()
  (let ((results (run 'random-lisp-code)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test variable-atom-p
  (is (equal nil (cl-random-lisp-code::variable-atom-p '(1 2 3))))
  (is (equal nil (cl-random-lisp-code::variable-atom-p 'var)))
  (is (equal 321 (cl-random-lisp-code::variable-atom-p 'var-321))))

(test code-matches-pattern-p
  (is (cl-rlc::code-matches-pattern-p '*** '(1 2 3)))
  (is (cl-rlc::code-matches-pattern-p '*** '***))
  (is (cl-rlc::code-matches-pattern-p '*** 'var-0))
  (is (cl-rlc::code-matches-pattern-p '*** 'asdf))
  (is (cl-rlc::code-matches-pattern-p '(lambda () 'asdf 'qwer)
				      '(lambda () 'asdf 'qwer)))
  (is (not (cl-rlc::code-matches-pattern-p '(lambda () 'asdf 'qwer)
					   '(lambda () 'asdf1 'qwer1))))
  (is (cl-rlc::code-matches-pattern-p '(lambda () 'asdf '***)
					    '(lambda () 'asdf 'qwer1)))
  (is (cl-rlc::code-matches-pattern-p '(var-0) '(var-0)))
  (let ((cl-rlc::*current-pattern-symbol-table* (let ((it (make-hash-table)))
						  (setf (gethash 'var-0 it) 'var-1)
						  it)))
    (is (cl-rlc::code-matches-pattern-p '(var-1) '(var-0)))
    (is (not (cl-rlc::code-matches-pattern-p '(var-0) '(var-0))))))
  
  
	
(test basics
  (let ((gen-3 (cl-rlc::mk-code-generator-from-pattern '(*** ***) (let ((i 0))
								    (lambda () (incf i)))))
	(gen-4 (cl-rlc::mk-code-generator-from-pattern '*** (let ((i 0))
							      (lambda () (list (incf i) 'a 'b)))))
	(gen-5 (cl-rlc::mk-code-generator-from-pattern '*** (let ((i 0))
							      (lambda () (list (incf i) 'c 'd)))))
	(gen-6 (cl-rlc::mk-code-generator-from-pattern '*** (let ((i 0))
							      (lambda () (list (incf i)
									       (incf i)
									       'a
									       (incf i))))))
	(gen-7 (cl-rlc::mk-code-generator-from-pattern '*** (let ((i 0))
							      (lambda () (list (incf i)
									       (incf i)
									       'c
									       (incf i)))))))
    (let ((conses-1 (iter (for i from 1 to 100)
			  (collect (funcall gen-3))))
	  (conses-2 (iter (for i from 1 to 100)
			  (collect (funcall gen-4))
			  (collect (funcall gen-5))))
	  (conses-3 (iter (for i from 1 to 100)
			  (collect (funcall gen-6))
			  (collect (funcall gen-7)))))
      (let ((pre-dir-prod-1 '(***))
	    (pre-dir-prod-2 '((*** a c) (*** b d)))
	    (pre-dir-prod-3 '(*** (*** a c) ***))
	    (dir-prod-1 '((***)))
	    (dir-prod-2 '((c d) (c b) (c ***)
			  (a d) (a b) (a ***)
			  (*** d) (*** b) (*** ***)))
	    (dir-prod-3 '((*** c ***) (*** a ***) (*** *** ***))))
	(is (equal pre-dir-prod-1 (cl-rlc::pre-patternize-conses-of-same-length conses-1)))
	(is (equal pre-dir-prod-2 (cl-rlc::pre-patternize-conses-of-same-length conses-2)))
	(is (equal pre-dir-prod-3 (cl-rlc::pre-patternize-conses-of-same-length conses-3)))
	(is (equal dir-prod-1 (cl-rlc::direct-product-respecting-wildcard '(***))))
	(is (equal dir-prod-2 (cl-rlc::direct-product-respecting-wildcard '((*** a c) (*** b d)))))
	(is (equal dir-prod-3 (cl-rlc::direct-product-respecting-wildcard '(*** (*** a c) ***))))
	(is (equal '((c d) (c ***) (a b) (a ***) (*** d) (*** b) (*** ***))
		   (cl-rlc::grep-meaningful-patterns dir-prod-2 conses-2)))))))

(test grep-leaf-patterns
  (is (equal '(***) (cl-rlc::grep-leaf-patterns '(***))))
  (is (equal '(a) (cl-rlc::grep-leaf-patterns '(*** a))))
  (is (equal '((a ***) (*** b)) (cl-rlc::grep-leaf-patterns '((*** ***) (a ***) (*** b)))))
  (is (equal '((a b)) (cl-rlc::grep-leaf-patterns '((*** ***) (a ***) (*** b) (a b))))))

(defparameter my-conses
  (let ((gen-1 (cl-rlc::mk-code-generator-from-pattern '(1 foo *** bar) (lambda () (random 1000))))
	(gen-2 (cl-rlc::mk-code-generator-from-pattern '(1 foo1 *** bar1) (lambda () (random 1000)))))
    (iter (for i from 1 to 100)
	  (collect (funcall gen-1))
	  (collect (funcall gen-2)))))

(test patternize-conses-of-same-length
  (is (equal '((foo1 *** bar1) (foo *** bar))
	     (cl-rlc::patternize-conses-of-same-length my-conses))))
						  
