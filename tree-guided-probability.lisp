
;; OK, so the basic idea is to have some interface
;; to be able to prefer some paths on a tree, and not others.

;; Let's first consider the following task:
;;   each node can be either a cons, or one of 2 symbols: A and B.

(defpackage #:cl-rlc-snippets
  (:nicknames #:cl-rlc-snip)
  (:use #:cl #:iterate #:cl-indeterminism #:lol-re #:defmacro-enhance #:lol-re
	#:cl-read-macro-tokens))

(in-package #:cl-rlc-snippets)

(cl-interpol:enable-interpol-syntax)

(enable-read-macro-tokens)

(defparameter *cons-prob* (/ 1 3))
(defparameter *a-sym-prob* (/ 1 3))
(defparameter *b-sym-prob* (/ 1 3))

(defparameter *nvars* 10)
(defparameter *nfuns* 10)

(defparameter *default-weights* `((:cons . 1)
				  (:a . 1)
				  (:b . 1)))

(defparameter *weights* nil)

(defparameter *built-ins* '(cons eq quote atom car cdr cond lambda labels defun))

(defmacro going-car-weights (&body body)
  `(let ((*weights* (cdr (assoc :car-weights *weights*))))
     ,@body))

(defmacro going-cdr-weights (&body body)
  `(let ((*weights* (cdr (assoc :cdr-weights *weights*))))
     ,@body))


(defun fetch-weight (key)
  (or (cdr (assoc key *weights*))
      (cdr (assoc key *default-weights*))
      1))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-weights-and-funs (clauses)
    (iter (for (key . code) in clauses)
	  (collect `(fetch-weight ,key) into weights)
	  (collect `(lambda () ,@code) into funs)
	  (finally (return (list weights funs))))))

(defmacro! rand-cond (&rest clauses)
  (destructuring-bind (weights funs) (generate-weights-and-funs clauses)
    `(let ((,g!-weights (list ,@weights))
	   (,g!-funs (list ,@funs))
	   (,g!-it (random 1.0)))
       (let ((,g!-total (apply #'+ ,g!-weights)))
	 (format t "Weights are ~a, value is ~a~%" ,g!-weights ,g!-it)
	 (iter ,g!-block (for ,g!-weight in ,g!-weights)
	       (for ,g!-fun in ,g!-funs)
	       (if (<= ,g!-it (/ ,g!-weight ,g!-total))
		   (return-from ,g!-block (funcall ,g!-fun))
		   (decf ,g!-it (/ ,g!-weight ,g!-total))))))))

(defmacro! random-indexed (prefix o!-total &body body)
  (let ((gen `(lambda (index)
		    ,@body)))
    `(let ((weights (iter (for i from 0 below ,o!-total)
			  (collect (fetch-weight (keywordicate (format nil "~a-~a" ,prefix i)))))))
       (let ((total (apply #'+ weights))
	     (it (random 1.0)))
	 (iter (for index from 0 below ,o!-total)
	       (for weight in weights)
	       (if (<= it (/ weight total))
		   (return (funcall ,gen index))
		   (decf it (/ weight total))))))))
	       
(defun random-var-name ()
  (random-indexed :var *nvars*
    (intern #?"VAR-$(index)")))

(defun random-fun-name ()
  (random-indexed :fun *nfuns*
    (intern #?"FUN-$(index)")))

(defun random-fun-pointer ()
  `(symbol-function (quote ,(random-fun-name))))

(defun random-symbol ()
  (rand-cond (:var-name (random-var-name))
	     (:fun-name (random-fun-name))
	     (:fun-pointer (random-fun-pointer))
	     (:built-in-sym (random-built-in-sym))))

(defun keywordicate (smth)
  (intern (string smth) (find-package "KEYWORD")))

(defun rand-from-set (set &key prefix)
  (let ((weights (mapcar (lambda (x)
			   (fetch-weight (keywordicate (if prefix
							   #?"$(prefix)-$(x)"
							   x))))
			 set)))
    (let ((total (apply #'+ weights))
	  (it (random 1.0)))
      (iter (for elt in set)
	    (for weight in weights)
	    (if (<= it (/ weight total))
		(return elt)
		(decf it (/ weight total)))))))

(defun random-built-in-sym ()
  (rand-from-set *built-ins* :prefix :built-in))

(defun random-symbol ()
  (rand-cond (:a 'a)
	     (:b 'b)))

(defun random-expression ()
  (rand-cond (:cons (cons (going-car-weights (random-expression))
			  (going-cdr-weights (random-expression))))
	     (t (random-symbol))))
	 
;; now, how to use this framework in my generation of proper lisp code?
