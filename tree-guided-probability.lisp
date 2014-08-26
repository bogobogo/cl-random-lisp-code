
;; OK, so the basic idea is to have some interface
;; to be able to prefer some paths on a tree, and not others.

;; Let's first consider the following task:
;;   each node can be either a cons, or one of 2 symbols: A and B.

(defpackage #:cl-rlc-snippets
  (:nicknames #:cl-rlc-snip)
  (:use #:cl #:iterate #:cl-indeterminism #:lol-re #:defmacro-enhance))

(in-package #:cl-rlc-snippets)

(defparameter *cons-prob* (/ 1 3))
(defparameter *a-sym-prob* (/ 1 3))
(defparameter *b-sym-prob* (/ 1 3))

(defparameter *default-probs* `((:cons . ,(/ 1 3))
				(:a . ,(/ 1 3))
				(:b . ,(/ 1 3))))

(defparameter *probs* nil)

(defmacro going-car-probs (&body body)
  `(let ((*probs* (cdr (assoc :car-probs *probs*))))
     ,@body))

(defmacro going-cdr-probs (&body body)
  `(let ((*probs* (cdr (assoc :cdr-probs *probs*))))
     ,@body))


(defun fetch-prob (key)
  (or (cdr (assoc key *probs*))
      (cdr (assoc key *default-probs*))
      (error "Key ~a not found in probabilities." key)))

(defmacro! rand-cond (&rest clauses)
  `(let ((,g!-probs (list ,@(mapcar (lambda (x)
				      (if (eq 't (car x))
					  1
					  `(fetch-prob ,(car x))))
				    clauses)))
	 (,g!-funs (list ,@(mapcar (lambda (x)
				     `(lambda () ,@(cdr x)))
				   clauses)))
	 (,g!-it (random 1.0)))
     (iter ,g!-block (for ,g!-prob in ,g!-probs)
	   (for ,g!-fun in ,g!-funs)
	   (if (< ,g!-it ,g!-prob)
	       (return-from ,g!-block (funcall ,g!-fun))
	       (decf ,g!-it ,g!-prob)))))

     
			 

(defun random-expression ()
  (rand-cond (:cons (cons (going-car-probs (random-expression))
			  (going-cdr-probs (random-expression))))
	     (:a 'a)
	     (t 'b)))
	 
