;;;; cl-random-lisp-code.lisp

(in-package #:cl-random-lisp-code)

(enable-read-macro-tokens)

(define-condition random-codegen-fail (error)
  ())

(defmacro if-rand (prob then &optional else)
  `(if (> ,prob (random 1.0))
       (handler-case ,then
	 (random-codegen-fail () ,else))
       ,else))

(defmacro poisson-forms ((total decay) &body body)
  (let ((g!-i (gensym "G!-I")))
    `(iter (for ,g!-i from 1 to (discrete-poisson ,total ,decay))
	   (collect (progn ,@body)))))

(cl-interpol:enable-interpol-syntax)

(defparameter *atom-prob* 0.5)
(defparameter *variable-prob* 0.5)

(defparameter *symbol-table* '())
(defparameter *vanilla-function-table* '((eq . 2) (atom . 1) (car . 1) (cdr . 1)
					 (cons . 2)))
(defparameter *function-table* (list *vanilla-function-table*))
(defparameter *allow-undefined* nil)

(defparameter *nvars* 100)

(defun function-table-length ()
  (labels ((rec (table acc)
	     (if (not table)
		 acc
		 (rec (cdr table) (+ acc (length (car table)))))))
    (rec *function-table* 0)))

(defun function-table-fetch (pos)
  (labels ((rec (table pos)
	     (if (not table)
		 (error 'random-codegen-fail :msg "Failed to fetch random function name.")
		 (let ((it (length (car table))))
		   (if (> pos (1- it))
		       (rec (cdr table) (- pos it))
		       (elt (car table) pos))))))
    (rec *function-table* pos)))

(defun %random-function ()
  (let ((it (function-table-length)))
    (function-table-fetch (random it))))

(defun random-function-call ()
  (destructuring-bind (fname . nargs) (%random-function)
    (cons fname (iter (for i from 1 to nargs)
		      (collect (random-expression))))))

(defun %random-var-name (&optional number)
  (intern #?"VAR-$((or number (random *nvars*)))"))

(defun %symbol-table-find (sym symbol-table)
  (if symbol-table
      (if (find sym (car symbol-table))
	  sym
	  (%symbol-table-find sym (cdr symbol-table)))))

(defun symbol-table-find (sym)
  (%symbol-table-find sym *symbol-table*))

(defun %function-table-find (sym symbol-table)
  (if symbol-table
      (if (find sym (car symbol-table))
	  sym
	  (%symbol-table-find sym (cdr symbol-table)))))

(defun function-table-find (sym)
  (%function-table-find sym *function-table*))


(defun random-var-name ()
  (if (and (not *allow-undefined*)
	   (not *symbol-table*))
      (error 'random-codegen-fail :msg "Attempt to generate variable name with empty symbol table")
      (let ((it (%random-var-name)))
	(iter (while (and (not *allow-undefined*)
			  (not (symbol-table-find it))))
	      (setf it (%random-var-name)))
	it)))



(defparameter *n-ints* 5)
(defparameter *data-expr-max-depth* 5)

(defun random-data-expr ()
  (let ((*allow-undefined* t)
	(*list-decay* 0.2))
    `(quote ,(%random-data-expr *data-expr-max-depth*))))

(defun %random-data-expr (&optional (depth 0))
  (if-rand *atom-prob*
	   (random-data-atom)
	   (if (< 0 depth)
	       (random-data-cons depth)
	       (random-data-atom))))

(defparameter *depth* 10)

(defun random-expression (&optional (top t))
  (if top
      (if-rand *atom-prob*
	       (random-atom)
	       (if (equal 0 *depth*)
		   (random-atom)
		   (let ((*depth* (1- *depth*)))
		     (random-cons-expression))))
      (random 10)))

;; (defun random-expression ()
;;   (random 10))
	   
(defun random-atom ()
  (if-rand *variable-prob*
	   (random-var-name)
	   (random-data-expr)))

(defun random-data-atom ()
  (if-rand *variable-prob*
	   (random-data-symbol)
	   (random *n-ints*)))

(defparameter *limit-of-list-elts* 10)
(defparameter *list-decay* 0.8)

(defun random-data-cons (&optional (depth 1))
  (poisson-forms (*limit-of-list-elts* *list-decay*)
    (%random-data-expr (1- depth))))

(defparameter *limit-of-cond-clauses* 10)
(defparameter *cond-decay* 0.6)

(defun discrete-poisson (total decay)
  (labels ((rec (num)
	     (if (equal total num)
		 num
		 (if-rand decay
			  (rec (1+ num))
			  num))))
    (rec 1)))

(defun random-cond-form ()
  `(cond
     ,@(poisson-forms (*limit-of-cond-clauses* *cond-decay*)
		      (list (random-expression) (random-expression)))))

(defparameter *limit-of-lambda-args* 5)
(defparameter *lambda-decay* 0.6)

(defparameter *limit-of-let-vars* 5)
(defparameter *let-decay* 0.6)

(defparameter *limit-of-progn-body* 10)
(defparameter *progn-decay* 0.6)

  

(defun random-lambda-form ()
  (let ((args (let ((*allow-undefined* t))
		(delete-duplicates (poisson-forms (*limit-of-lambda-args* *lambda-decay*)
				     (random-var-name))))))
    (let ((body (let ((*symbol-table* (cons args *symbol-table*)))
		  (poisson-forms (*limit-of-progn-body* *progn-decay*)
		      (random-expression)))))
      `(lambda ,args (declare (ignorable ,@args))
	       ,@body))))

(defun random-setf-form ()
  `(setf ,(random-var-name) ,(random-expression)))

(defun random-let-form ()
  (let ((vars (delete-duplicates (poisson-forms (*limit-of-let-vars* *let-decay*)
				   (list (let ((*allow-undefined* t))
					   (random-var-name))
					 (random-expression)))
				 :key #'car)))
    (let ((var-names (mapcar #'car vars)))
      (let ((body (let ((*symbol-table* (cons var-names
					      *symbol-table*)))
		    (poisson-forms (*limit-of-progn-body* *progn-decay*)
		      (random-expression)))))
	`(let ,vars (declare (ignorable ,@var-names))
	      ,@body)))))
  
(defun random-quote-form ()
  (let ((*allow-undefined* t))
    (declare (special *allow-undefined*))
    `(quote ,(random-expression))))

(let ((spec-ops '(:quote :cond :lambda :setf :let)))
  (defun random-special-operator ()
    (let ((it (elt spec-ops (random (length spec-ops)))))
      (funcall (symbol-function (intern #?"RANDOM-$(it)-FORM")))))
  (defun defined-special-operators ()
    (mapcar (lambda (x)
	      (intern (string x)))
	    spec-ops)))

(defparameter *funcall-prob* 0.5)

(defun random-cons-expression ()
  (handler-case (if-rand *funcall-prob*
			 (random-function-call)
			 (random-special-operator))
    (random-codegen-fail ()
      (random-cons-expression))))

(let ((psyms (delete-duplicates (append (mapcar #'car *vanilla-function-table*)
					(defined-special-operators)
					'(t nil)
					(iter (for i from 0 below *nvars*)
					      (collect (%random-var-name i)))))))
  (defun possible-symbols ()
    psyms))

(defun random-data-symbol ()
  (let ((syms (possible-symbols)))
    (if-rand 0.5
	     (if-rand 0.5
		      't
		      'nil)
	     (elt syms (random (length syms))))))

(defmacro with-muffled-style-warns (&body body)
  `(handler-bind
       ((alexandria::simple-warning
	 (lambda (warning)
	   (muffle-warning warning))))
     ,@body))

(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(defun lambda-compilable-p (expr)
  (handler-case (with-muffled-style-warns
		  (compile nil `(lambda ()
				  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
				  ,expr)))
    (error () nil)
    (:no-error (a b c) (declare (ignore b c))
	       a)))

(defun lambda-runnable-p (expr)
  (let ((it (lambda-compilable-p expr)))
    (if it
	(handler-case (funcall it)
	  (error () nil)
	  (:no-error (&rest vars) (declare (ignore vars)) t)))))
  
(defun cure-undefs (expr)
  (let ((it (find-undefs expr)))
    (let ((vars (cdr (assoc :variables it)))
	  (funcs (cdr (assoc :functions it))))
      `(let ,vars
	 (flet ,(mapcar (lambda (x)
			  `(,x (&rest args) (declare (ignore args)) nil))
			funcs)
	   ,expr)))))

(defun random-lambda-runnable-expression ()
  (let ((expr (cure-undefs (random-expression))))
    (iter (while (not (lambda-runnable-p expr)))
	  (setf expr (cure-undefs (random-expression))))
    expr))
    
(defun random-expression-with-constraints (&key constraints cure-undefs evaled-constraints)
  (let ((expr-gen (if cure-undefs
		      (lambda () (cure-undefs `(locally
						   (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
						 ,(random-expression))))
		      (lambda () `(locally
				      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
				    ,(random-expression))))))
    (let ((expr (funcall expr-gen)))
      (iter (while (not (block constraints
			  (and (iter (for constraint in constraints)
				     (if (not (funcall constraint expr))
					 (return nil))
				     (finally (return t)))
			       (let ((evaled-expr (with-muffled-style-warns
						    (handler-case (eval expr)
						      (error () (return-from constraints nil))))))
				 (iter (for constraint in evaled-constraints)
				       (if (not (funcall constraint evaled-expr))
					   (return nil))
				       (finally (return t))))))))
	    (for i from 1)
	    (when (equal 0 (mod i 1000))
	      (format t ".")
	      (setf i 0))
	    (setf expr (with-muffled-style-warns
			 (funcall expr-gen))))
      (format t "~%")
      (caddr expr))))

(defun simple-function (n)
  (lambda (fun)
    (and (functionp fun)
	 (equal n (length (sb-introspect:function-lambda-list fun))))))

(defun let-form-p (expr)
  (and (consp expr)
       (eq 'let (car expr))))

(defun universal-wildcard-p (sym)
  (and (symbolp sym)
       (equal "***" (string sym))))

(defmacro define-repeated-function-test (name &body body)
  (let ((elt-name (intern #?"ELT-$((string name))"))) 
    `(progn (defun ,elt-name (fun)
	      (macrolet ((failing-funcall (fun &rest its)
			   `(handler-case (funcall ,fun ,@its)
			      (error () (return-from ,',elt-name nil)))))
		,@body))
	    (defun ,name (n)
	      (lambda (fun)
		(iter (for i from 1 to n)
		      (if (not (,elt-name fun))
			  (return nil))
		      (finally (return t))))))))

(define-repeated-function-test not-function-test
  (let ((it (random-data-atom)))
    (let ((res (failing-funcall fun it)))
      (or (and it (not res))
	  (and (not it) res)))))

(define-repeated-function-test xor-function-test  (let ((it1 (random-data-atom))
	(it2 (random-data-atom)))
    (let ((res (failing-funcall fun it1 it2)))
      (cond ((and it1 it2) (not res))
	    ((and it1 (not it2)) res)
	    ((and (not it1) it2) res)
	    ((and (not it1) (not it2)) (not res))))))

    
(define-repeated-function-test *2-function-test
  (let ((it (random *n-ints*)))
    (let ((res (failing-funcall fun it)))
      (equal res (* it 2)))))
  
(defun frob ()
  (with-muffled-style-warns 
    (random-expression-with-constraints
     ; :constraints (list #'lambda-runnable-p) ; #'let-form-p)
     :evaled-constraints (list (simple-function 1)
			       (not-function-test 1000)))))

(defun frob-xor ()
  (with-muffled-style-warns 
    (random-expression-with-constraints
     ; :constraints (list #'lambda-runnable-p) ; #'let-form-p)
     :evaled-constraints (list (simple-function 2)
			       (xor-function-test 1000)))))


(defun frob ()
  (with-muffled-style-warns 
    (random-expression-with-constraints
     :constraints (list #'lambda-runnable-p) ; #'let-form-p)
     :evaled-constraints (list (simple-function 1) (not-function-test 1000)))))

(defun frob-1 ()
  (with-muffled-style-warns 
    (random-expression-with-constraints
     :constraints (list #'lambda-runnable-p) ; #'let-form-p)
     :evaled-constraints (list (simple-function 1) (*2-function-test 1000)))))

(defun pre-patternize-conses-of-same-length (conses)
  "Patternize each subexpression separately."
  (iter (for i from 0 below (length (cdar conses)))
	(collect (or (%patternize-code (mapcar (lambda (x)
						 (cons (car x)
						       (elt (cdr x) i)))
					       conses))
		     '***))))

(defun direct-product-respecting-wildcard (lst)
  (let (res)
    (labels ((rec (pre-patterns acc)
	       (if (not pre-patterns)
		   (push (reverse acc) res)
		   (if (universal-wildcard-p (car pre-patterns))
		       (rec (cdr pre-patterns) (cons '*** acc))
		       (iter (for elt in (car pre-patterns))
			     (rec (cdr pre-patterns) (cons elt acc)))))))
      (rec lst nil))
    res))

(defun grep-meaningful-patterns (patterns codes)
  (delete-duplicates
   (iter (for pattern in patterns)
	 (let ((affinity (/ (iter (for (id . cons) in codes)
				  (if (let ((*current-pattern-symbol-table*
					     (gethash id *pattern-symbol-tables*)))
					(code-matches-pattern-p pattern cons))
				      (summing 1)))
			    (* *num-sigmas* (sqrt (length codes))))))
	   (if (> affinity 1)
	       (collect pattern))))
   :test #'equal))


(defun grep-leaf-patterns (patterns)
  (let ((res (make-list (length patterns))))
    (iter (for pattern-1 on patterns)
	  (for pos-1 from 0)
	  (iter (for pattern-2 on (cdr pattern-1))
		(for pos-2 from (1+ pos-1))
		(let ((1-in-2 (code-matches-pattern-p (car pattern-2) (car pattern-1) t))
		      (2-in-1 (code-matches-pattern-p (car pattern-1) (car pattern-2) t)))
		  (cond ((and 1-in-2 (not 2-in-1)) (setf (elt res pos-2) t))
			((and 2-in-1 (not 1-in-2)) (setf (elt res pos-1) t))))))
    ;; (format t "Patterns ~a~%generals ~a~%" patterns res)
    (iter (for pattern in patterns)
	  (for general in res)
	  (if (not general)
	      (collect pattern)))))
	  
(defun patternize-conses-of-same-length (conses)
  (grep-leaf-patterns (grep-meaningful-patterns (direct-product-respecting-wildcard
						 (pre-patternize-conses-of-same-length conses))
						conses)))
      
	      ;; (format t "Pattern ~a matched ~a of ~a conses~%" pattern affinity (length conses))
	      ;; (if (equal pattern '(ignorable var-0))
	      ;; 	  (format t "Conses are ~a~%" conses)))))))

(defun patternize-conses (conses total)
  (let ((classes (make-hash-table :test #'equal)))
    (iter (for cons in conses)
	  (push cons (gethash (length (cdr cons)) classes)))
    (let ((it (iter (for (key val) in-hashtable classes)
		    (when (> (length val) (* *num-sigmas* (sqrt total)))
		      (appending (patternize-conses-of-same-length val))))))
      (if it
	  (cons '*** it)))))

(defparameter *pattern-symbol-tables* (make-hash-table :test #'equal))
(defparameter *current-pattern-symbol-table* nil)
	  
(defun patternize-code (codes)
  ;; (format t "In patternize code ~a~%" codes)
  (or (grep-leaf-patterns
       (let ((*pattern-symbol-tables* (make-hash-table :test #'equal)))
	 (%patternize-code (iter (for code in codes)
				 (for i from 1)
				 (setf (gethash i *pattern-symbol-tables*) (make-hash-table))
				 (collect `(,i . ,code))))))
      '***))

(defun variable-atom-p (code)
  (and (symbolp code)
       (m~ "^VAR-(\d+)$" (string code))
       (parse-integer $1)))

(defun translate-var-name (id code)
  (let ((*current-pattern-symbol-table* (gethash id *pattern-symbol-tables*)))
    (%translate-var-name code)))

(defun %translate-var-name (code)
  (if *current-pattern-symbol-table*
      (multiple-value-bind (item got) (gethash code *current-pattern-symbol-table*)
	(if got
	    item
	    (let ((it (setf (gethash code *current-pattern-symbol-table*)
			    (%random-var-name (hash-table-count *current-pattern-symbol-table*)))))
	      (format t "New translation: ~a goes to ~a~%" code it)
	      it)))
      code))

(defparameter *num-sigmas* 3)

(defun most-probable-var-names (vars)
  (let ((res (make-hash-table))
	(total (length vars)))
    (iter (for (id . var-name) in vars)
	  (incf (gethash var-name res 0)))
    (let ((it (iter (for (var-name count) in-hashtable res)
		    (if (> count (* *num-sigmas* (sqrt total)))
			(collect var-name)))))
      (if it
	  (cons '*** it)))))
	  
(defun %patternize-code (codes &optional (total (length codes)))
  (let ((classes (make-hash-table :test #'equal)))
    (iter (for (id . code) in codes)
	  (if (atom code)
	      (if (variable-atom-p code)
		  (push `(,id . ,(translate-var-name id code)) (gethash '*var* classes))
		  (push code (gethash code classes)))
	      (push `(,id . ,code) (gethash '*cons* classes))))
    (let ((it (iter (for (key val) in-hashtable classes)
		    (cond ((eq '*cons* key)
			   (appending (patternize-conses val total)))
			  ((eq '*var* key)
			   (appending (most-probable-var-names val)))
			  (t (when (> (length val) (* *num-sigmas* (sqrt total)))
			       (collect key)))))))
      (if it
	  (cons '*** it)))))
	      
(defun code-matches-pattern-p (pattern code &optional ignore-variability)
  (cond ((universal-wildcard-p pattern) t)
	((atom pattern) (if (and (not ignore-variability)
				 (variable-atom-p pattern))
			    (if (variable-atom-p code)
				(equal pattern (%translate-var-name code)))
			    (equal pattern code)))
	(t (if (consp code)
	       (and (equal (length code) (length pattern))
		    (iter (for pattern-elt in pattern)
			  (for code-elt in code)
			  (if (not (code-matches-pattern-p pattern-elt code-elt ignore-variability))
			      (return-from code-matches-pattern-p nil))
			  (finally (return t))))))))
	       
	      
(defun frob-2 ()
  (iter (for i from 1 to 10)
	(collect (frob))
	(format t "   *** Found ~a!~%" i)))

(defun scan-pattern-for-wildcards (pattern)
  (if (universal-wildcard-p pattern)
      '***
      (let (res)
	(labels ((rec (tree)
		   (if (not (atom tree))
		       (iter (for potential-place on tree)
			     (when (universal-wildcard-p (car potential-place))
			       (push potential-place res))
			     (rec (car potential-place))))))
	  (rec pattern)
	  res))))

(defun mk-code-generator-from-pattern (pattern elt-code-generator)
  (let ((my-pattern (copy-tree pattern)))
    (let ((places (scan-pattern-for-wildcards my-pattern)))
      (if (universal-wildcard-p places)
	  (lambda ()
	    (funcall elt-code-generator))
	  (lambda ()
	    (iter (for place in places)
		  (setf (car place) (funcall elt-code-generator)))
	    (copy-tree my-pattern))))))
