(in-package :minerva)

;; Current order of transforms:
;; 1. transform-annotate-lambda
;; 2. transform-funcall-to-tailcall
;; 3. transform-tailcall-register-guard
;; 4. transform-lambda-to-closure
;; 5. transform-quote-to-constant
;; N.B. Transforms will break if called in wrong order.

(defun transform-annotate-lambda (x)
  (labels ((transform-annotate-lambda-1 (x formals)
	     (cond ((immediatep x)
		    (values x nil))
		   ((variablep x)
		    (values x (unless (member x formals) (list x))))
		   ((quotep x)
		    (values x nil))
		   ((lambdap x)
		    (let ((new-body nil) (body-freevars nil))
		      (loop for e in (lambda-body x) do
			   (multiple-value-bind (new-e e-freevars)
			       (transform-annotate-lambda-1 e (lambda-formalvars x))
			     (push new-e new-body)
			     (setf body-freevars (union body-freevars e-freevars))))
		      (let ((lambda-freevars (remove-if (lambda (v) (member v (lambda-formalvars x))) body-freevars)))
			(values `(lambda ,(lambda-formalvars x) ,lambda-freevars ,@(reverse new-body)) body-freevars))))
		    ((letp x)
		    (let ((freevars nil))
		      (values
		       `(let
			    ,(loop for (v expr) in (let-bindings x) collect
				  (multiple-value-bind (te fv)
				      (transform-annotate-lambda-1 expr formals)
				    (setf freevars (append freevars fv))
				    (list v te)))
			  ,@(loop for e in (let-body x) collect
				 (multiple-value-bind (te fv)
				     (transform-annotate-lambda-1 e formals)
				   (setf freevars (union freevars fv))
				   te)))
		       freevars)))
		   ((listp x)
		    (let ((freevars nil))
		      (values
		       `(,(first x) ,@(loop for e in (rest x) collect
					   (multiple-value-bind (element fv)
					       (transform-annotate-lambda-1 e formals)
					     (setf freevars (append freevars fv))
					     element)))
		       freevars))))))
    (transform-annotate-lambda-1 x nil)))
	   
(defun transform-lambda-to-closure (x)
  (let ((lexprs nil))
    (labels
	((transform-lambda-to-closure-1 (x)
	   (cond ((or (immediatep x) (variablep x) (quotep x))
		  x)
		 ((lambdap x)
		  (let ((label (gensym))
			(transformed-body (mapcar #'transform-lambda-to-closure-1 (lambda-annotated-body x))))
		    (push `(,label (code ,(lambda-annotated-formalvars x) ,(lambda-annotated-freevars x) ,@transformed-body)) lexprs)
		    `(closure ,label ,@(lambda-annotated-freevars x))))
		 ((listp x)
		  (loop for e in x collect (transform-lambda-to-closure-1 e))))))
      (let
	  ((labels-body (transform-lambda-to-closure-1 x)))
	`(labels ,lexprs ,labels-body)))))

(defun transform-funcall-to-tailcall (x)
  (cond ((funcallp x)
	 `(tailcall ,(funcall-closure x) ,@(funcall-args x)))
	((lambdap x)
	 `(lambda ,(lambda-annotated-formalvars x) ,(lambda-annotated-freevars x)
	    ,@(butlast (lambda-annotated-body x))
	    ,(transform-funcall-to-tailcall (car (last (lambda-annotated-body x))))))
	((letp x)
	 `(let
	      ,(loop for (var val) in (let-bindings x) collect
		    (list var (transform-funcall-to-tailcall val)))
	    ,@(butlast (let-body x)) ,(transform-funcall-to-tailcall (car (last (let-body x))))))
	((if-p x)
	 `(if ,(if-test x) ,(transform-funcall-to-tailcall (if-conseq x)) ,(transform-funcall-to-tailcall (if-altern x))))
	(t
	 x)))
    
(defun transform-quote-to-constant (x)
  (let ((constants nil))
    (destructuring-bind (lexprs . body) (rest x)
      (labels
	  ((lift-quotes (x)
	     (cond
	       ((quotep x)
		(let ((constant-label (gensym)))
		  (push (cons constant-label (cadr x)) constants)
		  `(constant-ref ,constant-label)))
	       ((listp x)
		(loop for e in x
		   collect (lift-quotes e)))
	       (t
		x)))
	   (prepare-init (x)
	     (cond
	       ((stringp x)
		`(string ,@(loop for i across x collect i)))
	       ((immediatep x)
		x)
	       ((consp x)
		(if (listp (cdr x))
		    `(list ,@(loop for i in x collect (prepare-init i)))
		    `(cons ,(prepare-init (car x)) ,(prepare-init (cdr x)))))
	       (t
		(error "Don't know how to quote this: ~a" x)))))
	(let ((new-body (mapcar #'lift-quotes body))
	      (new-lexprs (mapcar #'lift-quotes lexprs)))
	  `(labels
	       ,new-lexprs
	     ,@(append
		 (loop for (label . value) in constants
		    collect (list 'constant-init label (prepare-init value)))
		 new-body)))))))

;; not sure if this is the best solution, but it works and it's simple
;; could possibly interfere with top-level define
;; maybe funcall should be replaced here with a special form ("top-level" or "tailcall-register-guard") that works like funcall
(defun transform-tailcall-register-guard (x)
  `(funcall (lambda () () ,x)))

;; TODO add handling of cases like (let ((lambda (lambda (x) (* x x)))) (lambda 3))
(defun transform-alpha-conversion (x)
  (labels
      ((transform (x env)
	 (cond
	   ((variablep x)
	    (let ((alias (cdr (assoc x env))))
	      (or alias (error "Unbound variable: ~a" x))))
	   ((quotep x)
	    x)
	   ((lambdap x)
	    `(lambda
	       ,(loop for v in (lambda-formalvars x)
		      do (setf env (acons v (gensym) env))
		      collect (cdr (assoc v env)))
	       ,@(loop for e in (lambda-body x)
		       collect (transform e env))))
	   ((letp x)
	    `(let
		 ,(loop for (v b) in (let-bindings x)
			do (setf env (acons v (gensym) env))
			collect (list (cdr (assoc v env)) (transform b env)))
	       ,@(loop for e in (let-body x)
		       collect (transform e env))))
	   ((and (listp x) (not (null x)))
	    `(,(car x)
	      ,@(loop for e in (cdr x)
		      collect (transform e env))))
	   (t
	    x))))
    (transform x nil)))

(defun flatten (ls)
  (labels ((mklist (x) (if (listp x) x (list x))))
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (temp-flatten x))) ls)))

;; TODO find-set -> replace-set-get -> reconstruct-form
;; TODO find-set: flatten the input, then check every occurence of set! for formal variables
(defun transform-assignment (x)
  x)
