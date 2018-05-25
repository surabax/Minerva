(in-package :minerva)

;; Current order of transforms:
;; 1. transform-alpha-conversion
;; 2. transform-assignment
;; 3. transform-annotate-lambda
;; 4. transform-funcall-to-tailcall
;; 5. transform-tailcall-register-guard
;; 6. transform-lambda-to-closure
;; 7. transform-quote-to-constant
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
		    (let ((freevars nil) (formals formals))
		      (values
		       `(let
			    ,(loop for (v expr) in (let-bindings x) collect
								    (multiple-value-bind (te fv)
									(transform-annotate-lambda-1 expr formals)
								      (setf freevars (append freevars fv))
								      (setf formals (cons v formals))
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
	    (let ((old-env env))
	    `(let
		 ,(loop for (v b) in (let-bindings x)
			do (setf env (acons v (gensym) env))
			collect (list (cdr (assoc v env)) (transform b old-env)))
	       ,@(loop for e in (let-body x)
		       collect (transform e env)))))
	   ((and (listp x) (not (null x))) ; TODO redo this after adding macro-expander transforms:
	    `(,(car x) ; TODO first element should be processed like every other one
	      ,@(loop for e in (cdr x)
		      collect (transform e env))))
	   (t
x	    x))))
    (transform x nil)))

(defun mklist (x)
  (if (listp x) x (list x)))

(defun flatten (ls)
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls))

(defun transform-assignment (x)
  (labels
      ((find-set! (x var)
	 (let ((fx (flatten x)))
	   (loop for i from 0 to (1- (length fx))
		 for e in fx do
		   (when (and
			  (eql e 'set!)
			  (eql (nth (1+ i) fx) var))
		     (return t)))))
       (replace-set-get (x vars)
	 (cond
	   ((and (variablep x) (member x vars))
	    `(vector-ref ,x 0))
	   ((set!-p x)
	    (if (member (second x) vars)
		`(vector-set! ,(second x) 0 ,(replace-set-get (third x) vars))
		x))
	   ((quotep x)
	    x)
	   ((not (atom x))
	    (mapcar #'(lambda (y) (replace-set-get y vars)) x))
	   (t
	    x)))
       (wrap-in-let (x aliases)
	 (if aliases
	     (list (wrap-in-let `(let ((,(car (car aliases)) (vector ,(cdr (car aliases))))) ,@x) (cdr aliases)))
	     x)))
    (cond
      ((lambdap x)
       (let* ((assignables (remove-if-not #'(lambda (v) (find-set! (lambda-body x) v)) (lambda-formalvars x)))
	      (new-body (replace-set-get (lambda-body x) assignables))
	      (aliases (mapcar #'(lambda (x) (cons x (gensym))) assignables)))
	 `(lambda
	    ,(mapcar
	      #'(lambda (v)
		  (let ((a (assoc v aliases)))
		    (if a (cdr a) v)))
	      (lambda-formalvars x))
	    ,@(wrap-in-let (transform-assignment new-body) aliases))))
      ((letp x)
       (let*
	   ((bound-vars (mapcar #'car (let-bindings x)))
	    (new-bindings (mapcar #'transform-assignment (mapcar #'cadr (let-bindings x))))
	    (assignables (remove-if-not #'(lambda (v) (find-set! (let-body x) v)) bound-vars))
	    (new-body (replace-set-get (let-body x) assignables))
	    (aliases (mapcar #'(lambda (x) (cons x (gensym))) assignables)))
	 `(let
	      ,(loop for v in bound-vars
		     for b in new-bindings
		     collect (list
			      (let ((a (assoc v aliases)))
				(if a (cdr a) v))
			      b))
	    ,@(wrap-in-let (transform-assignment new-body) aliases))))
      ((quotep x)
       x)
      ((listp x)
       (mapcar #'transform-assignment x))
      (t
       x))))
	 
	 
