(in-package :minerva)

(defun emit-labels (x si)
  (let ((entry (unique-label)))
    (emit "jmp ~a" entry)
    (let ((env (process-lvars (labels-lvars x))))
      (emit "~a:" entry)
      (loop for e in (labels-expr x) do
	   (emit-expr e si env)))))

(defun labels-lvars (x)
  (second x))

(defun labels-expr (x)
  (rest (rest x)))

(defun process-lvars (lvars)
  (let ((env nil))
    (loop for (lvar nil) in lvars do
	 (push (cons lvar (unique-label)) env))
    (loop for (lvar lexpr) in lvars do
	 (when (lexpr-code-p lexpr)
	   (emit "~a:" (lookup lvar env))
	   (emit-lexpr-code lexpr env)))
    env))

(defun lexpr-formalvars (x)
  (second x))

(defun lexpr-freevars (x)
  (third x))
  
(defun lexpr-expr (x)
  (rest (rest (rest x))))

(defun emit-lexpr-code (lexpr env)
  (let* ((new-env (prepare-lexpr-formalvars (lexpr-formalvars lexpr) (prepare-lexpr-freevars (lexpr-freevars lexpr) env)))
	 (new-si (prepare-lexpr-si new-env)))
    (loop for e in (lexpr-expr lexpr) do (emit-expr e new-si new-env))
    (emit "ret")))

(defun prepare-lexpr-formalvars (vars env)
  (loop for i = (- +word-size+) then (- i +word-size+)
     for v in vars do
       (push (cons v i) env))
  env)

(defun prepare-lexpr-freevars (vars env)
  (loop for i = +word-size+ then (+ i +word-size+)
     for v in vars do
       (push (cons v i) env))
  env)

(defun prepare-lexpr-si (env)
  (if (or (not (numberp (cdar env))) (closed-variable-p (caar env) env))
      (- +word-size+)
      (- (cdar env) +word-size+)))

(defun funcall-closure (x)
  (second x))

(defun funcall-args (x)
  (rest (rest x)))

(defun closure-lvar (x)
  (second x))

(defun closure-values (x)
  (rest (rest x)))

(defun lambda-formalvars (x)
  (second x))

(defun lambda-body (x)
  (rest (rest x)))

(defun lambda-annotated-formalvars (x)
  (second x))

(defun lambda-annotated-freevars (x)
  (third x))

(defun lambda-annotated-body (x)
  (rest (rest (rest x))))

(defun emit-closure (x si env)
  (emit "movl $~a, %ebx" (lookup (closure-lvar x) env))
  (emit "movl %ebx, 0(%esi)")
  (loop for v in (closure-values x)
     for i = +word-size+ then (+ i +word-size+) do
       (emit-expr v si env)
       (emit "movl %eax, ~a(%esi)" i))
  (emit "movl %esi, %eax")
  (emit "orl $~a, %eax" +tag-closure+)
  (emit "movl $~a, %ebx" (* (1- (length x)) +word-size+))
  (emit "addl $11, %ebx")
  (emit "andl $-8, %ebx")
  (emit "addl %ebx, %esi"))
	
(defun emit-funcall (x si env)
  (let ((new-si (- si (* +word-size+ 2))))
    (loop for arg in (funcall-args x)
       for arg-si = new-si then (- arg-si +word-size+) do
	 (emit-expr arg arg-si env)
	 (emit "movl %eax, ~a(%esp)" arg-si))
    (emit "movl %edi, ~a(%esp)" si)
    (emit-expr (funcall-closure x) si env)
    (emit "xorl $~a, %eax" +tag-closure+)
    (emit "movl %eax, %edi")
    (emit "addl $~a, %esp" si)
    (emit "call 0(%edi)")
    (emit "subl $~a, %esp" si)
    (emit "movl ~a(%esp), %edi" si)))

(defun emit-tailcall (x si env)
  (loop for arg in (funcall-args x)
     for arg-si = si then (- arg-si +word-size+) do
       (emit-expr arg arg-si env)
       (emit "movl %eax, ~a(%esp)" arg-si))
  (emit-expr (funcall-closure x) si env)
  (emit "xorl $~a, %eax" +tag-closure+)
  (emit "movl %eax, %edi")
  (loop for arg in (funcall-args x)
     for si-shift = 0 then (+ si-shift +word-size+) do
       (emit "movl ~a(%esp), %eax" (- si si-shift))
       (emit "movl %eax, ~a(%esp)" (- (- +word-size+) si-shift)))
  (emit "jmp 0(%edi)"))

(defun emit-constant-init (x si env)
  (destructuring-bind (const value) (rest x)
    (emit ".lcomm ~a,~a" const +word-size+) ; not sure if it's going to work with ELF targets
    (emit-expr value si env)
    (emit "movl %eax, (%ebp)")
    (emit "movl %ebp, %eax")
    (emit "addl $~a, %ebp" +word-size+)
    (emit "movl %eax, ~a" const)
    (emit "movl $0, %eax")))

(defun emit-constant-ref (x si env)
  (declare (ignore si env))
  (let ((const (cadr x)))
    (emit "movl ~a, %eax" const)
    (emit "movl (%eax), %eax")))

(defun emit-expr (x si env)
  (cond ((immediatep x)
	 (emit "movl $~a, %eax" (immediate-rep x)))
	((variablep x)
	 (emit-variable x env))
	((letp x)
	 (emit-let (let-bindings x) (let-body x) si env))
	((if-p x)
	 (emit-if (if-test x) (if-conseq x) (if-altern x) si env))
	((closurep x)
	 (emit-closure x si env))
	((funcallp x)
	 (emit-funcall x si env))
	((tailcallp x)
	 (emit-tailcall x si env))
	((constant-init-p x)
	 (emit-constant-init x si env))
	((constant-ref-p x)
	 (emit-constant-ref x si env))
	((primcallp x)
	 (emit-primitive-call x si env))))

(defun emit-variable (x env)
  (let ((i (lookup x env)))
    (if (closed-variable-p x env)
      (emit "movl ~a(%edi), %eax" i)
      (emit "movl ~a(%esp), %eax" i))))

(defun emit-let (bindings body si env)
  (labels
      ((emit-let-1 (b* new-env si)
	 (if (null b*)
	     (loop for e in body do (emit-expr e si new-env))
	     (let ((b (car b*)))
	       (emit-expr (second b) si env)
	       (emit "movl %eax, ~a(%esp)" si)
	       (emit-let-1 (cdr b*)
			   (extend-env (first b) si new-env)
			   (- si +word-size+))))))
    (emit-let-1 bindings env si)))

(defun emit-if (test conseq altern si env)
  (let
      ((L0 (unique-label))
       (L1 (unique-label)))
    (emit-expr test si env)
    (emit "cmpl $~a, %eax" (immediate-rep #f))
    (emit "je ~a" L0)
    (emit-expr conseq si env)
    (emit "jmp ~a" L1)
    (emit "~a:" L0)
    (emit-expr altern si env)
    (emit "~a:" L1)))

(defun if-test (x)
  (second x))

(defun if-conseq (x)
  (third x))

(defun if-altern (x)
  (fourth x))

(defun lookup (x env)
  (cdr (assoc x env)))

(defun extend-env (symbol si env)
  (cons (cons symbol si) env))

(defun unique-label ()
  (symbol-name (gensym)))
	    
(defun let-bindings (x)
  (second x))

(defun let-body (x)
  (cdr (cdr x)))
