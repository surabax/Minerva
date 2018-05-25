(in-package :minerva)

(defun primcall-op (x)
  (first x))

(defun primcall-operand1 (x)
  (second x))

(defun primcall-operand2 (x)
  (third x))

(defun primcall-operand3 (x)
  (fourth x))

(defun emit-primitive-call (x si env)
  (case (primcall-op x)
    (add1
     (emit-add1 x si env))
    (integer->char
     (emit-integer->char x si env))
    (char->integer
     (emit-char->integer x si env))
    (zero?
     (emit-zero? x si env))
    (null?
     (emit-null? x si env))
    (not
     (emit-not x si env))
    (integer?
     (emit-integer? x si env))
    (boolean?
     (emit-boolean? x si env))
    (+
     (emit-+ x si env))
    (-
     (emit-- x si env))
    (*
     (emit-* x si env))
    (=
     (emit-= x si env))
    (>
     (emit-> x si env))
    (char=?
     (emit-char=? x si env))
    (cons
     (emit-cons x si env))
    (car
     (emit-car x si env))
    (cdr
     (emit-cdr x si env))
    (make-vector
     (emit-make-vector x si env))
    (vector-ref
     (emit-vector-ref x si env))
    (vector-set!
     (emit-vector-set! x si env))
    (vector
     (emit-vector x si env))
    (make-string
     (emit-make-string x si env))
    (string-ref
     (emit-string-ref x si env))
    (string-set!
     (emit-string-set! x si env))
    (string
     (emit-string x si env))
    (eq?
     (emit-eq? x si env))
    (t (error "Unknown primcall-op: ~a~%" (primcall-op x)))))

(defun emit-add1 (x si env)
  (emit-expr (primcall-operand1 x) si env)
  (emit "addl $~a, %eax" (immediate-rep 1)))
    
(defun emit-integer->char (x si env)
  (emit-expr (primcall-operand1 x) si env)
  (emit "sall $~a, %eax" (- +shift-character+ +shift-integer+))
  (emit "addl $~a, %eax" +tag-character+))

(defun emit-char->integer (x si env)
  (emit-expr (primcall-operand1 x) si env)
  (emit "sarl $~a, %eax" (- +shift-character+ +shift-integer+)))

(defun emit-zero? (x si env)
  (emit-expr (primcall-operand1 x) si env)
  (emit "cmpl $0, %eax")
  (emit "movl $0, %eax")
  (emit "sete %al")
  (emit "sall $~a, %eax" +shift-bool+)
  (emit "orl $~a, %eax" +tag-bool+))

(defun emit-null? (x si env)
  (emit-expr (primcall-operand1 x) si env)
  (emit "cmpl $~a, %eax" +tag-nil+)
  (emit "movl $0, %eax")
  (emit "sete %al")
  (emit "sall $~a, %eax" +shift-bool+)
  (emit "orl $~a, %eax" +tag-bool+))

(defun emit-not (x si env)
  (emit-expr (primcall-operand1 x) si env)
  (emit "cmpl $~a, %eax" (immediate-rep #f))
  (emit "movl $0, %eax")
  (emit "sete %al")
  (emit "sall $~a, %eax" +shift-bool+)
  (emit "orl $~a, %eax" +tag-bool+))

(defun emit-integer? (x si env)
  (emit-expr (primcall-operand1 x) si env)
  (emit "andl $~a, %eax" +mask-integer+)
  (emit "cmpl $~a, %eax" +tag-integer+)
  (emit "movl $0, %eax")
  (emit "sete %al")
  (emit "sall $~a, %eax" +shift-bool+)
  (emit "orl $~a, %eax" +tag-bool+))

(defun emit-boolean? (x si env)
  (emit-expr (primcall-operand1 x) si env)
  (emit "andl $~a, %eax" +mask-bool+)
  (emit "cmpl $~a, %eax" +tag-bool+)
  (emit "movl $0, %eax")
  (emit "sete %al")
  (emit "sall $~a, %eax" +shift-bool+)
  (emit "orl $~a, %eax" +tag-bool+))

(defun emit-+ (x si env)
  (emit-expr (primcall-operand2 x) si env)
  (emit "movl %eax, ~a(%esp)" si)
  (emit-expr
   (primcall-operand1 x)
   (- si +word-size+)
   env)
  (emit "addl ~a(%esp), %eax" si))

(defun emit-- (x si env)
  (emit-expr (primcall-operand2 x) si env)
  (emit "movl %eax, ~a(%esp)" si)
  (emit-expr
   (primcall-operand1 x)
   (- si +word-size+)
   env)
  (emit "subl ~a(%esp), %eax" si))

(defun emit-* (x si env)
  (emit-expr (primcall-operand2 x) si env)
  (emit "sarl $~a, %eax" +shift-integer+)
  (emit "movl %eax, ~a(%esp)" si)
  (emit-expr
   (primcall-operand1 x)
   (- si +word-size+)
   env)
  (emit "sarl $~a, %eax" +shift-integer+)
  (emit "mull ~a(%esp)" si)
  (emit "sall $~a, %eax" +shift-integer+))

(defun emit-= (x si env)
  (emit-expr (primcall-operand2 x) si env)
  (emit "movl %eax, ~a(%esp)" si)
  (emit-expr
   (primcall-operand1 x)
   (- si +word-size+)
   env)
  (emit "cmpl ~a(%esp), %eax" si)
  (emit "movl $0, %eax")
  (emit "sete %al")
  (emit "sall $~a, %eax" +shift-bool+)
  (emit "orl $~a, %eax" +tag-bool+))

(defun emit-> (x si env)
  (emit-expr (primcall-operand2 x) si env)
  (emit "movl %eax, ~a(%esp)" si)
  (emit-expr
   (primcall-operand1 x)
   (- si +word-size+)
   env)
  (emit "cmpl ~a(%esp), %eax" si)
  (emit "movl $0, %eax")
  (emit "setg %al")
  (emit "sall $~a, %eax" +shift-bool+)
  (emit "orl $~a, %eax" +tag-bool+))

(defun emit-char=? (x si env)
  (emit-expr (primcall-operand2 x) si env)
  (emit "sarl $~a, %eax" +shift-character+)
  (emit "movl %eax, ~a(%esp)" si)
  (emit-expr
   (primcall-operand1 x)
   (- si +word-size+)
   env)
  (emit "sarl $~a, %eax" +shift-character+)
  (emit "cmpl ~a(%esp), %eax" si)
  (emit "movl $0, %eax")
  (emit "sete %al")
  (emit "sall $~a, %eax" +shift-bool+)
  (emit "orl $~a, %eax" +tag-bool+))

(defun emit-cons (x si env)
  (emit-expr (primcall-operand2 x) si env)
  (emit "movl %eax, ~a(%esp)" si)
  (emit-expr
   (primcall-operand1 x)
   (- si +word-size+)
   env)
  (emit "movl %eax, 0(%esi)")
  (emit "movl ~a(%esp), %eax" si)
  (emit "movl %eax, 4(%esi)")
  (emit "movl %esi, %eax")
  (emit "orl $~a, %eax" +tag-pair+)
  (emit "addl $8, %esi"))

(defun emit-car (x si env)
  (emit-expr (primcall-operand1 x) si env)
  (emit "movl -1(%eax), %eax"))

(defun emit-cdr (x si env)
  (emit-expr (primcall-operand1 x) si env)
  (emit "movl 3(%eax), %eax"))

(defun emit-make-vector (x si env)
  (emit-expr (primcall-operand1 x) si env)
  (emit "sarl $~a, %eax" +shift-integer+)
  (emit "movl $~a, %ebx" +word-size+)
  (emit "mull %ebx")
  (emit "movl %eax, 0(%esi)")
  (emit "movl %eax, %ebx")
  (emit "movl %esi, %eax")
  (emit "orl $~a, %eax" +tag-vector+)
  (emit "addl $11, %ebx")
  (emit "andl $-8, %ebx")
  (emit "addl %ebx, %esi"))

(defun emit-vector-ref (x si env)
  (emit-expr (primcall-operand2 x) si env)
  (emit "movl %eax, ~a(%esp)" si)
  (emit-expr
   (primcall-operand1 x)
   (- si +word-size+)
   env)
  (emit "xorl $~a, %eax" +tag-vector+)
  (emit "movl ~a(%esp), %edx" si)
  (emit "sarl $~a, %edx" +shift-integer+)
  (emit "addl $1, %edx")
  (emit "movl (%eax,%edx,~a), %eax" +word-size+))
  
(defun emit-vector-set! (x si env)
  (emit-expr (primcall-operand3 x) si env)
  (emit "movl %eax, ~a(%esp)" si)
  (emit-expr
   (primcall-operand2 x)
   (- si +word-size+)
   env)
  (emit "movl %eax, ~a(%esp)" (- si +word-size+))
  (emit-expr
   (primcall-operand1 x)
   (- si (* +word-size+ 2))
   env)
  (emit "movl ~a(%esp), %edx" (- si +word-size+))
  (emit "movl ~a(%esp), %ebx" si)
  (emit "xorl $~a, %eax" +tag-vector+)
  (emit "sarl $~a, %edx" +shift-integer+)
  (emit "addl $1, %edx")
  (emit "movl %ebx, (%eax,%edx,~a)" +word-size+))

(defun emit-vector (x si env) ; TODO incomplete
  (emit-expr
   (let ((new-vector (gensym)))
     `(let ((,new-vector (make-vector 1))) (vector-set! ,new-vector 0 ,(primcall-operand1 x)) ,new-vector))
   si
   env))

(defun emit-make-string (x si env)
  (emit-expr (primcall-operand1 x) si env)
  (emit "sarl $~a, %eax" +shift-integer+)
  (emit "movl $~a, %ebx" +word-size+)
  (emit "mull %ebx")
  (emit "movl %eax, 0(%esi)")
  (emit "movl %eax, %ebx")
  (emit "movl %esi, %eax")
  (emit "orl $~a, %eax" +tag-string+)
  (emit "addl $11, %ebx")
  (emit "andl $-8, %ebx")
  (emit "addl %ebx, %esi"))

(defun emit-string-ref (x si env)
  (emit-expr (primcall-operand2 x) si env)
  (emit "movl %eax, ~a(%esp)" si)
  (emit-expr
   (primcall-operand1 x)
   (- si +word-size+)
   env)
  (emit "xorl $~a, %eax" +tag-string+)
  (emit "movl ~a(%esp), %edx" si)
  (emit "sarl $~a, %edx" +shift-integer+)
  (emit "addl $1, %edx")
  (emit "movl (%eax,%edx,~a), %eax" +word-size+)
  (emit "sall $~a, %eax" +shift-character+)
  (emit "orl $~a, %eax" +tag-character+))

(defun emit-string-set! (x si env)
  (emit-expr (primcall-operand3 x) si env)
  (emit "movl %eax, ~a(%esp)" si)
  (emit-expr
   (primcall-operand2 x)
   (- si +word-size+)
   env)
  (emit "movl %eax, ~a(%esp)" (- si +word-size+))
  (emit-expr
   (primcall-operand1 x)
   (- si (* +word-size+ 2))
   env)
  (emit "movl ~a(%esp), %edx" (- si +word-size+))
  (emit "movl ~a(%esp), %ebx" si)
  (emit "xorl $~a, %eax" +tag-string+)
  (emit "sarl $~a, %edx" +shift-integer+)
  (emit "addl $1, %edx")
  (emit "sarl $~a, %ebx" +shift-character+)
  (emit "movl %ebx, (%eax,%edx,~a)" +word-size+))

(defun emit-string (x si env) ; TODO temporarily takes only one argument, needs to be extended later
  (emit-expr
   (let ((new-string (gensym)))
     `(let ((,new-string (make-string 1))) (string-set! ,new-string 0 ,(primcall-operand1 x)) ,new-string))
   si
   env))

;; TODO implement emit-list

(defun emit-eq? (x si env)
  (emit-= x si env))
