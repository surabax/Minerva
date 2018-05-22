;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minerva Scheme Compiler   ;;;
;;; 2018 (c) Yaroslav Khnygin ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :minerva)

(defparameter *saved-registers* (list "%ebx" "%ebp" "%esp" "%esi" "%edi"))

(defun save-registers ()
  (loop for r in *saved-registers*
     for i = (- +word-size+) then (- i +word-size+)
     do (emit "movl ~a, ~a(%esp)" r i)))

(defun restore-registers ()
  (loop for r in *saved-registers*
     for i = (- +word-size+) then (- i +word-size+)
       do (emit "movl ~a(%esp), ~a" i r)))

(defun compile-program (x) ; TODO add transforms to a list instead and make a function to apply everything from a list to x
  (let ((si
	 (- (* +word-size+ (1+ (length *saved-registers*)))))
	(transformed-program
	 (transform-quote-to-constant
	  (transform-lambda-to-closure
	   (transform-tailcall-register-guard
	    (transform-funcall-to-tailcall
	     (transform-annotate-lambda x)))))))
    (emit-labels transformed-program si)
    (restore-registers)
    (emit "ret")))

(defmacro with-output-to-file (file &body body)
  `(with-open-file (*compile-output* ,file :direction :output)
     (write-preamble)
     ,@body))

(defun write-preamble ()
  (emit ".text")
  (emit ".p2align 2,,3")
  (emit ".globl	_scheme_entry")
  (emit ".def	_scheme_entry;	.scl	2;	.type	32;	.endef")
  (emit "_scheme_entry:")
  (save-registers)
  (emit "movl 4(%esp), %eax")
  (emit "movl 0(%eax), %esi")
  (emit "movl 4(%eax), %ebp"))







