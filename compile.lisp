;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minerva Scheme                 ;;;
;;; 2018-2020 (c) Yaroslav Khnygin ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	     (transform-annotate-lambda
	      (transform-assignment
	       (transform-alpha-conversion x)))))))))
    (emit-labels transformed-program si)
    (restore-registers)
    (emit "ret")))

(defmacro with-output-to-file (file &body body)
  `(with-open-file (*compile-output* ,file :direction :output)
     (write-preamble)
     ,@body))

(defun compile-payload (input output)
  (with-output-to-file (make-pathname :type "s" :defaults output) (compile-program input)))

(defun compile-executable (output)
  (uiop:run-program (list "gcc" (file-namestring (make-pathname :type "s" :defaults output)) "runtime.c" "-m32" "-o" (pathname-name output)) :directory (make-pathname :name nil :type nil :defaults output)))

(defun compile-scheme-string (input output)
  (compile-payload input output)
  (compile-executable output))

(defun write-preamble ()
  (emit ".text")
  (emit ".p2align 2,,3")
  #+win32
  (emit ".globl	_scheme_entry")
  #+linux
  (emit ".globl scheme_entry")
  #+win32
  (emit "_scheme_entry:")
  #+linux
  (emit "scheme_entry:")
  (save-registers)
  (emit "movl 4(%esp), %eax")
  (emit "movl 0(%eax), %esi")
  (emit "movl 4(%eax), %ebp"))







