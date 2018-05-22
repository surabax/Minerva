(in-package :minerva)

;;; Scheme's boolean, implemented in CL for convenience.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass bool ()
    ((value :reader value :initarg :value :initform (error "Must define value."))))
  
  (defmethod print-object ((self bool) stream)
    (format stream "~:[#f~;#t~]" (value self)))

  (defun boolp (x)
    (eql (class-of x) (find-class 'bool)))

  (defparameter *false* (make-instance 'bool :value nil))
  (defparameter *true*  (make-instance 'bool :value t))

  (set-dispatch-macro-character #\# #\f
				(lambda (dc sc infix)
				  (declare (ignore dc sc infix))
				  *false*))
  (set-dispatch-macro-character #\# #\t
				(lambda (dc sc infix)
				  (declare (ignore dc sc infix))
				  *true*))
  
  (defmethod make-load-form ((self bool) &optional environment)
    (declare (ignore environment))
    (if (value self)
	`*true*
	`*false*)))
