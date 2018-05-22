(in-package :minerva)

(defun labels-p (x)
  (and (listp x) (eql (first x) 'labels)))

(defun lexpr-code-p (x)
  (eql (car x) 'code))

(defun funcallp (x)
  (and (listp x) (eql (first x) 'funcall)))

(defun tailcallp (x)
  (and (listp x) (eql (first x) 'tailcall)))

(defun closurep (x)
  (and (listp x) (eql (first x) 'closure)))

(defun lambdap (x)
  (and (listp x) (eql (first x) 'lambda)))

(defun quotep (x)
  (and (listp x) (eql (first x) 'quote)))

(defun constant-init-p (x)
  (and (listp x) (eql (car x) 'constant-init)))

(defun constant-ref-p (x)
  (and (listp x) (eql (car x) 'constant-ref)))

(defun if-p (x)
  (and (listp x) (eql (first x) 'if)))

(defun immediatep (x)
  (or (integerp x) (characterp x) (boolp x) (null x)))

(defun variablep (x)
  (and (symbolp x) (not (null x))))

(defun closed-variable-p (x env)
  (> (lookup x env) 0))

(defun letp (x)
  (and (listp x) (eql (first x) 'let)))

(defun primcallp (x)
  (and (listp x) (not (null x))))
