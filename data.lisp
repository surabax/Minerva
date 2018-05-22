(in-package :minerva)

(defconstant +word-size+       4) ; octets

(defconstant +tag-integer+     #b00)
(defconstant +tag-bool+        #b0011111)
(defconstant +tag-character+   #b00001111)
(defconstant +tag-nil+         #b00101111)

(defconstant +tag-pair+        #b001)
(defconstant +tag-vector+      #b010)
(defconstant +tag-string+      #b011)
(defconstant +tag-symbol+      #b101)
(defconstant +tag-closure+     #b110)

(defconstant +mask-integer+    #b11)
(defconstant +mask-bool+       #b1111111)

(defconstant +shift-integer+   2)
(defconstant +shift-character+ 8)
(defconstant +shift-bool+      7)

(defun immediate-rep (x)
  (cond ((integerp x) (encode-integer x))
	((characterp x) (encode-character x))
	((boolp x) (encode-bool x))
	((null x) (encode-nil))))

(defun encode-integer (x)
  (let ((int 0))
    (setf (ldb (byte +shift-integer+ 0) int) +tag-integer+)
    (setf (ldb (byte 30 +shift-integer+) int) x)
    int))

(defun encode-character (x)
  (let ((char 0))
    (setf (ldb (byte +shift-character+ 0) char) +tag-character+)
    (setf (ldb (byte 24 +shift-character+) char) (char-code x))
    char))

(defun encode-bool (x)
  (let ((bool 0))
    (setf (ldb (byte +shift-bool+ 0) bool) +tag-bool+)
    (setf (ldb (byte 1 +shift-bool+) bool) (if (value x) 1 0))
    bool))

(defun encode-nil ()
  +tag-nil+)
