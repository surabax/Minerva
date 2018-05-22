(in-package minerva)

(defparameter *compile-output* *standard-output*)

(defmacro emit (string &rest args)
  `(progn
     (format *compile-output* ,string ,@args)
     (fresh-line *compile-output*)))
