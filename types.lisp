;;;; types.lisp -- various useful types

(cl:in-package :nibbles)

(deftype index () '(mod #.array-dimension-limit))

(deftype octet-vector (&optional (length '*))
  `(array (unsigned-byte 8) (,length)))

(deftype simple-octet-vector (&optional (length '*))
  #+(or sbcl cmu) `(simple-array (unsigned-byte 8) (,length))
  #-(or sbcl cmu) `(array (unsigned-byte 8) (,length)))
