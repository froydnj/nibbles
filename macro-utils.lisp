;;;; macro-utils.lisp -- functions for compile-time macros

(cl:in-package :nibbles)

(defun byte-fun-name (bitsize signedp big-endian-p desc)
  (intern (format nil "~:[U~;S~]B~D~A/~:[LE~;BE~]"
                  signedp bitsize desc big-endian-p)))

(defun byte-ref-fun-name (bitsize signedp big-endian-p)
  (byte-fun-name bitsize signedp big-endian-p "REF"))

(defun byte-set-fun-name (bitsize signedp big-endian-p)
  (byte-fun-name bitsize signedp big-endian-p "SET"))

(defun stream-ref-fun-name (bitsize readp signedp big-endian-p)
  (intern (format nil "~:[WRITE~;READ~]-~:[U~;S~]B~D/~:[LE~;BE~]"
                  readp signedp bitsize big-endian-p)))
