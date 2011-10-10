;;;; macro-utils.lisp -- functions for compile-time macros

(cl:in-package :nibbles)

(defun byte-fun-name (bitsize signedp big-endian-p desc)
  (let ((*package* (find-package :nibbles)))
    (intern (format nil "~:[U~;S~]B~D~A/~:[LE~;BE~]"
                    signedp bitsize desc big-endian-p))))

(defun byte-ref-fun-name (bitsize signedp big-endian-p)
  (byte-fun-name bitsize signedp big-endian-p "REF"))

(defun byte-set-fun-name (bitsize signedp big-endian-p)
  (byte-fun-name bitsize signedp big-endian-p "SET"))

(defun stream-ref-fun-name (bitsize readp signedp big-endian-p)
  (let ((*package* (find-package :nibbles)))
    (intern (format nil "~:[WRITE~;READ~]-~:[U~;S~]B~D/~:[LE~;BE~]"
                    readp signedp bitsize big-endian-p))))

(defun stream-seq-fun-name (bitsize readp signedp big-endian-p)
  (let ((*package* (find-package :nibbles)))
    (intern (format nil "~:[WRITE~;READ~]-~:[U~;S~]B~D/~:[LE~;BE~]-SEQUENCE"
		    readp signedp bitsize big-endian-p))))

(defun internalify (s)
  (let ((*package* (find-package :nibbles)))
    (intern (concatenate 'string "%" (string s)))))

(defun format-docstring (&rest args)
  (loop with docstring = (apply #'format nil args)
	for start = 0 then (when pos (1+ pos))
	for pos = (and start (position #\Space docstring :start start))
	while start
	collect (subseq docstring start pos) into words
	finally (return (format nil "~{~<~%~1,76:;~A~>~^ ~}"
				words))))
