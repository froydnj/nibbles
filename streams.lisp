;;;; streams.lisp -- reading/writing signed/unsigned bytes to streams

(cl:in-package :nibbles)

(defun read-n-bytes-into (stream n-bytes v)
  (dotimes (i n-bytes v)
    ;; READ-SEQUENCE would likely be more efficient here, but it does
    ;; not have the semantics we want--in particular, the blocking
    ;; semantics of READ-SEQUENCE are potentially bad.  It's not clear
    ;; that READ-BYTE is any better here, though...
    (setf (aref v i) (read-byte stream))))

(declaim (inline read-byte* write-byte*))
(defun read-byte* (stream n-bytes reffer)
  (let ((v (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent v))
    (read-n-bytes-into stream n-bytes v)
    (funcall reffer v 0)))

(defun write-byte* (integer stream n-bytes setter)
  (let ((v (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent v))
    (funcall setter v 0 integer)
    (write-sequence v stream)
    integer))

#.(loop for i from 0 upto #b10111
        for bitsize = (ecase (ldb (byte 2 3) i)
                        (0 16)
                        (1 32)
                        (2 64))
        for readp = (logbitp 2 i)
        for signedp = (logbitp 1 i)
        for big-endian-p = (logbitp 0 i)
	for name = (stream-ref-fun-name bitsize readp signedp big-endian-p)
	for n-bytes = (truncate bitsize 8)
	for byte-fun = (if readp
			   (byte-ref-fun-name bitsize signedp big-endian-p)
			   (byte-set-fun-name bitsize signedp big-endian-p))
	for arglist = (if readp '(stream) '(integer stream))
	for subfun = (if readp 'read-byte* 'write-byte*)
        collect `(defun ,name ,arglist
		   (,subfun ,@arglist ,n-bytes #',byte-fun)) into forms
        finally (return `(progn ,@forms)))
