;;;; streams.lisp -- reading/writing signed/unsigned bytes to streams

(cl:in-package :nibbles)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun byte-ref-fun-name (bitsize signedp big-endian-p)
  (intern (format nil "~:[U~;S~]B~DREF/~:[LE~;BE~]"
                  signedp bitsize big-endian-p)))

(defun stream-ref-fun-name (bitsize readp signedp big-endian-p)
  (intern (format nil "~:[WRITE~;READ~]-~:[U~;S~]B~D/~:[LE~;BE~]"
                  readp signedp bitsize big-endian-p)))
) ; EVAL-WHEN

(declaim (inline read-byte* write-byte*))
(defun read-byte* (stream n-bytes reffer)
  (let ((v (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent v))
    ;; READ-SEQUENCE would likely be more efficient here, but it does
    ;; not have the semantics we want--in particular, the blocking
    ;; semantics of READ-SEQUENCE are potentially bad.  It's not clear
    ;; that READ-BYTE is any better here, though...
    (dotimes (i n-bytes (funcall reffer v 0))
      (setf (aref v i) (read-byte stream)))))

(defun write-byte* (integer stream n-bytes setter)
  (let ((v (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent v))
    (funcall setter integer v 0)
    (write-sequence v stream)
    integer))

#.(loop for bitsize in '(16 32 64)
        collect (loop for i from 0 below 8
                      for readp = (logbitp 2 i)
                      for signedp = (logbitp 1 i)
                      for big-endian-p = (logbitp 0 i)
                      collect (let* ((name (stream-ref-fun-name bitsize readp signedp big-endian-p))
                                     (n-bytes (truncate bitsize 8))
                                     (byte-ref (byte-ref-fun-name bitsize signedp big-endian-p))
                                     (accessor (if readp
                                                   byte-ref
                                                   `(setf ,byte-ref)))
                                     (arglist (if readp
                                                  '(stream)
                                                  '(integer stream)))
                                     (subfun (if readp
                                                 'read-byte*
                                                 'write-byte*)))
                                `(defun ,name ,arglist
                                   (,subfun ,@arglist ,n-bytes #',accessor))) into forms
                      finally (return `(progn ,@forms))) into forms
        finally (return `(progn ,@forms)))
