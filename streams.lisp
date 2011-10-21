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

(declaim (inline read-into-vector*))
(defun read-into-vector* (stream vector start end n-bytes reffer)
  (declare (type function reffer))
  (let ((v (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent v))
    (loop for i from start below end
	  do (read-n-bytes-into stream n-bytes v)
	     (setf (aref vector i) (funcall reffer v 0))
	  finally (return vector))))

(defun read-into-list* (stream list start end n-bytes reffer)
  (declare (type function reffer))
  (do ((end (or end) (length list))
       (v (make-array n-bytes :element-type '(unsigned-byte 8)))
       (rem (nthcdr start list) (rest rem))
       (i start (1+ i)))
      ((or (endp rem) (>= i end)) list)
    (declare (dynamic-extent v))
    (read-n-bytes-into stream n-bytes v)
    (setf (first rem) (funcall reffer v 0))))

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
	for byte-arglist = (if readp '(stream) '(integer stream))
	for subfun = (if readp 'read-byte* 'write-byte*)
	for element-type = `(,(if signedp 'signed-byte 'unsigned-byte) ,bitsize)
        collect `(defun ,name ,byte-arglist
		   (,subfun ,@byte-arglist ,n-bytes #',byte-fun)) into forms
	if readp
	  collect `(defun ,(stream-seq-fun-name bitsize t signedp big-endian-p)
		       (result-type stream count)
		     ,(format-docstring "Return a sequence of type RESULT-TYPE, containing COUNT elements read from STREAM.  Each element is a ~D-bit ~:[un~;~]signed integer read in ~:[little~;big~]-endian order.  RESULT-TYPE must be either CL:VECTOR or CL:LIST.  STREAM must have an element type of (UNSIGNED-BYTE 8)."
					bitsize signedp big-endian-p)
		     (case result-type
		       (list
			(let ((list (make-list count)))
			  (read-into-list* stream list 0 count
					   ,n-bytes #',byte-fun)))
		       (vector
			(let ((vector (make-array count
						  :element-type ',element-type)))
			  (read-into-vector* stream vector 0 count
					     ,n-bytes #',byte-fun))))) into forms
	else
	  collect `(defun ,(stream-seq-fun-name bitsize nil signedp big-endian-p)
		       (seq stream &key (start 0) end)
		     ,(format-docstring "Write elements from SEQ between START and END as ~D-bit ~:[un~;~]signed integers in ~:[little~;big~]-endian order to STREAM.  SEQ may be either a vector or a list.  STREAM must have an element type of (UNSIGNED-BYTE 8)."
					bitsize signedp big-endian-p)
		     (etypecase seq
		       (list
			(mapc (lambda (e) (,name e stream))
			      (subseq seq start end))
			seq)
		       (vector
			(loop with end = (or end (length seq))
			      for i from start below end 
			      do (,name (aref seq i) stream)
			      finally (return seq))))) into forms
	if readp
	  collect `(defun ,(intern (format nil "READ-~:[U~;S~]B~D/~:[LE~;BE~]-INTO-SEQUENCE"
					   signedp bitsize big-endian-p))
		       (seq stream &key (start 0) end)
		     ,(format-docstring "Destructively modifies SEQ by replacing the elements of SEQ between START and END with elements read from STREAM.  Each element is a ~D-bit ~:[un~;~]signed integer written in ~:[little~;big~]-endian order.  SEQ may be either a vector or a list.  STREAM must have an element type of (UNSIGNED-BYTE 8)."
					bitsize signedp big-endian-p)
		     (etypecase seq
		       (list (read-into-list* stream seq start end
					      ,n-bytes #',byte-fun))
		       (vector
			(let ((end (or end (length seq))))
			  (read-into-vector* stream seq start end
					     ,n-bytes #',byte-fun))))) into forms
        finally (return `(progn ,@forms)))
