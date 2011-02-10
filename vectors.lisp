;;;; vectors.lisp -- signed/unsigned byte accessors

(cl:in-package :nibbles)

(declaim (inline array-data-and-offsets))
(defun array-data-and-offsets (v start end)
  "Like ARRAY-DISPLACEMENT, only more useful."
  #+sbcl
  (sb-kernel:with-array-data ((v v) (start start) (end end))
    (values v start end))
  #+cmu
  (lisp::with-array-data ((v v) (start start) (end end))
    (values v start end))
  #-(or sbcl cmu)
  (values v start (or end (length v))))

(eval-when (:compile-toplevel :execute)
(defun ref-form (vector-name index-name byte-size signedp big-endian-p)
  "Return a form that fetches a SIGNEDP BYTE-SIZE value from VECTOR-NAME,
starting at INDEX-NAME.  The value is stored in the vector according to
BIG-ENDIAN-P."
  (multiple-value-bind (low high increment compare)
      (if big-endian-p
          (values 0 (1- byte-size) 1 #'>)
          (values (1- byte-size) 0 -1 #'<))
    (do ((i (+ low increment) (+ i increment))
         (shift (* (- byte-size 2) 8) (- shift 8))
         (forms nil))
        ((funcall compare i high)
         `(let* ((high-byte (aref , vector-name
                                    (+ ,index-name ,low)))
                 ;; Would be great if we could just sign-extend along
                 ;; with the load, but this is as good as it gets in
                 ;; portable Common Lisp.
                 (signed-high ,(if signedp
                                   `(if (logbitp 7 high-byte)
                                        (- high-byte 256)
                                        high-byte)
                                   'high-byte))
                 (shifted-into-place
                  (ash signed-high ,(* (1- byte-size) 8))))
            (declare (type (unsigned-byte 8) high-byte))
            (declare (type (,(if signedp 'signed-byte 'unsigned-byte) 8)
                           signed-high))
            (logior shifted-into-place ,@(nreverse forms))))
      (push `(ash (aref ,vector-name
                        (+ ,index-name ,i))
                  ,shift)
            forms))))
(defun set-form (vector-name index-name value-name byte-size big-endian-p)
  "Return a form that stores a BYTE-SIZE VALUE-NAME into VECTOR-NAME,
starting at INDEX-NAME.  The value is stored in the vector according to
BIG-ENDIAN-P.  The form returns VALUE-NAME."
  `(progn
     ,@(loop for i from 1 to byte-size
             collect (let ((offset (if big-endian-p
                                       (- byte-size i)
                                       (1- i))))
                       `(setf (aref ,vector-name
                                    (+ ,index-name ,offset))
                              (ldb (byte 8 ,(* 8 (1- i))) ,value-name))))
     ,value-name))
) ; EVAL-WHEN

(macrolet ((define-fetcher (bitsize signedp big-endian-p)
             (let ((ref-name (byte-ref-fun-name bitsize signedp big-endian-p))
                   (bytes (truncate bitsize 8)))
               `(defun ,ref-name (vector index)
                  (declare (type octet-vector vector))
                  (declare (type (integer 0 ,(- array-dimension-limit bytes)) index))
                  (multiple-value-bind (vector start end)
                      (array-data-and-offsets vector index (+ index ,bytes))
                     #+sbcl (declare (optimize (sb-c::insert-array-bounds-checks 0)))
                     (declare (type (integer 0 ,(- array-dimension-limit bytes)) start))
                    (declare (ignore end))
                    ,(ref-form 'vector 'start bytes signedp big-endian-p)))))
           (define-storer (bitsize signedp big-endian-p)
             (let ((ref-name (byte-ref-fun-name bitsize signedp big-endian-p))
                   (set-name (byte-set-fun-name bitsize signedp big-endian-p))
                   (bytes (truncate bitsize 8)))
               `(progn
                 (defun ,set-name (vector index value)
                   (declare (type octet-vector vector))
                   (declare (type (integer 0 ,(- array-dimension-limit bytes)) index))
                   (declare (type (,(if signedp
                                        'signed-byte
                                        'unsigned-byte) ,bitsize) value))
                   (multiple-value-bind (vector start end)
                       (array-data-and-offsets vector index (+ index ,bytes))
                     #+sbcl (declare (optimize (sb-c::insert-array-bounds-checks 0)))
                     (declare (type (integer 0 ,(- array-dimension-limit bytes)) start))
                     (declare (ignore end))
                     ,(set-form 'vector 'start 'value bytes big-endian-p)))
                 (defsetf ,ref-name ,set-name))))
           (define-fetchers-and-storers (bitsize)
               (loop for i from 0 below 4
                     for signedp = (logbitp 1 i)
                     for big-endian-p = (logbitp 0 i)
                     collect `(define-fetcher ,bitsize ,signedp ,big-endian-p) into forms
                     collect `(define-storer ,bitsize ,signedp ,big-endian-p) into forms
                     finally (return `(progn ,@forms)))))
  (define-fetchers-and-storers 16)
  (define-fetchers-and-storers 32)
  (define-fetchers-and-storers 64))

(defun not-supported ()
  (error "not supported"))

(defun ieee-single-ref/be (vector index)
  (declare (ignorable vector index))
  #+sbcl
  (sb-kernel:make-single-float (sb32ref/be vector index))
  #+cmu
  (kernel:make-single-float (sb32ref/be vector index))
  #+ccl
  (ccl::host-single-float-from-unsigned-byte-32 (ub32ref/be vector index))
  #+allegro
  (let ((b (ub32ref/be vector index)))
    (excl:shorts-to-single-float (ldb (byte 16 16) b) (ldb (byte 16 0) b)))
  #+lispworks
  (let* ((ub (ub32ref/be vector index))
         (v (sys:make-typed-aref-vector 4)))
    (declare (optimize (speed 3) (float 0) (safety 0)))
    (declare (dynamic-extent v))
    (setf (sys:typed-aref '(unsigned-byte 32) v 0) ub)
    (sys:typed-aref 'single-float v 0))
  #-(or sbcl cmu ccl allegro lispworks)
  (not-supported))

(defun (setf ieee-single-ref/be) (value vector index)
  (declare (ignorable value vector index))
  #+sbcl
  (progn
    (setf (sb32ref/be vector index) (sb-kernel:single-float-bits value))
    value)
  #+cmu
  (progn
    (setf (sb32ref/be vector index) (kernel:single-float-bits value))
    value)
  #+ccl
  (progn
    (setf (ub32ref/be vector index) (ccl::single-float-bits value))
    value)
  #+allegro
  (multiple-value-bind (hi lo) (excl:single-float-to-shorts value)
    (setf (ub16ref/be vector index) hi
          (ub16ref/be vector (+ index 2) lo))
    value)
  #+lispworks
  (let* ((v (sys:make-typed-aref-vector 4)))
    (declare (optimize (speed 3) (float 0) (safety 0)))
    (declare (dynamic-extent v))
    (setf (sys:typed-aref 'single-float v 0) value)
    (setf (ub32ref/be vector index) (sys:typed-aref '(unsigned-byte 32) v 0))
    value)
  #-(or sbcl cmu ccl allegro lispworks)
  (not-supported))

(defun ieee-single-ref/le (vector index)
  (declare (ignorable vector index))
  #+sbcl
  (sb-kernel:make-single-float (sb32ref/le vector index))
  #+cmu
  (kernel:make-single-float (sb32ref/le vector index))
  #+ccl
  (ccl::host-single-float-from-unsigned-byte-32 (ub32ref/le vector index))
  #+allegro
  (let ((b (ub32ref/le vector index)))
    (excl:shorts-to-single-float (ldb (byte 16 16) b) (ldb (byte 16 0) b)))
  #+lispworks
  (let* ((ub (ub32ref/le vector index))
         (v (sys:make-typed-aref-vector 4)))
    (declare (optimize (speed 3) (float 0) (safety 0)))
    (declare (dynamic-extent v))
    (setf (sys:typed-aref '(unsigned-byte 32) v 0) ub)
    (sys:typed-aref 'single-float v 0))
  #-(or sbcl cmu ccl allegro lispworks)
  (not-supported))

(defun (setf ieee-single-ref/le) (value vector index)
  (declare (ignorable value vector index))
  #+sbcl
  (progn
    (setf (sb32ref/le vector index) (sb-kernel:single-float-bits value))
    value)
  #+cmu
  (progn
    (setf (sb32ref/le vector index) (kernel:single-float-bits value))
    value)
  #+ccl
  (progn
    (setf (ub32ref/le vector index) (ccl::single-float-bits value))
    value)
  #+allegro
  (multiple-value-bind (hi lo) (excl:single-float-to-shorts value)
    (setf (ub16ref/le vector (+ index 2)) hi
          (ub16ref/le vector index lo))
    value)
  #+lispworks
  (let* ((v (sys:make-typed-aref-vector 4)))
    (declare (optimize (speed 3) (float 0) (safety 0)))
    (declare (dynamic-extent v))
    (setf (sys:typed-aref 'single-float v 0) value)
    (setf (ub32ref/le vector index) (sys:typed-aref '(unsigned-byte 32) v 0))
    value)
  #-(or sbcl cmu ccl allegro lispworks)
  (not-supported))

(defun ieee-double-ref/be (vector index)
  (declare (ignorable vector index))
  #+sbcl
  (let ((upper (sb32ref/be vector index))
        (lower (ub32ref/be vector (+ index 4))))
    (sb-kernel:make-double-float upper lower))
  #+cmu
  (let ((upper (sb32ref/be vector index))
        (lower (ub32ref/be vector (+ index 4))))
    (kernel:make-double-float upper lower))
  #+ccl
  (let ((upper (ub32ref/be vector index))
        (lower (ub32ref/be vector (+ index 4))))
    (ccl::make-double-float-from-bits upper lower))
  #-(or sbcl cmu ccl)
  (not-supported))

(defun (setf ieee-double-ref/be) (value vector index)
  (declare (ignorable value vector index))
  #+sbcl
  (progn
    (setf (sb32ref/be vector index) (sb-kernel:double-float-high-bits value)
          (ub32ref/be vector (+ index 4)) (sb-kernel:double-float-low-bits value))
    value)
  #+cmu
  (progn
    (setf (sb32ref/be vector index) (kernel:double-float-high-bits value)
          (ub32ref/be vector (+ index 4)) (kernel:double-float-low-bits value))
    value)
  #+ccl
  (multiple-value-bind (upper lower) (ccl::double-float-bits value)
    (setf (ub32ref/be vector index) upper
          (ub32ref/be vector (+ index 4)) lower)
    value)
  #-(or sbcl cmu ccl)
  (not-supported))

(defun ieee-double-ref/le (vector index)
  (declare (ignorable vector index))
  #+sbcl
  (let ((upper (sb32ref/le vector (+ index 4)))
        (lower (ub32ref/le vector index)))
    (sb-kernel:make-double-float upper lower))
  #+cmu
  (let ((upper (sb32ref/le vector (+ index 4)))
        (lower (ub32ref/le vector index)))
    (kernel:make-double-float upper lower))
  #+ccl
  (let ((upper (ub32ref/le vector (+ index 4)))
        (lower (ub32ref/le vector index)))
    (ccl::make-double-float-from-bits upper lower))
  #-(or sbcl cmu ccl)
  (not-supported))

(defun (setf ieee-double-ref/le) (value vector index)
  (declare (ignorable value vector index))
  #+sbcl
  (progn
    (setf (sb32ref/le vector (+ index 4)) (sb-kernel:double-float-high-bits value)
          (ub32ref/le vector index) (sb-kernel:double-float-low-bits value))
    value)
  #+cmu
  (progn
    (setf (sb32ref/le vector (+ index 4)) (kernel:double-float-high-bits value)
          (ub32ref/le vector index) (kernel:double-float-low-bits value))
    value)
  #+ccl
  (multiple-value-bind (upper lower) (ccl::double-float-bits value)
    (setf (ub32ref/le vector (+ index 4)) upper
          (ub32ref/le vector index) lower)
    value)
  #-(or sbcl cmu ccl)
  (not-supported))