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

(defun ieee-single-set/be (vector index value)
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
          (ub16ref/be vector (+ index 2)) lo)
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
(defsetf ieee-single-ref/be ieee-single-set/be)

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

(defun ieee-single-set/le (vector index value)
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
          (ub16ref/le vector index) lo)
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
(defsetf ieee-single-ref/le ieee-single-set/le)

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
    (ccl::double-float-from-bits upper lower))
  #-(or sbcl cmu ccl)
  (not-supported))

(defun ieee-double-set/be (vector index value)
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
(defsetf ieee-double-ref/be ieee-double-set/be)

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
    (ccl::double-float-from-bits upper lower))
  #-(or sbcl cmu ccl)
  (not-supported))

(defun ieee-double-set/le (vector index value)
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
(defsetf ieee-double-ref/le ieee-double-set/le)
