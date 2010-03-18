;;;; vectors.lisp -- signed/unsigned byte accessors

(cl:in-package :nibbles)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun byte-ref-fun-name (bitsize signedp big-endian-p)
  (intern (format nil "~:[U~;S~]B~DREF/~:[LE~;BE~]"
                  signedp bitsize big-endian-p)))
) ; EVAL-WHEN

;;; These functions are named according to big-endian conventions.  The
;;; comment is here because I always forget and need to be reminded.
#.(loop for i from 1 to 8
        collect (let ((name (intern (format nil "~:@(~:R~)-BYTE" i))))
                  `(progn
                    (declaim (inline ,name))
                    (declaim (ftype (function (unsigned-byte) (unsigned-byte 8)) ,name))
                    (defun ,name (ub)
                      (declare (type unsigned-byte ub))
                      (ldb (byte 8 ,(* 8 (1- i))) ub)))) into forms
        finally (return `(progn ,@forms)))

(macrolet ((define-fetcher (bitsize signedp big-endian-p)
             (let ((name (byte-ref-fun-name bitsize signedp big-endian-p))
                   (bytes (truncate bitsize 8)))
               `(progn
                 (declaim (inline ,name))
                 (defun ,name (buffer index)
                   (declare (type simple-octet-vector buffer))
                   (declare (type (integer 0 ,(- array-dimension-limit bytes)) index))
                   (let ((value (logand ,(1- (ash 1 bitsize))
                                        ,(loop for i from 0 below bytes
                                               collect (let* ((offset (if big-endian-p
                                                                          i
                                                                          (- bytes i 1)))
                                                              (shift (if big-endian-p
                                                                         (* (- bytes i 1) 8)
                                                                         (* offset 8))))
                                                         `(ash (aref buffer (+ index ,offset)) ,shift)) into forms
                                               finally (return `(logior ,@forms))))))
                     ,(if signedp
                          `(if (logbitp ,(1- bitsize) value)
                               (dpb value (byte ,bitsize 0) -1)
                               value)
                          'value))))))
           (define-storer (bitsize signedp big-endian-p)
             (let ((name (byte-ref-fun-name bitsize signedp big-endian-p))
                   (bytes (truncate bitsize 8)))
               `(progn
                 (declaim (inline (setf ,name)))
                 (defun (setf ,name) (value buffer index)
                   (declare (type simple-octet-vector buffer))
                   (declare (type (integer 0 ,(- array-dimension-limit bytes)) index))
                   (declare (type (unsigned-byte ,bitsize) value))
                   ,@(loop for i from 1 to bytes
                           collect (let ((offset (if big-endian-p
                                                     (- bytes i)
                                                     (1- i))))
                                     `(setf (aref buffer (+ index ,offset))
                                       (,(intern (format nil "~:@(~:R~)-BYTE" i)) value))))
                   (values)))))
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

(defun ieee-single-ref/be (buffer index)
  #+sbcl
  (sb-kernel:make-single-float (sb32ref/be buffer index))
  #+ccl
  (ccl::host-single-float-from-unsigned-byte-32 (ub32ref/be buffer index))
  #+allegro
  (let ((b (ub32ref/be buffer index)))
    (excl:shorts-to-single-float (ldb (byte 16 16) b) (ldb (byte 16 0) b)))
  #-(or sbcl ccl allegro)
  (not-supported))

(defun (setf ieee-single-ref/be) (value buffer index)
  #+sbcl
  (progn
    (setf (sb32ref/be buffer index) (sb-kernel:single-float-bits value))
    value)
  #+ccl
  (progn
    (setf (ub32ref/be buffer index) (ccl::single-float-bits value))
    value)
  #+allegro
  (multiple-value-bind (hi lo) (excl:single-float-to-shorts value)
    (setf (ub16ref/be buffer index) hi
          (ub16ref/be buffer (+ index 2) lo))
    value)
  #-(or sbcl ccl allegro)
  (not-supported))

(defun ieee-single-ref/le (buffer index)
  #+sbcl
  (sb-kernel:make-single-float (sb32ref/le buffer index))
  #+ccl
  (ccl::host-single-float-from-unsigned-byte-32 (ub32ref/le buffer index))
  #+allegro
  (let ((b (ub32ref/le buffer index)))
    (excl:shorts-to-single-float (ldb (byte 16 16) b) (ldb (byte 16 0) b)))
  #-(or sbcl ccl allegro)
  (not-supported))

(defun (setf ieee-single-ref/le) (value buffer index)
  #+sbcl
  (progn
    (setf (sb32ref/le buffer index) (sb-kernel:single-float-bits value))
    value)
  #+ccl
  (progn
    (setf (ub32ref/le buffer index) (ccl::single-float-bits value))
    value)
  #+allegro
  (multiple-value-bind (hi lo) (excl:single-float-to-shorts value)
    (setf (ub16ref/le buffer (+ index 2)) hi
          (ub16ref/le buffer index lo))
    value)
  #-(or sbcl ccl allegro)
  (not-supported))

(defun ieee-double-ref/be (buffer index)
  #+sbcl
  (let ((upper (sb32ref/be buffer index))
        (lower (ub32ref/be buffer (+ index 4))))
    (sb-kernel:make-double-float upper lower))
  #+ccl
  (let ((upper (ub32ref/be buffer index))
        (lower (ub32ref/be buffer (+ index 4))))
    (ccl::make-double-float-from-bits upper lower))
  #-(or sbcl ccl)
  (not-supported))

(defun (setf ieee-double-ref/be) (value buffer index)
  #+sbcl
  (progn
    (setf (sb32ref/be buffer index) (sb-kernel:double-float-high-bits value)
          (ub32ref/be buffer (+ index 4)) (sb-kernel:double-float-low-bits value))
    value)
  #+ccl
  (multiple-value-bind (upper lower) (ccl::double-float-bits value)
    (setf (ub32ref/be buffer index) upper
          (ub32ref/be buffer (+ index 4)) lower)
    value)
  #-(or sbcl ccl)
  (not-supported))

(defun ieee-double-ref/le (buffer index)
  #+sbcl
  (let ((upper (sb32ref/le buffer (+ index 4)))
        (lower (ub32ref/le buffer index)))
    (sb-kernel:make-double-float upper lower))
  #+ccl
  (let ((upper (ub32ref/le buffer (+ index 4)))
        (lower (ub32ref/le buffer index)))
    (ccl::make-double-float-from-bits upper lower))
  #-(or sbcl ccl)
  (not-supported))

(defun (setf ieee-double-ref/le) (value buffer index)
  #+sbcl
  (progn
    (setf (sb32ref/le buffer (+ index 4)) (sb-kernel:double-float-high-bits value)
          (ub32ref/le buffer index) (sb-kernel:double-float-low-bits value))
    value)
  #+ccl
  (multiple-value-bind (upper lower) (ccl::double-float-bits value)
    (setf (ub32ref/le buffer (+ index 4)) upper
          (ub32ref/le buffer index) lower)
    value)
  #-(or sbcl ccl)
  (not-supported))