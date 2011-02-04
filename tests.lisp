;;;; tests.lisp -- tests for various bits of functionality

(cl:defpackage :nibbles-tests
  (:use :cl))

(cl:in-package :nibbles-tests)

;;; Basic tests for correctness.

(defun make-byte-combiner (n-bytes)
  (let ((count 0)
        (buffer 0))
    #'(lambda (byte)
        (setf buffer (logior (ash buffer 8) byte))
        (unless (= count n-bytes)
          (incf count))
        (cond
          ((= count n-bytes)
           (let ((val (ldb (byte (* 8 n-bytes) 0) buffer)))
             (multiple-value-prog1 (values val t)
               (setf buffer val))))
          (t (values 0 nil))))))

(defun generate-random-test (ref-size signedp &optional (n-octets 4096))
  (do* ((byte-kind (if signedp 'signed-byte 'unsigned-byte))
        (n-bits (* 8 ref-size))
        (total-octets (+ n-octets (1- ref-size)))
        (bv (make-array total-octets
                        :element-type '(unsigned-byte 8)))
        (ev (make-array n-octets
                        :element-type `(,byte-kind ,n-bits)))
        (i 0 (1+ i))
        (j 0)
        (combiner (make-byte-combiner ref-size)))
      ((>= i total-octets) (values bv ev n-octets))
    (let ((byte (random 256)))
      (multiple-value-bind (aggregate set-p) (funcall combiner byte)
        (setf (aref bv i) byte)
        (when set-p
          (setf (aref ev j)
                (if (and signedp (logbitp (1- n-bits) aggregate))
                    (dpb aggregate (byte n-bits 0) -1)
                    aggregate))
          (incf j))))))

(defun ref-test (reffer ref-size signedp &optional (n-octets 4096))
  (multiple-value-bind (byte-vector expected-vector)
      (generate-random-test ref-size signedp n-octets)
    (loop for i from 0 below n-octets
          for j from 0
          do (let ((reffed-val (funcall reffer byte-vector i))
                   (expected-val (aref expected-vector j)))
               (unless (= reffed-val expected-val)
                 (error "wanted ~D, got ~D from ~A"
                        expected-val reffed-val
                        (subseq byte-vector i (+ i ref-size)))))
          finally (return :ok))))

(defun set-test (setter set-size signedp &optional (n-octets 4096))
  (multiple-value-bind (byte-vector expected-vector)
      (generate-random-test set-size signedp n-octets)
    (loop with fill-vec = (let ((v (copy-seq byte-vector)))
                            (fill v 0)
                            v)
          for i from 0 below n-octets
          for j from 0
          do (funcall setter fill-vec i (aref expected-vector j))
          finally (return
                    (if (mismatch fill-vec byte-vector)
                        (error "wanted ~A, got ~A" byte-vector fill-vec)
                        :ok)))))

;;; Big-endian integer ref tests

(rtest:deftest :ub16ref/be
  (ref-test #'nibbles:ub16ref/be 2 nil)
  :ok)

(rtest:deftest :sb16ref/be
  (ref-test #'nibbles:sb16ref/be 2 t)
  :ok)

(rtest:deftest :ub32ref/be
  (ref-test #'nibbles:ub32ref/be 4 nil)
  :ok)

(rtest:deftest :sb32ref/be
  (ref-test #'nibbles:sb32ref/be 4 t)
  :ok)

(rtest:deftest :ub64ref/be
  (ref-test #'nibbles:ub64ref/be 8 nil)
  :ok)

(rtest:deftest :sb64ref/be
  (ref-test #'nibbles:sb64ref/be 8 t)
  :ok)

;;; Big-endian set tests
;;;
;;; FIXME: DEFSETF doesn't automagically define SETF functions, so we
;;; have to reach into internals to do these tests.  It would be ideal
;;; if we didn't have to do this.

(rtest:deftest :ub16set/be
  (set-test #'nibbles::ub16set/be 2 nil)
  :ok)

(rtest:deftest :sb16set/be
  (set-test #'nibbles::sb16set/be 2 t)
  :ok)

(rtest:deftest :ub32set/be
  (set-test #'nibbles::ub32set/be 4 nil)
  :ok)

(rtest:deftest :sb32set/be
  (set-test #'nibbles::sb32set/be 4 t)
  :ok)

(rtest:deftest :ub64set/be
  (set-test #'nibbles::ub64set/be 8 nil)
  :ok)

(rtest:deftest :sb64set/be
  (set-test #'nibbles::sb64set/be 8 t)
  :ok)
