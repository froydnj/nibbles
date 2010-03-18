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
