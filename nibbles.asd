; -*- mode: lisp -*-

(cl:defpackage :nibbles-system
  (:use :cl))

(cl:in-package :nibbles-system)

(defclass nibbles-source-file (asdf:cl-source-file) ())
(defclass txt-file (asdf:doc-file) ((type :initform "txt")))
(defclass css-file (asdf:doc-file) ((type :initform "css")))

;;; Borrowed from iolib.
(defun defknown-redefinition-error-p (error)
  (and (typep error 'simple-error)
       (search "overwriting old FUN-INFO"
               (simple-condition-format-control error))))

(macrolet ((do-silently (&body body)
             `(handler-bind (((satisfies defknown-redefinition-error-p) #'continue))
                ,@body)))
(defmethod asdf:perform :around ((op asdf:compile-op) (c nibbles-source-file))
  (let ((*print-base* 10)               ; INTERN'ing FORMAT'd symbols
        (*print-case* :upcase)
        #+sbcl (sb-ext:*inline-expansion-limit* (max sb-ext:*inline-expansion-limit* 1000))
        #+cmu (ext:*inline-expansion-limit* (max ext:*inline-expansion-limit* 1000)))
    (do-silently (call-next-method))))

(defmethod asdf:perform :around ((op asdf:load-op) (c nibbles-source-file))
  (do-silently (call-next-method))))

(asdf:defsystem :nibbles
  :version "0.12"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :description "A library for accessing octet-addressed blocks of data in big- and little-endian orders"
  :license "BSD-style (http://opensource.org/licenses/BSD-3-Clause)"
  :default-component-class nibbles-source-file
  :components ((:static-file "README")
               (:static-file "LICENSE")
               (:static-file "NEWS")
               (:file "package")
               (:file "types" :depends-on ("package"))
               (:file "macro-utils" :depends-on ("package"))
               (:file "vectors" :depends-on ("types" "macro-utils"))
               (:file "streams" :depends-on ("vectors"))
	       (:module "doc"
			:components
			((:html-file "index")
			 (:txt-file "nibbles-doc")
			 (:css-file "style")))
               (:module "sbcl-opt"
                        :depends-on ("package" "macro-utils")
                        :components ((:file "fndb")
                                     (:file "nib-tran" :depends-on ("fndb"))
                                     (:file "x86-vm" :depends-on ("fndb"))
                                     (:file "x86-64-vm" :depends-on ("fndb"))))))

(defmethod asdf:perform ((op asdf:test-op)
                         (c (eql (asdf:find-system :nibbles))))
  (asdf:oos 'asdf:test-op 'nibbles-tests))

(asdf:defsystem :nibbles-tests
  :depends-on (:nibbles)
  :version "0.1"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :in-order-to ((asdf:test-op (asdf:load-op :nibbles-tests)))
  :components ((:file "rt")
               (:file "tests" :depends-on ("rt"))))

(defmethod asdf:perform ((op asdf:test-op)
                         (c (eql (asdf:find-system :nibbles-tests))))
  (or (funcall (intern (symbol-name :do-tests) (find-package :rtest)))
      (error "TEST-OP failed for NIBBLES-TESTS")))
