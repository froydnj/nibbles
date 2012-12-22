; -*- mode: lisp -*-

(cl:defpackage :nibbles-system
  (:use :cl))

(cl:in-package :nibbles-system)

(defclass txt-file (asdf:doc-file) ((type :initform "txt")))
(defclass css-file (asdf:doc-file) ((type :initform "css")))

(asdf:defsystem :nibbles
  :version "0.10"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :description "A library for accessing octet-addressed blocks of data"
  :components ((:static-file "README")
               (:static-file "LICENSE")
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
  (or (funcall (intern "DO-TESTS" (find-package "RTEST")))
      (error "TEST-OP failed for NIBBLES-TESTS")))
