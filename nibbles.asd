; -*- mode: lisp -*-

(cl:defpackage :nibbles-system
  (:use :cl))

(cl:in-package :nibbles-system)

(defclass txt-file (asdf:doc-file) ())
(defclass css-file (asdf:doc-file) ())

(defmethod asdf:source-file-type ((c txt-file) (s asdf:module)) "txt")
(defmethod asdf:source-file-type ((c css-file) (s asdf:module)) "css")

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
                        :if-component-dep-fails :ignore
                        :components ((:file "fndb"
                                            :in-order-to ((asdf:compile-op
                                                           (asdf:feature :sbcl))))
                                     (:file "nib-tran" :depends-on ("fndb"))
                                     (:file "x86-vm"
                                            :in-order-to ((asdf:compile-op (asdf:feature :x86))
                                                          (asdf:compile-op (asdf:load-op "fndb"))))
                                     (:file "x86-64-vm" :depends-on ("fndb")
                                            :in-order-to ((asdf:compile-op (asdf:feature :x86-64))
                                                          (asdf:compile-op (asdf:load-op "fndb"))))))))

(defmethod asdf:perform ((op asdf:test-op)
                         (c (eql (asdf:find-system :nibbles))))
  (asdf:oos 'asdf:test-op 'nibbles-tests))

;;; A tester's job is never done!
(defmethod asdf:operation-done-p ((op asdf:test-op)
                                  (c (eql (asdf:find-system :nibbles))))
  nil)

(asdf:defsystem :nibbles-tests
  :depends-on (:nibbles)
  :version "0.1"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :in-order-to ((asdf:test-op (asdf:load-op :nibbles-tests)))
  :components ((:file "rt")
               (:file "tests" :depends-on ("rt"))))

(defmethod asdf:operation-done-p ((op asdf:test-op)
                                  (c (eql (asdf:find-system :nibbles-tests))))
  nil)

(defmethod asdf:perform ((op asdf:test-op)
                         (c (eql (asdf:find-system :nibbles-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "RTEST")))
      (error "TEST-OP failed for NIBBLES-TESTS")))
