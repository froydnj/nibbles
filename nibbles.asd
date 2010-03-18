; -*- mode: lisp -*-

(asdf:defsystem :nibbles
  :version "0.1"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :description "A library for accessing octet-addressed blocks of data"
  :components ((:static-file "LICENSE")
               (:file "package")
               (:file "types" :depends-on ("package"))
               (:file "vectors" :depends-on ("types"))
               (:file "streams" :depends-on ("vectors"))))
