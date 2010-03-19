; -*- mode: lisp -*-

(asdf:defsystem :nibbles
  :version "0.1"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :description "A library for accessing octet-addressed blocks of data"
  :components ((:static-file "README")
               (:static-file "LICENSE")
               (:file "package")
               (:file "types" :depends-on ("package"))
               (:file "macro-utils" :depends-on ("package"))
               (:file "vectors" :depends-on ("types")
                      :in-order-to ((asdf:compile-op (asdf:load-op "macro-utils"))))
               (:file "streams" :depends-on ("vectors"))))
