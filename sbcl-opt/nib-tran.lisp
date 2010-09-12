;;;; nib-tran.lisp -- DEFTRANSFORMs for SBCL

(cl:in-package :nibbles)

#.(flet ((includep (bitsize signedp setterp)
           ;; Bleh.  No good way to solve this atm.
           ;;
           ;; Non-x86.  No support.
           #-(or x86 x86-64)
           nil
           ;; x86.  Can do everything.
           #+x86
           t
           ;; x86-64.  Can't do 16-bit right now.  Must verify that the
           ;; 16-bit ROL support does the right thing for all registers
           ;; first.
           #+x86-64
           (/= bitsize 16)))
    (loop for i from 0 to #-x86-64 #b0111 #+x86-64 #b1011
          for bitsize = (ecase (ldb (byte 2 2) i)
                          (0 16)
                          (1 32)
                          (2 64))
          for signedp = (logbitp 1 i)
          for setterp = (logbitp 0 i)
          for byte-fun = (if setterp
                             #'byte-set-fun-name
                             #'byte-ref-fun-name)
          for big-fun = (funcall byte-fun bitsize signedp t)
          for little-fun = (funcall byte-fun bitsize signedp nil)
          for internal-big = (internalify big-fun)
          for internal-little = (internalify little-fun)
          for arg-type = `(,(if signedp
                                'signed-byte
                                'unsigned-byte)
                                ,bitsize)
          for external-arg-types = `(array index ,@(when setterp
                                                     `(,arg-type)))
          for internal-arg-types = (subst '(simple-array (unsigned-byte 8)) 'array
                                          external-arg-types)
          for big-transform = `(sb-c:deftransform ,big-fun ((vector offset ,@(when setterp
                                                                               '(value)))
                                                                    ,internal-arg-types ,arg-type)
                                 '(,internal-big vector (%check-bound vector (length vector) offset ,(truncate bitsize 8))
                                   ,@(when setterp '(value))))
          for little-transform = (subst internal-little
                                          internal-big
                                          (subst little-fun big-fun big-transform))
          when (includep bitsize signedp setterp)
            collect big-transform into transforms
          when (includep bitsize signedp setterp)
            collect little-transform into transforms
          finally (return `(progn ,@transforms))))
