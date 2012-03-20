;;;; x86-64-vm.lisp -- VOP definitions SBCL

(cl:in-package :sb-vm)

(define-vop (%check-bound)
  (:translate nibbles::%check-bound)
  (:policy :fast-safe)
  (:args (array :scs (descriptor-reg))
         (bound :scs (any-reg))
         (index :scs (any-reg)))
  (:arg-types simple-array-unsigned-byte-8 positive-fixnum tagged-num
              (:constant (member 2 4 8 16)))
  (:info offset)
  (:temporary (:sc any-reg) temp)
  (:results (result :scs (any-reg)))
  (:result-types positive-fixnum)
  (:vop-var vop)
  (:generator 5
    (let ((error (generate-error-code vop 'invalid-array-index-error
                                      array bound index)))
      ;; We want to check the conditions:
      ;;
      ;; 0 <= INDEX
      ;; INDEX < BOUND
      ;; 0 <= INDEX + OFFSET
      ;; (INDEX + OFFSET) < BOUND
      ;;
      ;; We can do this naively with two unsigned checks:
      ;;
      ;; INDEX <_u BOUND
      ;; INDEX + OFFSET <_u BOUND
      ;;
      ;; If INDEX + OFFSET <_u BOUND, though, INDEX must be less than
      ;; BOUND.  We *do* need to check for 0 <= INDEX, but that has
      ;; already been assured by higher-level machinery.
      (inst lea temp (make-ea :qword
                              :index index :disp (fixnumize offset)))
      (inst cmp temp bound)
      (inst jmp :a error)
      (move result index))))

#.(flet ((frob (bitsize setterp signedp big-endian-p)
           (let* ((name (funcall (if setterp
                                     #'nibbles::byte-set-fun-name
                                     #'nibbles::byte-ref-fun-name)
                                 bitsize signedp big-endian-p))
                  (internal-name (nibbles::internalify name))
                  (ref-mov-insn (if (= bitsize 32)
                                    (if big-endian-p
                                        'mov
                                        (if signedp 'movsxd 'movzxd))
                                    'mov))
                  (result-sc (if signedp 'signed-reg 'unsigned-reg))
                  (result-type (if signedp 'signed-num 'unsigned-num)))
             `(define-vop (,name)
                (:translate ,internal-name)
                (:policy :fast-safe)
                (:args (vector :scs (descriptor-reg))
                       (index :scs (immediate unsigned-reg))
                       ,@(when setterp
                           `((value* :scs (,result-sc) :target result))))
                (:arg-types simple-array-unsigned-byte-8
                            positive-fixnum
                            ,@(when setterp
                                `(,result-type)))
                ,@(when (and setterp big-endian-p)
                    `((:temporary (:sc unsigned-reg
                                       :from (:load 0)
                                       :to (:result 0)) temp)))
                (:results (result :scs (,result-sc)))
                (:result-types ,result-type)
                (:generator 3
                  (let* ((base-disp (- (* vector-data-offset n-word-bytes)
                                       other-pointer-lowtag))
                         (operand-size ,(if (= bitsize 32) :dword :qword))
                         ,@(when setterp
                             '((value (reg-in-size value* operand-size))))
                         ,@(when (and setterp big-endian-p)
                             '((temp (reg-in-size temp operand-size))))
                         (memref (sc-case index
                                   (immediate
                                    (make-ea operand-size :base vector
                                             :disp (+ (tn-value index) base-disp)))
                                   (t
                                    (make-ea operand-size
                                             :base vector :index index
                                             :disp base-disp)))))
                    ,@(when (and setterp big-endian-p)
                        `((inst mov temp value)
                          (inst bswap temp)))
                    ,(if setterp
                         `(inst mov memref ,(if big-endian-p
                                                'temp
                                                'value))
                         `(inst ,ref-mov-insn
                                ,(if (and big-endian-p (= bitsize 32))
                                     '(reg-in-size result :dword)
                                     'result)
                                memref))
                    ,@(if setterp
                          '((move result value*))
                          (when big-endian-p
                            `((inst bswap
                                    ,(if (= bitsize 32)
                                         '(reg-in-size result :dword)
                                         'result))
                              ,(when (and (= bitsize 32) signedp)
                                 `(inst movsx result (reg-in-size result :dword))))))))))))
    (loop for i from 0 upto #b1111
          for bitsize = (if (logbitp 3 i) 32 64)
          for setterp = (logbitp 2 i)
          for signedp = (logbitp 1 i)
          for big-endian-p = (logbitp 0 i)
          collect (frob bitsize setterp signedp big-endian-p) into forms
          finally (return `(progn ,@forms))))
