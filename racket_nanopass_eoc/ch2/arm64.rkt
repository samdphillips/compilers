#lang nanopass

(require "common.rkt")

(provide arm64_var
         unparse-arm64_var)

(define (register? v)
  (match v
    [(list 'Reg
           (or 'x0 'x1 'x2 'x3 'x4 'x5 'x6 'x7
               'x8 'x9 'x10 'x11 'x12 'x13 'x14 'x15
               'x16 'x17 'x18 'x19 'x20 'x21 'x22 'x23
               'x24 'x25 'x26 'x27 'x28 'x29 'x30
               'xzr 'sp))
     #t]
    [_ #f]))

(define (deref? v)
  (match v
    [(list 'Deref (? register?) (? fixnum?)) #t]
    [_ #f]))

(define (instr-name? v)
  (match v
    [(or 'add 'subs 'sub 'stp 'str 'ldr 'ldp 'mov) #t]
    [_ #f]))

(define-language arm64_var
  (entry Program)
  (terminals
   (hash (info))
   (instr-name (op))
   (label (lbl))
   (register (reg r))
   (deref (d))
   (variable (v x))
   (fixnum (i)))

  (Arg (arg) i reg d)

  (Instr (instr)
    (inst op arg ...)
    (bl lbl i)
    (b lbl)
    (ret))

  (Block (b)
    (block info instr ...))

  (Program (p)
    (program info ([lbl b] ...))))