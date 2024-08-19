#lang nanopass

(require "lvar.rkt")

(provide (all-defined-out))

(define-language Lvar_mon
  (extends Lvar)
  (Prim (pr)
    (- (+ e0 e1)
       (- e)
       (- e0 e1))
    (+ (+ atm0 atm1)
       (- atm)
       (- atm0 atm1))))
