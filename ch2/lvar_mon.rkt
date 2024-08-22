#lang nanopass

(require "lvar.rkt")

(provide (all-defined-out))

(define-language Lvar_mon
  (extends Lvar)
  (Prim (p)
    (- (prim-app op e ...))
    (+ (prim-app op atm ...))))
