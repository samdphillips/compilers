#lang nanopass

(require "lvar_mon.rkt"
         "primitives.rkt")

(provide interp)

(define-pass interp : Lvar_mon (e) -> * ()
  (interp-atom : Atom (a r) -> * ()
    [,i i]
    [,x (hash-ref r x)])

  (interp-prim : Prim (p r) -> * ()
    [(prim-app ,op ,[interp-atom : atm* r -> v*] ...)
     (apply-prim op v*)])

  (interp-expr : Expr (e r) -> * ()
    [(let (,x ,[interp-expr : e r -> v]) ,b)
     (interp-expr b (hash-set r x v))])
  (interp-expr e (hash)))