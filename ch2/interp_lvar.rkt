#lang nanopass

(require "lvar.rkt"
         "primitives.rkt")

(provide interp)

(define-pass interp : Lvar (e) -> * ()
  (interp-atom : Atom (a r) -> * ()
    [,x (hash-ref r x)]
    [,i i])

  (interp-prim : Prim (p r) -> * ()
    [(prim-app ,op ,e* ...)
     (define e^
       (for/list ([e (in-list e*)])
         (interp-expr e r)))
     (apply-prim op e^)])

  (interp-expr : Expr (e r) -> * ()
    [(let (,x ,[interp-expr : e r -> v]) ,b)
     (interp-expr b (hash-set r x v))])
  (interp-expr e (hash)))