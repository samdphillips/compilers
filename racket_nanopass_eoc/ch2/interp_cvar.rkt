#lang nanopass

(require "cvar.rkt"
         "primitives.rkt")

(provide interp)

(define (find-block label label* block*)
  (cond
    [(null? label*) #f]
    [(eq? label (car label*)) (car block*)]
    [else
     (find-block label (cdr label*) (cdr block*))]))

(define-pass interp : Cvar (e) -> * ()
  (definitions
    (define vars (make-hash)))

  (interp-atom : Atom (a) -> * ()
    [,i i]
    [,x (hash-ref vars x)])

  (interp-prim : Prim (p) -> * ()
    [(prim-app ,op ,[interp-atom : atm* -> v*] ...)
     (apply-prim op v*)])

  (interp-expr : Expr (e) -> * ())

  (interp-statement : Statement (s) -> * ()
    [(assign ,v ,[interp-expr : e]) (hash-set! vars v e)])

  (interp-block : Tail (t) -> * ()
    [(seq ,[interp-statement : stmt] ,tail) (interp-block tail)]
    [(return ,[interp-expr : expr]) expr])

  (interp-program : Program (p) -> * ()
    [(program ,info ([,lbl* ,tail*] ...))
     (interp-block (find-block 'start lbl* tail*))]))