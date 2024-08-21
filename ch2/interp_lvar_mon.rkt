#lang nanopass

(require racket/fixnum
         "lvar_mon.rkt")

(provide interp)

(define-pass interp : Lvar_mon (e) -> * ()
  (interp-atom : Atom (a r) -> * ()
    [,i i]
    [,x (hash-ref r x)])
  (interp-prim : Prim (p r) -> * ()
    [(read) (define v (read))
            (unless (fixnum? v)
              (error 'interp "read expected a fixnum ~v" v))
            v]
    [(- ,[interp-atom : atm r -> v]) (fx- 0 v)]
    [(+ ,[interp-atom : atm0 r -> v0]
        ,[interp-atom : atm1 r -> v1])
     (fx+ v0 v1)]
    [(- ,[interp-atom : atm0 r -> v0]
        ,[interp-atom : atm1 r -> v1])
     (fx- v0 v1)])
  (interp-expr : Expr (e r) -> * ()
    [(let (,x ,e) ,b) (define v (interp-expr e r))
                      (interp-expr b (hash-set r x v))])
  (interp-expr e (hash)))