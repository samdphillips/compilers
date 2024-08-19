#lang nanopass

(require racket/fixnum
         "lvar.rkt")

(provide interp)

(define-pass interp : Lvar (e) -> * ()
  (interp-prim : Prim (p r) -> * ()
    [(read) (define v (read))
            (unless (fixnum? v)
              (error 'interp "read expected a fixnum ~v" v))
            v]
    [(- ,e) (define v (interp-expr e r))
            (fx- 0 v)]
    [(+ ,e0 ,e1) (define v0 (interp-expr e0 r))
                 (define v1 (interp-expr e1 r))
                 (fx+ v0 v1)]
    [(- ,e0 ,e1) (define v0 (interp-expr e0 r))
                 (define v1 (interp-expr e1 r))
                 (fx- v0 v1)])
  (interp-expr : Expr (e r) -> * ()
    [,i i]
    [,x (hash-ref r x)]
    [,prim (interp-prim prim r)]
    [(let (,x ,e) ,b) (define v (interp-expr e r))
                      (interp-expr b (hash-set r x v))])
  (interp-expr e (hash)))