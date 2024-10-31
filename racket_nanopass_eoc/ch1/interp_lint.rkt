#lang nanopass

(require racket/fixnum
         "lint.rkt")

(provide interp)

(define-pass interp : Lint (e) -> * ()
  (interp-prim : Prim (p) -> * ()
    [(read) (define v (read))
            (unless (fixnum? v)
              (error 'interp "read expected a fixnum ~v" v))
            v]
    [(- ,e) (define v (interp-expr e))
            (fx- 0 v)]
    [(+ ,e0 ,e1) (define v0 (interp-expr e0))
                 (define v1 (interp-expr e1))
                 (fx+ v0 v1)]
    [(- ,e0 ,e1) (define v0 (interp-expr e0))
                 (define v1 (interp-expr e1))
                 (fx- v0 v1)])
  (interp-expr : Expr (e) -> * ()
    [,i i]
    [,pr (interp-prim pr)]))