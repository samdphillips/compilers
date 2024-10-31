#lang nanopass

(require racket/fixnum
         "lint.rkt")

(provide peval)

(define-pass peval : Lint (e) -> Lint ()
  (pe_neg : Prim (e) -> Prim ()
    [(- ,i) `,(fx- 0 i)]
    [(- ,e) `(- ,e)])
  (pe_add : Prim (e) -> Prim ()
    [(+ ,i0 ,i1) `,(fx+ i0 i1)]
    [(+ ,e0 ,e1) `(+ ,e0 ,e1)])
  (pe_sub : Prim (e) -> Prim ()
    [(- ,i0 ,i1) `,(fx- i0 i1)]
    [(- ,e0 ,e1) `(- ,e0 ,e1)])
  (pe_expr : Expr (e) -> Expr ()
    [,i i]
    [(read) `(read)]
    [(- ,[e]) (pe_neg `(- ,e))]
    [(+ ,[e0] ,[e1]) (pe_add `(+ ,e0 ,e1))]
    [(- ,[e0] ,[e1]) (pe_sub `(- ,e0 ,e1))]))
