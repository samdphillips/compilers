#lang nanopass

(require "lvar.rkt")

(provide uniquify)

(define-pass uniquify : Lvar (e) -> Lvar ()
  (Prim : Prim (p r) -> Prim ())
  (Expr : Expr (e r) -> Expr ()
    [,x `,(hash-ref r x)]
    [(let (,x ,[e0]) ,e1)
     (define new-x (gensym x))
     `(let (,new-x ,e0)
        ,(Expr e1 (hash-set r x new-x)))])
  (Expr e (hash)))