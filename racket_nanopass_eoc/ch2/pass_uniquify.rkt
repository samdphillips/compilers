#lang nanopass

(require "lvar.rkt")

(provide uniquify)

(define-pass uniquify : Lvar (e) -> Lvar ()
  (Atom : Atom (a r) -> Atom ()
    [,x (hash-ref r x)])
  (Prim : Prim (p r) -> Prim ())
  (Expr : Expr (e r) -> Expr ()
    [(let (,x ,[e0]) ,e1)
     (define new-x (gensym x))
     `(let (,new-x ,e0)
        ,(Expr e1 (hash-set r x new-x)))])
  (Expr e (hash)))