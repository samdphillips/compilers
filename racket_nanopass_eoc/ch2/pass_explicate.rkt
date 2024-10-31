#lang nanopass

(require "cvar.rkt"
         "lvar_mon.rkt")

(provide explicate-control)

(define-pass explicate-control : Lvar_mon (e) -> Cvar ()
  (ec-assign : Expr (e x t) -> Tail ()
    [,atm
     `(seq (assign ,x ,atm) ,t)]
    [(prim-app ,op ,atm ...)
     `(seq (assign ,x (prim-app ,op ,atm ...))
           ,t)]
    [(let (,x1 ,e1) ,b)
     (ec-assign e1 x1 (ec-assign b x t))])

  (ec-tail-prim : Prim (p) -> Tail ()
    [(prim-app ,op ,atm* ...)
     `(return (prim-app ,op ,atm* ...))])

  (ec-tail : Expr (e) -> Tail ()
    [,atm `(return ,atm)]
    [(let (,x ,e) ,[ec-tail : b -> tail])
     (ec-assign e x tail)])

  (let ([info (hash)]
        [tail (ec-tail e)])
    `(program ,info ((start ,tail)))))
