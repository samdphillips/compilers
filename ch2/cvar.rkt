#lang nanopass

(provide Cvar
         unparse-Cvar)

(define label? symbol?)
(define var? symbol?)

(define (prim-name? v)
  (match v
    [(or 'read '+ '-) #t]
    [_ #f]))

(define-language Cvar
  (entry Program)
  (terminals
   [hash (info)]
   [label (lbl)]
   [var (v x)]
   [fixnum (i)]
   [prim-name (op)])

  (Atom (atm) i x)

  (Prim (prim)
    (prim-app op atm ...))

  (Expr (expr e)
    atm
    prim)

  (Statement (stmt)
    (assign v expr))

  (Tail (tail)
    (return expr)
    (seq stmt tail))

  (Program (p)
   (program info ([lbl tail] ...))))
