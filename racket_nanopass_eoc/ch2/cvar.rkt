#lang nanopass

(require "common.rkt")

(provide Cvar
         unparse-Cvar)

(define-language Cvar
  (entry Program)
  (terminals
   [hash (info)]
   [label (lbl)]
   [variable (v x)]
   [fixnum (i)]
   [primitive-name (op)])

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
