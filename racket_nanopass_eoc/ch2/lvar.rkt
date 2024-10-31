#lang nanopass

(require "common.rkt")

(provide Lvar
         parse
         unparse-Lvar)

(define-language Lvar
  (entry Expr)
  (terminals
   (fixnum (i))
   (variable (x))
   (primitive-name (op)))

  (Atom (atm) i x)
  (Prim (p)
        (prim-app op e ...))
  (Expr (e b)
        atm
        p
        (let (x e0) e1)))

(define-pass parse : * (s) -> Lvar ()
   (definitions
     (define (do-parse s)
       (with-output-language (Lvar Expr)
         (match s
           [(? fixnum?) s]
           [(? variable?) s]
           [(list 'let (list (? variable? x)
                             e)
                  body)
            `(let (,x ,(do-parse e))
               ,(do-parse body))]
           [s (with-output-language (Lvar Prim)
                (match s
                  [(list 'read) `(prim-app read)]
                  [(list '- e) `(prim-app - ,(do-parse e))]
                  [(list '- e0 e1) `(prim-app - ,(do-parse e0)
                                              ,(do-parse e1))]
                  [(list '+ e0 e1) `(prim-app + ,(do-parse e0)
                                              ,(do-parse e1))]))]))))
  (do-parse s))
