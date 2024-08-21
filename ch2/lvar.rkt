#lang nanopass

(provide Lvar
         parse
         unparse-Lvar)

(define variable? symbol?)

(define-language Lvar
  (entry Expr)
  (terminals
   (fixnum (i))
   (variable (x)))
  (Atom (atm) i x)
  (Prim (prim)
        (read)
        (+ e0 e1)
        (- e)
        (- e0 e1))
  (Expr (e b)
        atm
        prim
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
                  [(list 'read) `(read)]
                  [(list '- e) `(- ,(do-parse e))]
                  [(list '- e0 e1) `(- ,(do-parse e0)
                                       ,(do-parse e1))]
                  [(list '+ e0 e1) `(+ ,(do-parse e0)
                                       ,(do-parse e1))]))]))))
  (do-parse s))
