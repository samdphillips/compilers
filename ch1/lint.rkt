#lang nanopass

(provide (all-defined-out))

(define-language Lint
  (entry Expr)
  (terminals
   (fixnum (i)))
  (Prim (pr)
        (read)
        (+ e0 e1)
        (- e)
        (- e0 e1))
  (Expr (e)
        i
        pr))

(define-pass parse : * (s) -> Lint ()
   (definitions
     (define (do-parse s)
       (with-output-language Lint
         (match s
           [(? fixnum?) s]
           [s (with-output-language (Lint Prim)
                (match s
                  [(list 'read) `(read)]
                  [(list '- e) `(- ,(do-parse e))]
                  [(list '- e0 e1) `(- ,(do-parse e0)
                                       ,(do-parse e1))]
                  [(list '+ e0 e1) `(+ ,(do-parse e0)
                                       ,(do-parse e1))]))]))))
  (do-parse s))
