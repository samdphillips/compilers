#lang nanopass

(require racket/fixnum)

(module* for-test #f
  (provide spec)
  (define spec
    (list (list parse interp)
          (list pe unparse-Lint unparse-Lint interp))))          

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

(define-pass pe : Lint (e) -> Lint ()
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