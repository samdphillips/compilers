#lang nanopass

(require "lvar.rkt"
         "lvar_mon.rkt")

(provide rco)

(define (build-let bind* body)
  (cond
    [(null? bind*) body]
    [else
     (define bind (car bind*))
     (build-let (cdr bind*)
                (with-output-language (Lvar_mon Expr)
                  `(let (,(car bind) ,(cdr bind)) ,body)))]))

(define-pass rco : Lvar (e) -> Lvar_mon ()
  (definitions
    (define (introduce-temp e)
      (define tmp (gensym 'rco-temp))
      (values tmp (list (cons tmp (rco-expr e))))))

  (rco-atom : Expr (e) -> Expr (bind*)
    [,atm (values atm null)]
    [else (introduce-temp e)])

  (rco-expr : Expr (e) -> Expr ()
    [(- ,[rco-atom : e -> atm b])
     (build-let b (in-context Prim `(- ,atm)))]
    [(+ ,[rco-atom : e0 -> atm0 b0]
        ,[rco-atom : e1 -> atm1 b1])
     (build-let (append b1 b0)
                (in-context Prim `(+ ,atm0 ,atm1)))]
    [(- ,[rco-atom : e0 -> atm0 b0]
        ,[rco-atom : e1 -> atm1 b1])
     (build-let (append b1 b0)
                (in-context Prim `(- ,atm0 ,atm1)))]))
