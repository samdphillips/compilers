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
    [(prim-app ,op ,[rco-atom : e* -> atm* b*] ...)
     (build-let (apply append b*)
                `(prim-app ,op ,atm* ...))]))
