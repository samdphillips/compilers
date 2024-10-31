#lang nanopass

(require "cvar.rkt"
         "arm64.rkt")

(provide select-instructions)

(define-parser p Cvar)

(define x (p `(program ,(hash) ([start (return 42)]))))

(define-pass select-instructions : Cvar (p) -> arm64_var ()
  (do-stmt : Statement (s) -> * ()
    [(assign ,v (prim-app ,op ,atm0 ,atm1))
     (list `(inst add ,v ,atm0 ,atm1))]
    [(assign ,v ,expr)
     (error 'do-stmt "fixme")])

  (do-tail : Tail (t) -> * ()
    [(seq ,[do-stmt : stmt -> instr*]
          ,[do-tail : tail -> instr**])
     (append instr* instr**)]
    [(return ,expr)
     (error 'do-tail "fixme")]
    [(return ,atm) (list `(inst mov (Reg w0) ,atm)
                         `(ret))])

  (do-block : Tail (t) -> Block ()
    [(return ,[do-tail : expr -> instr*])
     (let ([info (hash)])
       `(block ,info ,instr*))])

  )