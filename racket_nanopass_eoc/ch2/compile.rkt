#lang racket/base

(require "lvar.rkt"
         "lvar_mon.rkt"
         "cvar.rkt"
         "arm64.rkt"
         "pass_uniquify.rkt"
         "pass_rco.rkt"
         "pass_explicate.rkt"
         "pass_select.rkt"
         (prefix-in lvar: "interp_lvar.rkt")
         (prefix-in lvar_mon: "interp_lvar_mon.rkt")
         (prefix-in cvar: "interp_cvar.rkt"))

(module* for-test #f
  (provide spec)
  (define spec
    (list (list parse lvar:interp)
          (list uniquify unparse-Lvar unparse-Lvar lvar:interp)
          (list rco unparse-Lvar unparse-Lvar_mon lvar_mon:interp)
          (list explicate-control unparse-Lvar_mon unparse-Cvar cvar:interp)
          (list select-instructions unparse-Cvar unparse-arm64_var values))))
