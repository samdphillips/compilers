#lang racket/base

(require "lvar.rkt"
         "lvar_mon.rkt"
         "pass_uniquify.rkt"
         "pass_rco.rkt"
         (prefix-in lvar: "interp_lvar.rkt")
         (prefix-in lvar_mon: "interp_lvar_mon.rkt"))

(module* for-test #f
  (provide spec)
  (define spec
    (list (list parse lvar:interp)
          (list uniquify unparse-Lvar unparse-Lvar lvar:interp)
          (list rco unparse-Lvar unparse-Lvar_mon lvar_mon:interp))))
