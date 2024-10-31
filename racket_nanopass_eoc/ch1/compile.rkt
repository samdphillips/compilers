#lang racket/base

(require "lint.rkt"
         "pass_peval.rkt"
         "interp_lint.rkt")

(module* for-test #f
  (provide spec)
  (define spec
    (list (list parse interp)
          (list peval unparse-Lint unparse-Lint interp))))
