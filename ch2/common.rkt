#lang racket/base

(require racket/match)

(provide (all-defined-out))

(define label? symbol?)
(define variable? symbol?)

(define (primitive-name? v)
  (match v
    [(or 'read '+ '-) #t]
    [_ #f]))