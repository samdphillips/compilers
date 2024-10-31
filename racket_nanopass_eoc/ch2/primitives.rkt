#lang racket/base

(require racket/fixnum
         racket/match)

(provide apply-prim)

(define (apply-prim op args)
  (match* (op args)
    [{'read (list)}
     (define v (read))
     (unless (fixnum? v)
       (error 'interp "read expected a fixnum ~v" v))
     v]
    [{'- (list v)}
     (fx- 0 v)]
    [{'- (list v0 v1)}
     (fx- v0 v1)]
    [{'+ (list v0 v1)}
     (fx+ v0 v1)]))