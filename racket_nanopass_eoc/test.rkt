#lang racket/base

(require racket/file
         racket/match
         racket/port
         rackunit)

(define (with-inputs i* thunk)
  (define b*
    (with-output-to-bytes
        (λ ()
          (for ([v (in-list i*)]) (newline) (write v)))))
  (with-input-from-bytes b* thunk))      

(define (build-test spec)
  (define ((test-pass p unparse-input unparse-output t k) ip iv* rv)            
    (define op (p ip))
    (define trv (with-inputs iv* (λ () (t op))))
    (define ci
      (list (make-check-info 'input-program (unparse-input ip))
            (make-check-info 'output-program (unparse-output op))
            (make-check-info 'pass (object-name p))))
    (with-check-info* ci (λ () (check-equal? trv rv)))
    (k op iv* rv))
  (define (build* spec*)
    (match spec*
      [(list (list p ui uo t))
       (test-pass p ui uo t void)]
      [(cons (list p ui uo t) spec*)
       (test-pass p ui uo t (build* spec*))]))
  (match-define (cons (list p t) spec*) spec)
  (define tests (build* spec*))
  (lambda (ip iv*)
    (define op (p ip))
    (define rv (with-inputs iv* (λ () (t op))))
    (tests op iv* rv)))

(define (run-tests mod test-dir num-inputs iterations)
  (define spec
    (dynamic-require `(submod ,mod for-test) 'spec
                     (λ ()
                       (error 'run-tests
                              "no test spec provided module ~v"
                              mod))))
  (define test
    (build-test spec))
  (define program*
    (for/list ([f (in-list (directory-list test-dir #:build? #t))]
               #:when (file-exists? f))
      (cons f (file->value f))))
  (for ([n (in-range iterations)])
    (random-seed n)
    (define iv* (for/list ([i (in-range num-inputs)]) (random 4294967087)))
    (for ([p (in-list program*)])
      (with-check-info (['program (car p)])        
        (test (cdr p) iv*)))))

(module* test #f
  (match-define (vector mod test-dir num-inputs iterations)
    (current-command-line-arguments))
  (run-tests mod
             test-dir
             (string->number num-inputs)
             (string->number iterations)))