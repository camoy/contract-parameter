#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [contract-parameter? predicate/c]
  [make-contract-parameter (->* (contract?)
                                (symbol?)
                                contract-parameter?)]
  [parameterize-contract? predicate/c])
 parameterize/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require "private/contract-parameter.rkt"
         "private/parameterize-contract-macro.rkt"
         "private/parameterize-contract.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (define foo/c (make-contract-parameter any/c))
  (define (exactly-once/c ctc)
    (define (before _) 0)
    (define (during _ acc) (add1 acc))
    (define (after acc) (= acc 1))
    (parameterize/c ([ctc during]
                     #:before before
                     #:after after)
      any/c))

  (define/contract (foo x)
    (-> foo/c any)
    x)

  (define/contract (bad)
    (parameterize/c ([foo/c none/c]) any/c)
    (foo 42))

  (define/contract (good)
    (parameterize/c ([foo/c integer?]) any/c)
    (foo 42))

  (define/contract (exactly-once-actually-none)
    (exactly-once/c foo/c)
    (void))

  (define/contract (exactly-once-actually-once)
    (exactly-once/c foo/c)
    (foo 42))

  (define/contract (exactly-once-actually-twice)
    (exactly-once/c foo/c)
    (+ (foo 42) (foo 42)))

  (chk
   (foo 42) 42
   (good) 42
   #:x (bad) "none/c allows no values"
   #:x (exactly-once-actually-none) "after clause failed"
   (exactly-once-actually-once) 42
   #:x (exactly-once-actually-twice) "after clause failed"
   ))
