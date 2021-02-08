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

  (define prohibitable/c (make-contract-parameter any/c))
  (define prohibit/c (parameterize/c [prohibitable/c none/c]))
  (define/contract (will-prevent x)
    (-> (and/c integer? prohibitable/c) any)
    (add1 x))

  (define/contract (good-prohibit)
    prohibit/c
    43)

  (define/contract (bad-prohibit)
    prohibit/c
    (will-prevent 42))

  (chk
   (good-prohibit) 43
   #:x (bad-prohibit) "none/c allows no values"
   ))
