#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         contract-etc
         contract-parameter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Enforces that the `sort` function is reentrant.
(define sort/p (make-contract-parameter any/c))
(define sort/c
  (and/c (-> list? (-> any/c any/c boolean?) list?)
         (parameterize/c [sort/p none/c])
         (apply/c [sort/p #t])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (define/contract sort* sort/c sort)

  (chk
   (sort* '(5 1 3 5 2) <)
   '(1 2 3 5 5)

   #:x
   (sort* '(5 1)
          (λ (x y)
            (sort* '(5 1) <)
            (< x y)))
   "none/c allows no values"
   ))
