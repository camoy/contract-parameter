#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         contract-etc
         contract-parameter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Enforces that the `map` function can only call the given function during
;; its dynamic extent.
(define map/c
  (dynamic->d
   (λ _
     (define map/p (make-contract-parameter none/c))
     (and/c (-> (apply/c [map/p #t]) list? list?)
            (parameterize/c [map/p any/c])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (define/contract good-map map/c map)

  (define cache #f)
  (define/contract (bad-map f xs) map/c
    (set! cache f)
    (map f xs))

  (chk
   (good-map add1 '(5 1 3 5 2))
   '(6 2 4 6 3)

   (bad-map add1 '(5 1))
   '(6 2)
   #:x
   (cache 1)
   "none/c allows no values"

   #:do (set! cache #f)
   #:x
   (bad-map (λ (x)
              (bad-map add1 '(5 1))
              (cache 1)
              (add1 x))
            '(5 1))
   "none/c allows no values"
   ))
