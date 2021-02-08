#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out contract-parameter)
         make-contract-parameter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         racket/match
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; A `contract-parameter` is a value that can be used in `parameterize/c` for
;; dynamic binding.
;;   - `parameter` is the actual parameter that does the binding,
;;   - `name` is a symbol used for name reporting.
(struct contract-parameter (parameter name)
  #:property prop:contract
  (build-contract-property
   #:name (η contract-parameter-name)
   #:late-neg-projection (η contract-parameter-late-neg)))

;; Constructor for contract parameters.
(define (make-contract-parameter ctc [name '???])
  (define ctc* (coerce-contract 'make-contract-parameter ctc))
  (contract-parameter (make-parameter ctc*) name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contract parameter operations

;; Returns the late neg projection for contract parameters.
(define (contract-parameter-late-neg pctc)
  (λ (blm)
    (λ (val neg)
      (define ctc ((contract-parameter-parameter pctc)))
      (((get/build-late-neg-projection ctc) blm) val neg))))
