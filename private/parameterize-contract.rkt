#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out parameterize-contract))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         racket/list
         racket/match
         "binding.rkt"
         "contract-parameter.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; A `parameterize-contract` wraps a procedure with a contract that dynamically
;; binds contract parameters within the carrier's dynamic extent.
;;   - `binding` is a list of bindings.
(struct parameterize-contract (bindings)
  #:property prop:contract
  (build-contract-property
   #:name (η parameterize-contract-name)
   #:first-order procedure?
   #:late-neg-projection (η parameterize-contract-late-neg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameterize contract properties

;; Returns the name of the parameterize contract.
(define (parameterize-contract-name pctc)
  (define bindings (parameterize-contract-bindings pctc))
  (cons 'parameterize/c (map binding-name bindings)))

;; Returns the late neg projection for parameterize contracts.
(define (parameterize-contract-late-neg pctc)
  (define bindings (parameterize-contract-bindings pctc))
  (λ (blm)
    (λ (proc neg)
      (unless (procedure? proc)
        (raise-blame-error
         blm proc
         '(expected: "procedure?" given: "~e") proc))
      (λ args
        (let go ([bindings bindings])
          (match bindings
            ['() (apply proc args)]
            [(cons (binding (contract-parameter param _) ctc) bindings-rest)
             (parameterize ([param ctc])
               (go bindings-rest))]))))))
