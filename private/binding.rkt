#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out binding)
         make-binding
         binding-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         racket/match
         "contract-parameter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; A `binding` associates a contract parameter to a contract.
;;   - `parameter` is a contract parameter or parameter to dynamically bind,
;;   - `value` is the dynamically bound contract or parameter value.
(struct binding (parameter value))

;; Constructor for bindings.
(define (make-binding param val)
  (binding param
           (if (contract? val)
               (coerce-contract 'make-binding val)
               val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; binding operations

;; Returns the name of the binding.
(define (binding-name b)
  (match-define (binding param val) b)
  (list (contract-parameter-name param)
        (if (contract? val)
            (contract-name val)
            (object-name val))))
