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
;;   - `parameter` is a contract parameter to dynamically bind,
;;   - `contract` is the dynamically bound contract.
(struct binding (parameter contract))

;; Constructor for bindings.
(define (make-binding param ctc)
  (binding param (coerce-contract 'make-binding ctc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; binding operations

;; Returns the name of the binding.
(define (binding-name b)
  (match-define (binding param ctc) b)
  (list (contract-parameter-name param)
        (contract-name ctc)))
