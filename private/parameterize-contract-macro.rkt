#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide parameterize/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         "clause.rkt"
         "contract-parameter.rkt"
         "parameterize-contract.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax classes

(begin-for-syntax
  ;; A `parameterize/c` clause.
  (define-splicing-syntax-class clause
    #:description "parameterize/c clause"
    #:attributes (norm)
    (pattern [param expr]
             #:declare param
             (expr/c #'contract-parameter? #:name "contract parameter")
             #:declare expr (expr/c #'contract-parameter-value?
                                    #:name "contract parameter value")
             #:with norm #'(binding-clause param.c expr.c #'param #'expr))

    (pattern (~seq #:before proc)
             #:declare proc (expr/c #'(procedure-arity-includes/c 1)
                                    #:name "single-argument procedure")
             #:with norm #'(before-clause proc.c #'proc))

    (pattern (~seq #:after proc)
             #:declare proc (expr/c #'(procedure-arity-includes/c 1)
                                    #:name "single-argument procedure")
             #:with norm #'(after-clause proc.c #'proc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

;; Macro `parameterize/c` constructs a parameterize contract.
(define-syntax parameterize/c
  (syntax-parser
    [(_ (cl:clause ...) body:expr)
     #'(parameterize-contract (list cl.norm ...) body)]))

;; Returns if the value is suitable for the value of a contract parameter.
(define (contract-parameter-value? x)
  (or (contract? x)
      (and (procedure? x) (procedure-arity-includes? x 2))))
