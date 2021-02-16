#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide parameterize/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         "binding.rkt"
         "contract-parameter.rkt"
         "parameterize-contract.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax classes

(begin-for-syntax
  ;; A `parameterize/c` binding.
  (define-splicing-syntax-class binding
    #:description "parameterize/c binding"
    #:attributes (norm)
    (pattern [param val:expr]
             #:declare param
             (expr/c #'(or/c contract-parameter? parameter?)
                     #:name "contract parameter or parameter")
             #:with norm
             #'(make-binding param.c val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

;; Macro `parameterize/c` constructs a parameterize contract.
(define-syntax parameterize/c
  (syntax-parser
    [(_ b:binding ...+)
     #'(parameterize-contract (list b.norm ...))]))
