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
    (pattern [param ctc]
             #:declare param
             (expr/c #'contract-parameter? #:name "contract parameter")
             #:declare ctc
             (expr/c #'contract? #:name "contract")
             #:with norm
             #'(make-binding param.c ctc.c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

;; Macro `parameterize/c` constructs a parameterize contract.
(define-syntax parameterize/c
  (syntax-parser
    [(_ b:binding ...+)
     #'(parameterize-contract (list b.norm ...))]))
