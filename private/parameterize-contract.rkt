#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out parameterize-contract))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         racket/list
         racket/match
         "contract-parameter.rkt"
         "clause.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; A `parameterize-contract` wraps a procedure with a contract that dynamically
;; binds contract parameters within the carrier's dynamic extent.
;;   - `clauses` is a list of clauses,
;;   - `body` is the body contract.
(struct parameterize-contract (clauses body)
  #:property prop:contract
  (build-contract-property
   #:name (η parameterize-contract-name)
   #:late-neg-projection (η parameterize-contract-late-neg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parameterize contract properties

;; Returns the name of the parameterize contract.
(define (parameterize-contract-name pctc)
  (match-define (parameterize-contract clauses body) pctc)
  (define clause-names (append-map clause-name clauses))
  `(parameterize/c ,clause-names
                   ,(contract-name body)))

;; Returns the late neg projection for parameterize contracts.
(define (parameterize-contract-late-neg pctc)
  (match-define (parameterize-contract clauses body) pctc)
  (define befores (filter before-clause? clauses))
  (define bindings (filter binding-clause? clauses))
  (define afters (filter after-clause? clauses))
  (define late-neg (get/build-late-neg-projection body))
  (λ (blm)
    (define blm* (blame-add-context blm "the body of"))
    (define proj (late-neg blm*))
    (λ (raw-proc neg)
      (define proc (proj raw-proc neg))
      (λ args
        (define acc-box (box #f))
        (for ([before (in-list befores)])
          (surround-clause-check before blm* raw-proc neg acc-box))
        (define returns
          (let go ([bindings bindings])
            (match bindings
              ['()
               (call-with-values (λ () (apply proc args)) list)]
              [(cons (binding-clause ctc-param ctc/proc _ _) bindings-rest)
               (define param (contract-parameter-parameter ctc-param))
               (define ctc/proc*
                 (if (contract? ctc/proc)
                     (coerce-contract 'parameterize/c ctc/proc)
                     ctc/proc))
               (define val (list ctc/proc* acc-box))
               (parameterize ([param val])
                 (go bindings-rest))])))
        (for ([after (in-list afters)])
          (surround-clause-check after blm* raw-proc neg acc-box))
        (apply values returns)))))
