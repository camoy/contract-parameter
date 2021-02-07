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
(struct contract-parameter (parameter explicit-name)
  #:property prop:contract
  (build-contract-property
   #:name (η contract-parameter-name)
   #:late-neg-projection (η contract-parameter-late-neg)))

;; Constructor for contract parameters. The actual parameter contains a list with
;; the dynamically bound value and the accumulator box (if applicable).
(define (make-contract-parameter ctc [explicit-name #f])
  (define ctc* (coerce-contract 'make-contract-parameter ctc))
  (contract-parameter (make-parameter (list ctc* #f))
                      explicit-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contract parameter operations

;; Returns the name of the parameter which is based on its contents.
(define (contract-parameter-name ctc)
  (match-define (contract-parameter param name) ctc)
  (match-define (list ctc/proc _) (param))
  (cond
    [name name]
    [(contract? ctc/proc) (contract-name ctc/proc)]
    [else (object-name ctc/proc)]))

;; Returns the late neg projection for contract parameters. If the parameter
;; value is a contract, it just applies that. Otherwise, it updates the
;; accumulator and rejects the carrier based on that.
(define (contract-parameter-late-neg ctc)
  (match-define (contract-parameter param _) ctc)
  (λ (blm)
    (λ (val neg)
      (match-define (list ctc/proc acc-box) (param))
      (cond
        [(contract? ctc/proc)
         (((get/build-late-neg-projection ctc/proc) blm) val neg)]
        [else
         (define acc* (ctc/proc val (unbox acc-box)))
         (unless acc*
           (raise-blame-error blm
                              #:missing-party neg
                              val
                              "contract parameter state became false"))
         (set-box! acc-box acc*)
         val]))))
