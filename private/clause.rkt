#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out binding-clause)
         (struct-out surround-clause)
         (struct-out before-clause)
         (struct-out after-clause)
         clause-name
         surround-clause-check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/contract
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

;; A `binding-clause` associates a contract parameter to a value.
;;   - `parameter` is a contract parameter to dynamically bind.
;;   - `value` is the dynamically bound value,
;;   - `parameter-stx` is parameter syntax for name reporting,
;;   - `value-stx` is value syntax for name reporting.
(struct binding-clause (parameter value parameter-stx value-stx))

;; A `surround-clause` is the super struct for checks that surround the
;; dynamic extent of the carrier of a parameterize contract.
;;   - `procedure` updates the accumulator,
;;   - `procedure-stx` is procedure syntax for name reporting.
(struct surround-clause (procedure procedure-stx))

;; A `before-clause` runs before a parameterize carrier is called.
(struct before-clause surround-clause ())

;; An `after-clause` runs before a parameterize carrier is called.
(struct after-clause surround-clause ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clause operations

;; Returns the name of each clause.
(define (clause-name clause)
  (cond
    [(binding-clause? clause)
     (match-define (binding-clause _ _ parameter-stx value-stx) clause)
     (list (list (syntax->datum parameter-stx)
                 (syntax->datum value-stx)))]
    [(before-clause? clause)
     (list '#:before (syntax->datum (surround-clause-procedure-stx clause)))]
    [(after-clause? clause)
     (list '#:after (syntax->datum (surround-clause-procedure-stx clause)))]))

;; Checks that the surround clause didn't return a false accumulator. If it did,
;; blame the procedure itself.
(define (surround-clause-check clause blm val neg acc-box)
  (define acc* ((surround-clause-procedure clause) (unbox acc-box)))
  (set-box! acc-box acc*)
  (unless acc*
    (define before-or-after (if (before-clause? clause) "before" "after"))
    (raise-blame-error blm
                       #:missing-party neg
                       val
                       "~a clause failed"
                       before-or-after)))
