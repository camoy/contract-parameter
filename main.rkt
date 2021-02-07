#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/match)

;; One-argument η-expansion.
(define-syntax-rule (η f)
  (λ (x) (f x)))

(struct parameterize-contract (bindings befores afters body)
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection (η parameterize-contract-late-neg)))

(struct contract-parameter (parameter)
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection (η contract-parameter-late-neg)))

(struct contract-parameter-inner (contract acc-box))

(define (make-contract-parameter ctc)
  (define inner (contract-parameter-inner ctc #f))
  (contract-parameter (make-parameter inner)))

(define (contract-parameter-late-neg ctc)
  (match-define (contract-parameter param) ctc)
  (λ (blm)
    (λ (val neg)
      (match-define (contract-parameter-inner ctc/proc acc-box) (param))
      (cond
        [(contract? ctc/proc)
         (define proj
           (get/build-late-neg-projection
            (coerce-contract 'contract-parameter ctc/proc)))
         ((proj blm) val neg)]
        [else
         (define acc* (ctc/proc val (unbox acc-box)))
         (set-box! acc-box acc*)
         (unless acc*
           (raise-blame-error blm
                              #:missing-party neg
                              val
                              "contract parameter bad"))
         val]))))

(struct binding-clause (parameter value))

(struct surround-clause (procedure))
(struct before-clause surround-clause ())
(struct after-clause surround-clause ())

(define (surround-clause-check clause blm val neg acc-box)
  (define acc* ((surround-clause-procedure clause) (unbox acc-box)))
  (set-box! acc-box acc*)
  (unless acc*
    (raise-blame-error blm
                       #:missing-party neg
                       val
                       "surround bad")))

(define (contract-parameter-value? x)
  (or (contract? x)
      (and (procedure? x)
           (procedure-arity-includes? x 2))))

(begin-for-syntax
  (define-splicing-syntax-class clause
    #:description "parameterize/c clause"
    #:attributes (norm)
    (pattern [param expr]
             #:declare param
             (expr/c #'contract-parameter? #:name "contract parameter")
             #:declare expr (expr/c #'contract-parameter-value?
                                    #:name "contract parameter value")
             #:with norm #'(binding-clause param.c expr.c))

    (pattern (~seq #:before proc)
             #:declare proc (expr/c #'procedure? #:name "procedure")
             #:with norm #'(before-clause proc.c))

    (pattern (~seq #:after proc)
             #:declare proc (expr/c #'procedure? #:name "procedure")
             #:with norm #'(after-clause proc.c))))

(define (make-parameterize-contract clauses body)
  (parameterize-contract (filter binding-clause? clauses)
                         (filter before-clause? clauses)
                         (filter after-clause? clauses)
                         body))

(define (parameterize-contract-late-neg ctc)
  (match-define (parameterize-contract bindings befores afters body) ctc)
  (define late-neg (get/build-late-neg-projection body))
  (λ (blm)
    (define blm* (blame-add-context blm "the body of"))
    (define proj (late-neg blm*))
    (λ (raw-proc neg)
      (define proc (proj raw-proc neg))
      (λ args
        (define acc-box (box #f))
        (for ([before (in-list befores)])
          (surround-clause-check before blm raw-proc neg acc-box))
        (define returns
          (let go ([bindings bindings])
            (match bindings
              ['()
               (call-with-values (λ () (apply proc args)) list)]
              [(cons (binding-clause ctc-param contract) bindings-rest)
               (define param (contract-parameter-parameter ctc-param))
               (define val (contract-parameter-inner contract acc-box))
               (parameterize ([param val])
                 (go bindings-rest))])))
        (for ([after (in-list afters)])
          (surround-clause-check after blm raw-proc neg acc-box))
        (apply values returns)))))

(define-syntax parameterize/c
  (syntax-parser
    [(_ (cl:clause ...) body:expr)
     #'(make-parameterize-contract (list cl.norm ...) body)]))


(module+ test
  (require chk)

  (define foo/c (make-contract-parameter any/c))
  (define (exactly-once/c ctc)
    (define (before _) 0)
    (define (during _ acc) (add1 acc))
    (define (after acc) (= acc 1))
    (parameterize/c ([ctc during]
                     #:before before
                     #:after after)
      any/c))

  (define/contract (foo x)
    (-> foo/c any)
    x)

  (define/contract (bad)
    (parameterize/c ([foo/c none/c]) any/c)
    (foo 42))

  (define/contract (good)
    (parameterize/c ([foo/c integer?]) any/c)
    (foo 42))

  (define/contract (exactly-once-actually-none)
    (exactly-once/c foo/c)
    (void))

  (define/contract (exactly-once-actually-once)
    (exactly-once/c foo/c)
    (foo 42))

  (define/contract (exactly-once-actually-twice)
    (exactly-once/c foo/c)
    (+ (foo 42) (foo 42)))

  (chk
   (foo 42) 42
   (good) 42
   #:x (bad) "none/c allows no values"
   #:x (exactly-once-actually-none) ""
   (exactly-once-actually-once) 42
   #:x (exactly-once-actually-twice) ""
   ))

#;(module+ test
    (define thingy/c (make-contract-parameter natural?))
    (parameterize/c ([thingy/c integer?]
                     #:before something?
                     #:after something-else?)
      (-> integer? integer?))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

;; * contract names
;; * first-order checks
;; * chaperone or not
;; * context for blames
;; * do we need to coerce?
;; * blame error messages
