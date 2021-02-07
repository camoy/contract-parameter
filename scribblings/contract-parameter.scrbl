#lang scribble/manual

@require[@for-label["../main.rkt"
                    racket/base
                    racket/contract
                    racket/function]
         racket/sandbox
         scribble/example]

@(define evaluator
  (make-base-eval
    '(require racket/contract
              racket/function
              contract-parameter)))

@title{Contract Parameter}
@author{Cameron Moy}

@defmodule[contract-parameter]

@section{Prohibit Contract}

Create a contract parameter that allows us to
target calls,
as well as a @racket[prohibit/c] combinator
that will instantiate that parameter with
@racket[none/c].

@examples[#:eval evaluator #:no-result
(define prohibitable/c
  (make-contract-parameter integer?))

(define (prohibit/c ctc-param ctc)
  (parameterize/c ([ctc-param none/c]) ctc))]

Can prohibit calling @racket[will-prevent]
by giving it a contract.

@examples[#:eval evaluator #:no-result
(define/contract (will-prevent x)
  (-> prohibitable/c any)
  (add1 x))

(define/contract (good-prohibit)
  (prohibit/c prohibitable/c (-> integer?))
  43)

(define/contract (bad-prohibit)
  (prohibit/c prohibitable/c (-> integer?))
  (will-prevent 42))]

Here's calling those in action.

@examples[#:eval evaluator #:label #f
  (good-prohibit)
  (eval:error (bad-prohibit))]

@section{Exactly-Once Contract}

We want to enforce a call is exactly once.
The parameterize contract can thread some state
when values flow through the instance.
Here, before clause initializes this state to @racket[0].
When a value flows through the dynamically bound contract,
it increments that state.
After the function ends,
we ensure the state is @racket[1].

@examples[#:eval evaluator #:no-result
(define once/c
  (make-contract-parameter integer?))

(define (exactly-once/c ctc-param ctc)
  (parameterize/c ([ctc-param (λ (val acc) (add1 acc))]
                   #:before (const 0)
                   #:after (λ (x) (= x 1)))
    ctc))]

Similar to before,
we attach the @racket[once/c] which will count up
the state when values pass through it.

@examples[#:eval evaluator #:no-result
(define/contract (will-count x)
  (-> once/c any)
  (add1 x))

(define/contract (good-once)
  (exactly-once/c once/c (-> integer?))
  (will-count 43))

(define/contract (bad-once)
  (exactly-once/c once/c (-> integer?))
  (+ (will-count 20) (will-count 20)))]

And we get the expected result.

@examples[#:eval evaluator #:label #f
  (good-once)
  (eval:error (bad-once))]

Notice that this enforces a "late" check
that will only raise an error after the function returns.
If the state is ever @racket[#false],
the contract will raise an error.
This allows for an "early" check
that raises an error as soon as there is a
second call.

@examples[#:eval evaluator #:no-result
(define (exactly-once-early/c ctc-param ctc)
  (parameterize/c ([ctc-param (λ (val acc)
                                (and (zero? acc)
                                     (add1 acc)))]
                   #:before (const 0))
    ctc))]

@section{Reference}

@defform[(parameterize/c (clause ...) body)
         #:grammar
         [(clause (code:line [parameter-contract-expr value-expr])
                  (code:line #:before before-expr)
                  (code:line #:after after-expr))]]{
  Creates a parameterize contract.
}

@defproc[(make-contract-parameter [contract contract?]
                                  [name symbol? #f])
         contract-parameter?]{
  Creates a contract parameter.
}

@defproc[(parameterize-contract? [v any/c]) boolean?]{
  Returns whether the argument is a parameterize contract.
}

@defproc[(contract-parameter? [v any/c]) boolean?]{
  Returns whether the argument is a contract parameter.
}
