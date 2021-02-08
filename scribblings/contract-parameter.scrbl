#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@require[@for-label["../main.rkt"
                    racket/base
                    racket/contract
                    racket/function]
         racket/sandbox
         scribble/example
         scriblib/autobib]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; example evaluator

@(define evaluator
  (make-base-eval
    '(require racket/contract
              racket/function
              contract-parameter)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; bibliography

@(define-cite ~cite citet generate-bibliography
   #:style author+date-square-bracket-style
   #:cite-author cite-author
   #:cite-year cite-year)

@(define scholliers-2013
   (make-bib
    #:title "Computational Contracts"
    #:author (authors "Christophe Scholliers"
                      "Éric Tanter"
                      "Wolfgang De Meuter")
    #:location (proceedings-location "Science of Computer Programming")
    #:date 2013))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Contract Parameters}
@author{Cameron Moy}

@defmodule[contract-parameter]

@margin-note{
This library is experimental;
compatibility may not be maintained.
}

This package implements contract parameters
that allow for dynamically bound contracts.
Contract parameters can be used to
implement checks that are similar to
computational contracts@~cite[scholliers-2013].

@section{Example: Prohibit Contract}

Suppose you want a contract that prevents
calling a certain family of functions---perhaps
forbidding a certain
function from printing to the screen.
Enforcing such a restriction requires cooperation
between the function to be prohibited
and its context.
Contract parameters allow this cooperation by
allowing the context to dynamically bind a
contract.

First,
create a contract parameter
called @racket[prohibitable/c].
What it checks depends on the context.
By default,
it checks @racket[integer?].

@examples[#:eval evaluator #:no-result
(define prohibitable/c (make-contract-parameter integer?))]

Next,
define a @racket[prohibit/c] contract
that will instantiate the @racket[prohibitable/c] parameter
with @racket[none/c].
Thus,
if you attach @racket[prohibit/c]
to a function,
then during that function's execution
@racket[prohibitable/c] will be bound to @racket[none/c].

@examples[#:eval evaluator #:no-result
(define prohibit/c
  (parameterize/c ([prohibitable/c none/c])
    any/c))]

Using @racket[prohibitable/c] in a function contract
will allow you to prohibit it.
In this case,
we can check that @racket[will-prevent]
will never be called.

@examples[#:eval evaluator #:no-result
(define/contract (will-prevent x)
  (-> prohibitable/c any)
  (add1 x))

(define/contract (good-prohibit)
  prohibit/c
  43)

(define/contract (bad-prohibit)
  prohibit/c
  (will-prevent 42))]

Here's what it looks like if you try
to call @racket[good-prohibit]
and @racket[bad-prohibit]:

@examples[#:eval evaluator #:label #f
  (good-prohibit)
  (eval:error (bad-prohibit))]

@section{Example: Exactly-Once Contract}

Suppose instead we wish to enforce that a particular
function is called exactly once.
For this,
the parameterization needs to thread some state
when a value flows through a target call site.
The following parameterize contract does that:

@examples[#:eval evaluator #:no-result
(define once/c (make-contract-parameter integer?))
(define exactly-once/c
  (parameterize/c ([once/c (λ (val state) (add1 state))]
                   #:before (const 0)
                   #:after (λ (state) (= state 1)))
    any/c))]

Instead of dynamically binding to a contract,
we bind to a two-argument procedure that updates a
piece of state.
When this state becomes @racket[#false],
the contract will signal an error.
Using a before clause, we initialize the state to @racket[0].
Every time a value flows through the given parameter,
the state will be incremented with @racket[add1].
After the function returns,
we ensure the state is @racket[1].

Attaching @racket[once/c] to a function
allows it to become the target of dynamic binding.

@examples[#:eval evaluator #:no-result
(define/contract (will-count x)
  (-> once/c any)
  (add1 x))

(define/contract (good-once)
  exactly-once/c
  (will-count 43))

(define/contract (bad-once)
  exactly-once/c
  (+ (will-count 20) (will-count 20)))]

Here's what it looks like if you try
to call @racket[good-once]
and @racket[bad-once]:

@examples[#:eval evaluator #:label #f
  (good-once)
  (eval:error (bad-once))]

This enforces a "late" check
that only raises an error after the function returns.
We can also write an "early" check
that raises an error as soon as there is a second call.
Such a strategy can be enforced by
inspecting the state every time it's updated
instead of just once after the function returns.

@examples[#:eval evaluator #:no-result
(define exactly-once-early/c
  (parameterize/c ([once/c (λ (val state)
                             (and (zero? state)
                                  (add1 state)))]
                   #:before (const 0))
    any/c))]

@section{Reference}

@defform[(parameterize/c (clause ...) body)
         #:grammar
         [(clause (code:line [contract-parameter-expr value-expr])
                  (code:line #:before before-expr)
                  (code:line #:after after-expr))]]{
  Returns a parameterize contract that protects a procedure
  with the @racket[body] contract.
  Within the dynamic extent of this procedure,
  each of the contract parameters will be bound to the given values.

  These values can be either a contract
  or a two-argument procedure.
  If it's a two-argument procedure,
  the first argument is the contract carrier
  and the second is the previous state.
  Such a procedure is expected to return
  the new state
  or @racket[#false] to signal a contract error.

  The before and after clauses are executed
  before and after the function runs respectively.
  Each is a one-argument function that is given
  the current state (initially @racket[#false])
  and is expected to update it.
  These clauses are run sequentially.
}

@defproc[(make-contract-parameter [contract contract?]
                                  [name symbol? #f])
         contract-parameter?]{
  Returns a contract parameter with a default contract.
  Such a contract can be dynamically bound with
  @racket[parameterize/c].
}

@defproc[(parameterize-contract? [v any/c]) boolean?]{
  Returns whether the argument is a parameterize contract.
}

@defproc[(contract-parameter? [v any/c]) boolean?]{
  Returns whether the argument is a contract parameter.
}

@generate-bibliography[]
