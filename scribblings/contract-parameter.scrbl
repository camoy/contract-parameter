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
it checks @racket[any/c].

@examples[#:eval evaluator #:no-result
(define prohibitable/c (make-contract-parameter any/c))]

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
(define prohibit/c (parameterize/c [prohibitable/c none/c]))]

Using @racket[prohibitable/c] in a function contract
will allow you to prohibit it.
In this case,
we can check that @racket[will-prevent]
will never be called.

@examples[#:eval evaluator #:no-result
(define/contract (will-prevent x)
  (-> (and/c integer? prohibitable/c) any)
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

@section{Reference}

@defform[(parameterize/c [contract-parameter-expr contract-expr] ...+)]{
  Returns a parameterize contract that protects a procedure.
  Within the dynamic extent of this procedure,
  each of the contract parameters will be bound to the given contract.
}

@defproc[(make-contract-parameter [contract contract?]
                                  [name symbol? '???])
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
