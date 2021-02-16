#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require contract-parameter
         racket/contract
         racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

;; Parameter that contains hashes that cannot be mutated.
(define current-immutable-hashes (make-parameter (set)))

;; Determines if the given hash is mutable in this context.
(define (mutable-hash? h)
  (not (set-member? (current-immutable-hashes) h)))

;; Prohibit mutation of the given hash during the given procedure.
(define (no-hash-mutation/c h)
  (parameterize/c
   [current-immutable-hashes (set-add (current-immutable-hashes) h)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  ;; Calls the given function on all keys of the hash.
  (define/contract (hash-each-key h f)
    (->i ([h hash?]
          [f (h) (no-hash-mutation/c h)])
         any)
    (for ([k (in-hash-keys h)])
      (f k)))

  ;; `hash-remove!` ensuring that its argument can be mutated.
  (define/contract hash-remove!*
    (-> mutable-hash? any/c any)
    hash-remove!)

  (chk
   #:do (define h-ok (make-hash '((a . 1))))
   #:t (hash-each-key h-ok (λ (k) k))
   #:do (define h-bad (make-hash '((a . 1))))
   #:x
   (hash-each-key h-bad (λ (k) (hash-remove!* h-bad 'a)))
   "expected: mutable-hash?"
   ))

;; Example of bad behavior (doesn't print a):
#|
(define h (make-hash '((a . 1) (b . 1))))
(for ([(k v) (in-hash h)])
  (displayln k)
  (hash-remove! h 'b))
|#
