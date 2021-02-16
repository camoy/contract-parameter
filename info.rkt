#lang info

;; General

(define collection "contract-parameter")
(define pkg-desc "Dynamically bound contracts.")
(define version "0.0")
(define pkg-authors '(camoy))
(define scribblings '(("scribblings/contract-parameter.scrbl" ())))

;; Dependencies

(define deps
  '("contract-etc"
    "base"))

(define build-deps
  '("chk-lib"
    "sandbox-lib"
    "scribble-lib"
    "racket-doc"
    "rackunit-lib"))
