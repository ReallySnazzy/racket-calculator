#lang racket

(require "calc-gui.rkt")
(require "tokens.rkt")
(require "evaluate.rkt")

(define (calc-callback expression-str)
  (evaluate (tokens expression-str))
)

(show-calc calc-callback)