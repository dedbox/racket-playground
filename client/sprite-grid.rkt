#lang racket/base

(require playground/client/grid
         playground/client/sprite)

(provide (all-defined-out))

(define (sprite-grid-add! G S)
  (grid-add! G (sprite-x S) (sprite-y S) S))

(define (sprite-grid-ref G S)
  (grid-ref G (sprite-x S) (sprite-y S)))
