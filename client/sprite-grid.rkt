#lang racket/base

(require playground/client/grid
         playground/client/sprite)

(provide (all-defined-out))

(define (sprite-grid-ref G S)
  (let ([x-min (sprite-x-min S)]
        [y-min (sprite-y-min S)]
        [x-max (sprite-x-max S)]
        [y-max (sprite-y-max S)])
    (grid-area-ref G x-min y-min x-max y-max)))

(define (sprite-grid-add! G S)
  (let ([x-min (sprite-x-min S)]
        [y-min (sprite-y-min S)]
        [x-max (sprite-x-max S)]
        [y-max (sprite-y-max S)])
    (grid-area-add! G x-min y-min x-max y-max S)))
