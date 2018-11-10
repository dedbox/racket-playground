#lang racket/base

(require playground/client/sprite
         racket/flonum)

(provide (all-defined-out))

(struct player sprite (speed) #:transparent)

(define (move-player P dx dy)
  (struct-copy player P
               [x #:parent sprite (fl+ (sprite-x P) dx)]
               [y #:parent sprite (fl+ (sprite-y P) dy)]))
