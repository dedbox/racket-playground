#lang racket/base

(require playground/client/player
         playground/client/sprite
         racket/flonum)

(provide (all-defined-out))

(define (run-simulation P W dt)
  (define-values (f1 f2) (player-force P))
  (define a1 (fl/ (fl* f1 dt) (sprite-m P)))
  (define a2 (fl/ (fl* f2 dt) (sprite-m P)))
  (set-sprite-v1! P (fl+ (sprite-v1 P) (fl* a1 dt)))
  (set-sprite-v2! P (fl+ (sprite-v2 P) (fl* a2 dt)))
  (set-sprite-x1! P (fl+ (sprite-x1 P) (fl* (sprite-v1 P) dt)))
  (set-sprite-x2! P (fl+ (sprite-x2 P) (fl* (sprite-v2 P) dt))))
