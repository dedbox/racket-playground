#lang racket/base

(require playground/client/player
         playground/client/sprite
         racket/class
         racket/flonum
         racket/function
         racket/match)

(provide (all-defined-out))

(define (run-simulation P Ws dt)
  (match P
    [(sprite bitmap x y)
     (let*-values ([(v1 v2) (player-force P)])
       (define x* (fl+ x (fl* v1 dt)))
       (define y* (fl+ y (fl* v2 dt)))
       (set-sprite-x! P x*)
       (set-sprite-y! P y*)
       (when (ormap (curry sprites-collide? P) Ws)
         (set-sprite-y! P y)
         (when (ormap (curry sprites-collide? P) Ws)
           (set-sprite-y! P y*)
           (set-sprite-x! P x)
           (when (ormap (curry sprites-collide? P) Ws)
             (set-sprite-y! P y)))))]))

(define (sprites-collide? sprite1 sprite2)
  (let ([d1x (fl- (sprite-x-min sprite2) (sprite-x-max sprite1))]
        [d1y (fl- (sprite-y-min sprite2) (sprite-y-max sprite1))]
        [d2x (fl- (sprite-x-min sprite1) (sprite-x-max sprite2))]
        [d2y (fl- (sprite-y-min sprite1) (sprite-y-max sprite2))])
    (and (fl> 0. d1x) (fl> 0. d1y) (fl> 0. d2x) (fl> 0. d2y))))
