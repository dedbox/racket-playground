#lang racket/base

(require playground/client/player
         playground/client/sprite
         racket/class
         racket/flonum
         racket/function
         racket/match)

(provide (all-defined-out))

(define (run-simulation player1 walls dt)
  (match player1
    [(sprite bitmap x y)
     (let*-values ([(v1 v2) (player-force player1)])
       (define x* (fl+ x (fl* v1 dt)))
       (define y* (fl+ y (fl* v2 dt)))
       (set-sprite-x! player1 x*)
       (set-sprite-y! player1 y*)
       (when (ormap (curry sprites-collide? player1) walls)
         (set-sprite-y! player1 y)
         (when (ormap (curry sprites-collide? player1) walls)
           (set-sprite-y! player1 y*)
           (set-sprite-x! player1 x)
           (when (ormap (curry sprites-collide? player1) walls)
             (set-sprite-y! player1 y)))))]))

(define (sprites-collide? sprite1 sprite2)
  (let ([d1x (fl- (sprite-x-min sprite2) (sprite-x-max sprite1))]
        [d1y (fl- (sprite-y-min sprite2) (sprite-y-max sprite1))]
        [d2x (fl- (sprite-x-min sprite1) (sprite-x-max sprite2))]
        [d2y (fl- (sprite-y-min sprite1) (sprite-y-max sprite2))])
    (and (fl> 0. d1x) (fl> 0. d1y) (fl> 0. d2x) (fl> 0. d2y))))
