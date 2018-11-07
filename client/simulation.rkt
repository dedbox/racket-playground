#lang racket/base

(require playground/client/player
         playground/client/sprite
         playground/client/sprite-grid
         racket/class
         racket/flonum
         racket/function
         racket/match)

(provide (all-defined-out))

(define (run-simulation player1 grid dt)
  (match player1
    [(sprite bitmap x y)
     (let*-values ([(v1 v2) (player-force player1)])
       (define x* (fl+ x (fl* v1 dt)))
       (define y* (fl+ y (fl* v2 dt)))
       (set-sprite-x! player1 x*)
       (set-sprite-y! player1 y*)
       (when (ormap (curry sprites-collide? player1)
                    (sprite-grid-ref grid player1))
         (set-sprite-y! player1 y)
         (when (ormap (curry sprites-collide? player1)
                      (sprite-grid-ref grid player1))
           (set-sprite-y! player1 y*)
           (set-sprite-x! player1 x)
           (when (ormap (curry sprites-collide? player1)
                        (sprite-grid-ref grid player1))
             (set-sprite-y! player1 y)))))]))
