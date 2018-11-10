#lang racket/base

(require playground/client/input
         playground/client/player
         playground/client/sprite
         playground/client/sprite-grid
         racket/flonum
         racket/function)

(provide (all-defined-out))

(define (run-simulation P I grid dt)
  (define speed (player-speed P))
  (define-values (vx vy) (input->v2 I))
  (define dx (fl* (fl* speed vx) dt))
  (define dy (fl* (fl* speed vy) dt))
  (define walls (sprite-grid-ref grid P))
  (define P*xy (move-player P dx dy))
  (if (not (ormap (curry sprites-collide? P*xy) walls))
      P*xy
      (let ([P*x (move-player P dx 0.)])
        (if (not (ormap (curry sprites-collide? P*x) walls))
            P*x
            (let ([P*y (move-player P 0. dy)])
              (if (not (ormap (curry sprites-collide? P*y) walls)) P*y P))))))
