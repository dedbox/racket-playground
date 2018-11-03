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

(define (sprites-collide? P W)
  (match* (P W)
    [((sprite P-bitmap P-x P-y)
      (sprite W-bitmap W-x W-y))
     (let* ([P-w/2 (fl/ (->fl (send P-bitmap get-width))  2.)]
            [P-h/2 (fl/ (->fl (send P-bitmap get-height)) 2.)]
            [W-w/2 (fl/ (->fl (send W-bitmap get-width))  2.)]
            [W-h/2 (fl/ (->fl (send W-bitmap get-height)) 2.)]
            ;; ------------------------
            [P-x-min (fl- P-x P-w/2)]
            [P-x-max (fl+ P-x P-w/2)]
            [P-y-min (fl- P-y P-h/2)]
            [P-y-max (fl+ P-y P-h/2)]
            [W-x-min (fl- W-x W-w/2)]
            [W-x-max (fl+ W-x W-w/2)]
            [W-y-min (fl- W-y W-h/2)]
            [W-y-max (fl+ W-y W-h/2)]
            ;; ------------------------
            [d1x (fl- W-x-min P-x-max)]
            [d1y (fl- W-y-min P-y-max)]
            [d2x (fl- P-x-min W-x-max)]
            [d2y (fl- P-y-min W-y-max)])
       (and (fl> 0. d1x) (fl> 0. d1y) (fl> 0. d2x) (fl> 0. d2y)))]))
