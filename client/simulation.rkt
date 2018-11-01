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
    [(sprite bitmap x1 x2)
     (let*-values ([(v1 v2) (player-force P)])
       (define x1* (fl+ x1 (fl* v1 dt)))
       (define x2* (fl+ x2 (fl* v2 dt)))
       (set-sprite-x1! P x1*)
       (set-sprite-x2! P x2*)
       (when (ormap (curry sprites-collide? P) Ws)
         (set-sprite-x2! P x2)
         (when (ormap (curry sprites-collide? P) Ws)
           (set-sprite-x2! P x2*)
           (set-sprite-x1! P x1)
           (when (ormap (curry sprites-collide? P) Ws)
             (set-sprite-x2! P x2)))))]))

(define (sprites-collide? P W)
  (match* (P W)
    [((sprite P-bitmap P-x1 P-x2)
      (sprite W-bitmap W-x1 W-x2))
     (let* ([P-w/2 (fl/ (->fl (send P-bitmap get-width))  2.)]
            [P-h/2 (fl/ (->fl (send P-bitmap get-height)) 2.)]
            [W-w/2 (fl/ (->fl (send W-bitmap get-width))  2.)]
            [W-h/2 (fl/ (->fl (send W-bitmap get-height)) 2.)]
            ;; ------------------------
            [P-x1-min (fl- P-x1 P-w/2)]
            [P-x1-max (fl+ P-x1 P-w/2)]
            [P-x2-min (fl- P-x2 P-h/2)]
            [P-x2-max (fl+ P-x2 P-h/2)]
            [W-x1-min (fl- W-x1 W-w/2)]
            [W-x1-max (fl+ W-x1 W-w/2)]
            [W-x2-min (fl- W-x2 W-h/2)]
            [W-x2-max (fl+ W-x2 W-h/2)]
            ;; ------------------------
            [d1x1 (fl- W-x1-min P-x1-max)]
            [d1x2 (fl- W-x2-min P-x2-max)]
            [d2x1 (fl- P-x1-min W-x1-max)]
            [d2x2 (fl- P-x2-min W-x2-max)])
       (and (fl> 0. d1x1) (fl> 0. d1x2) (fl> 0. d2x1) (fl> 0. d2x2)))]))
