#lang racket/base

(require playground/client/player
         playground/client/sprite
         racket/class
         racket/flonum
         racket/match)

(provide (all-defined-out))

(define (run-simulation P W dt)
  (match P
    [(sprite bitmap x1 x2 v1 v2 m)
     (let*-values ([(f1 f2) (player-force P)]
                   [(a1) (fl/ (fl* f1 dt) (sprite-m P))]
                   [(a2) (fl/ (fl* f2 dt) (sprite-m P))])
       (set-sprite-v1! P (fl+ (sprite-v1 P) (fl* a1 dt)))
       (set-sprite-v2! P (fl+ (sprite-v2 P) (fl* a2 dt)))
       (define x1* (fl+ (sprite-x1 P) (fl* (sprite-v1 P) dt)))
       (define x2* (fl+ (sprite-x2 P) (fl* (sprite-v2 P) dt)))
       (set-sprite-x1! P x1*)
       (set-sprite-x2! P x2*)
       (when (sprites-collide? P W)
         (set-sprite-x2! P x2)
         (when (sprites-collide? P W)
           (set-sprite-x2! P x2*)
           (set-sprite-x1! P x1)
           (when (sprites-collide? P W)
             (set-sprite-x2! P x2)))))]))

(define (sprites-collide? P W)
  (match* (P W)
    [((sprite P-bitmap P-x1 P-x2 _ _ _)
      (sprite W-bitmap W-x1 W-x2 _ _ _))
     (let* ([P-w/2 (fl/ (->fl (send P-bitmap get-width)) 2.)]
            [P-h/2 (fl/ (->fl (send P-bitmap get-height)) 2.)]
            [W-w/2 (fl/ (->fl (send W-bitmap get-width)) 2.)]
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
