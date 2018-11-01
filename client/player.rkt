#lang racket/base

(require playground/client/colors
         playground/client/sprite
         racket/bool
         racket/flonum
         racket/match)

(provide (all-defined-out))

(struct player sprite (speed go-left go-right go-up go-down)
  #:mutable #:transparent)

(define (make-player bitmap x1 x2 speed)
  (player bitmap x1 x2 speed #f #f #f #f))

(define player-force
  (match-lambda
    [(player _ _ _ s L R U D)
     (define -s (fl- 0. s))
     (match* (L R U D)
       [(#t #f #f #f) (values -s  0.)]
       [(#f #t #f #f) (values  s  0.)]
       [(#f #f #t #f) (values  0. -s)]
       [(#f #f #f #t) (values  0.  s)]
       [(#t #f #t #f) (values -s  -s)]
       [(#t #f #f #t) (values -s   s)]
       [(#f #t #t #f) (values  s  -s)]
       [(#f #t #f #t) (values  s   s)]
       [( _  _  _  _) (values  0. 0.)])]))
