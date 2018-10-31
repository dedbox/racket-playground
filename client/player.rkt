#lang racket/base

(require playground/client/sprite
         racket/bool
         racket/flonum
         racket/match)

(provide (all-defined-out))

(struct player sprite (push drag go-left go-right go-up go-down x1-stop x2-stop)
  #:mutable #:transparent)

(define (make-player push drag x1 x2)
  (define S (make-sprite 20 20 x1 x2 0. 0. 1.))
  (player (sprite-bitmap S) x1 x2 0. 0. 1. push drag #f #f #f #f 0. 0.))

(define (push-player-left! P)
  (set-player-go-left! P #t)
  (set-player-x1-stop! P (current-inexact-milliseconds)))

(define (push-player-right! P)
  (set-player-go-right! P #t)
  (set-player-x1-stop! P (current-inexact-milliseconds)))

(define (push-player-up! P)
  (set-player-go-up! P #t)
  (set-player-x2-stop! P (current-inexact-milliseconds)))

(define (push-player-down! P)
  (set-player-go-down! P #t)
  (set-player-x2-stop! P (current-inexact-milliseconds)))

(define (drag-player-left! P)
  (set-player-go-left! P #f)
  (set-player-x1-stop! P (current-inexact-milliseconds)))

(define (drag-player-right! P)
  (set-player-go-right! P #f)
  (set-player-x1-stop! P (current-inexact-milliseconds)))

(define (drag-player-up! P)
  (set-player-go-up! P #f)
  (set-player-x2-stop! P (current-inexact-milliseconds)))

(define (drag-player-down! P)
  (set-player-go-down! P #f)
  (set-player-x2-stop! P (current-inexact-milliseconds)))

(define (player-force P)
  (values
   ((if (is-player-going-x1? P) push-player-x1 drag-player-x1) P)
   ((if (is-player-going-x2? P) push-player-x2 drag-player-x2) P)))

(define (is-player-going-x1? P)
  (define L (player-go-left P))
  (define R (player-go-right P))
  (and (xor L R)
       (fl> (fl- (current-inexact-milliseconds) (player-x1-stop P)) 0.001)))

(define (is-player-going-x2? P)
  (define U (player-go-up P))
  (define D (player-go-down P))
  (and (xor U D)
       (fl> (fl- (current-inexact-milliseconds) (player-x2-stop P)) 0.001)))

(define (push-player-x1 P)
  (fl* (player-push P)
       (match* ((player-go-left P) (player-go-right P))
         [(#t #f) -1.]
         [(#f #t)  1.]
         [( _  _)  0.])))

(define (push-player-x2 P)
  (fl* (player-push P)
       (match* ((player-go-up P) (player-go-down P))
         [(#t #f) -1.]
         [(#f #t)  1.]
         [( _  _)  0.])))

(define (drag-player-x1 P)
  (fl* (fl* (sprite-v1 P) (player-drag P))
       (match* ((player-go-left P) (player-go-right P))
         [(#t #t) 1.]
         [(#f #f) 1.]
         [( _  _) 0.])))

(define (drag-player-x2 P)
  (fl* (fl* (sprite-v2 P) (player-drag P))
       (match* ((player-go-up P) (player-go-down P))
         [(#t #t) 1.]
         [(#f #f) 1.]
         [( _  _) 0.])))
