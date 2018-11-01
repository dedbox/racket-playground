#lang racket/base

(require playground/client/camera
         playground/client/colors
         playground/client/player
         playground/client/simulation
         playground/client/sprite
         racket/class
         racket/flonum
         racket/function
         racket/gui/base
         racket/match)

(provide (all-defined-out))

(define (start-client title)
  (define frame (new frame% [label title]))
  (define P (make-player 800. 0. 0.))
  (define Ws (list (make-sprite 620 20 0. -300.)
                   (make-sprite 620 20 0.  300.)
                   (make-sprite 20 620 -300. 0.)
                   (make-sprite 20 620  300. 0.)))
  (define cam (camera 0. 0.))
  (define fps 0.)
  (define canvas
    (new (class canvas%
           (define (repaint _ dc)
             (send dc set-background black)
             (send dc clear)
             (for-each (curryr draw-sprite cam dc) Ws)
             (draw-sprite P cam dc)
             (send dc set-text-foreground white)
             (send dc draw-text (format "~a" fps) 10 10))
           (define/override (on-size w h)
             (set! cam (camera (fl/ (->fl w) 2.) (fl/ (->fl h) 2.))))
           (define/override (on-char event)
             (match* ((send event get-key-code)
                      (send event get-key-release-code))
               [('left  'press) (set-player-go-left!  P #t)]
               [('right 'press) (set-player-go-right! P #t)]
               [('up    'press) (set-player-go-up!    P #t)]
               [('down  'press) (set-player-go-down!  P #t)]
               [('release  'left) (set-player-go-left!  P #f)]
               [('release 'right) (set-player-go-right! P #f)]
               [('release    'up) (set-player-go-up!    P #f)]
               [('release  'down) (set-player-go-down!  P #f)]
               [(pcode rcode) (writeln `(KEY ,pcode ,rcode))]))
           (super-new [parent frame]
                      [paint-callback repaint]))))
  (send frame show #t)
  (send canvas focus)
  (thread
   (λ ()
     (collect-garbage)
     (let loop ([t (current-inexact-milliseconds)])
       (collect-garbage 'incremental)
       (define t* (current-inexact-milliseconds))
       (define dt (fl- t* t))
       (run-simulation P Ws (fl/ dt 1000.))
       (set! fps (fl+ (fl* fps 0.99) (fl/ 1. dt))) ; 100-sample moving average
       (send canvas refresh-now)
       (sleep)
       (loop t*)))))
