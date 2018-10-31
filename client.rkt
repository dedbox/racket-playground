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
  (define P (make-player 1500000. -100000. 0. 0.))
  (define Ws (list (make-sprite 620 20 0. -300. 0. 0. 1.)
                   (make-sprite 620 20 0.  300. 0. 0. 1.)
                   (make-sprite 20 620 -300. 0. 0. 0. 1.)
                   (make-sprite 20 620  300. 0. 0. 0. 1.)))
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
               [('left  'press) (push-player-left!  P)]
               [('right 'press) (push-player-right! P)]
               [('up    'press) (push-player-up!    P)]
               [('down  'press) (push-player-down!  P)]
               [('release  'left) (drag-player-left!  P)]
               [('release 'right) (drag-player-right! P)]
               [('release    'up) (drag-player-up!    P)]
               [('release  'down) (drag-player-down!  P)]
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
       (set! fps (fl+ (fl* fps 0.99) (fl/ 1. dt)))
       (send canvas refresh-now)
       (sleep)
       (loop t*)))))
