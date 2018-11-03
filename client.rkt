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
  (define Fs (list (sprite (dummy-bitmap 600 600 green black) 0. 0.)))
  (define Ws
    (list* (sprite (dummy-bitmap 600 20) 0. -300.)
           (sprite (dummy-bitmap 600 20) 0.  300.)
           (sprite (dummy-bitmap 20 600) -300. 0.)
           (sprite (dummy-bitmap 20 600)  300. 0.)
           (sprite (dummy-bitmap 20 20) -300. -300.)
           (sprite (dummy-bitmap 20 20) -300.  300.)
           (sprite (dummy-bitmap 20 20)  300. -300.)
           (sprite (dummy-bitmap 20 20)  300.  300.)
           (for*/list ([x1 (in-range -200 201 100)]
                       [x2 (in-range -200 201 100)])
             (sprite (dummy-bitmap 20 20) (->fl x1) (->fl x2)))))
  (define P (make-player (dummy-bitmap 20 20 red) 50. 50. 800.))
  (define cam (camera 0. 0.))
  (define fps 0.)
  (define canvas
    (new (class canvas%
           (define (repaint _ dc)
             (send dc set-background black)
             (send dc clear)
             (for-each (curryr draw-sprite cam dc) Fs)
             (for-each (curryr draw-sprite cam dc) Ws)
             (draw-sprite P cam dc)
             (send dc set-text-foreground white)
             (send dc draw-text (real->decimal-string fps 1) 10 10))
           (define/override (on-size w h)
             (set! cam (camera (fl/ (->fl w) 2.) (fl/ (->fl h) 2.))))
           (define ctrl-down? #f)
           (define/override (on-char event)
             (match* ((send event get-key-code)
                      (send event get-key-release-code))
               ;; --------------------------------------------
               ;; player movement
               [('left  'press) (set-player-go-left!  P #t)]
               [('right 'press) (set-player-go-right! P #t)]
               [('up    'press) (set-player-go-up!    P #t)]
               [('down  'press) (set-player-go-down!  P #t)]
               [('release  'left) (set-player-go-left!  P #f)]
               [('release 'right) (set-player-go-right! P #f)]
               [('release    'up) (set-player-go-up!    P #f)]
               [('release  'down) (set-player-go-down!  P #f)]
               ;; --------------------------------------------
               ;; modifiers
               [('control 'press) (set! ctrl-down? #t)]
               [('release 'control) (set! ctrl-down? #f)]
               ;; --------------------------------------------
               ;; commands
               [(#\q 'press) (when ctrl-down? (exit))]
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
