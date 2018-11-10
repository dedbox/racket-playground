#lang racket/base

(require playground/client/camera
         playground/client/colors
         playground/client/grid
         playground/client/input
         playground/client/player
         playground/client/simulation
         playground/client/sprite
         playground/client/sprite-grid
         racket/class
         racket/flonum
         racket/function
         racket/gui/base
         racket/match)

(provide (all-defined-out))

(define (start-client title)
  (define frame (new frame% [label title]))
  (define roofs (list (sprite (dummy-bitmap 20 20 orange) -300. -300.)
                      (sprite (dummy-bitmap 20 20 orange) -300.  300.)
                      (sprite (dummy-bitmap 20 20 orange)  300. -300.)
                      (sprite (dummy-bitmap 20 20 orange)  300.  300.)))
  (define floors (list (sprite (dummy-bitmap 600 600 green black) 0. 0.)))
  (define walls (list* (sprite (dummy-bitmap 600 20) 0. -300.)
                       (sprite (dummy-bitmap 600 20) 0.  300.)
                       (sprite (dummy-bitmap 20 600) -300. 0.)
                       (sprite (dummy-bitmap 20 600)  300. 0.)
                       (for*/list ([x (in-range -200. 201. 100.)]
                                   [y (in-range -200. 201. 100.)])
                         (sprite (dummy-bitmap 20 20) x y))))
  (define grid (make-grid -320. -320. 320. 320. 100. 100.))
  (for-each (curry sprite-grid-add! grid) walls)
  (define cam (camera 0. 0.))
  (define input1 (input #f #f #f #f))
  (define canvas
    (new (class canvas%
           (define/override (on-size w h)
             (set! cam (camera (fl/ (->fl w) 2.) (fl/ (->fl h) 2.))))
           (define mod? #f)
           (define/override (on-char event)
             (match* ((send event get-key-code)
                      (send event get-key-release-code))
               ;; player movement
               [('left  'press) (set-input-go-left?!  input1 #t)]
               [('right 'press) (set-input-go-right?! input1 #t)]
               [('up    'press) (set-input-go-up?!    input1 #t)]
               [('down  'press) (set-input-go-down?!  input1 #t)]
               [('release  'left) (set-input-go-left?!  input1 #f)]
               [('release 'right) (set-input-go-right?! input1 #f)]
               [('release    'up) (set-input-go-up?!    input1 #f)]
               [('release  'down) (set-input-go-down?!  input1 #f)]
               ;; modifiers
               [('control 'press)   (set! mod? #t)]
               [('release 'control) (set! mod? #f)]
               ;; global commands
               [(#\q 'press) #:when mod? (exit)]
               ;; everything else
               [(pcode rcode) (writeln `(KEY ,pcode ,rcode))]))
           (super-new [parent frame]))))
  (send frame show #t)
  (send canvas focus)
  (define (repaint P fps)
    (λ (dc)
      (send dc set-background black)
      (send dc clear)
      (for-each (curryr draw-sprite cam dc) floors)
      (for-each (curryr draw-sprite cam dc) walls)
      (draw-sprite P cam dc)
      (for-each (curryr draw-sprite cam dc) roofs)
      (send dc set-text-foreground white)
      (send dc draw-text (real->decimal-string fps 1) 10 10)
      (send dc draw-text (format "[~a ~a // ~a ~a]"
                                 (real->decimal-string (sprite-x-min P) 1)
                                 (real->decimal-string (sprite-y-min P) 1)
                                 (real->decimal-string (sprite-x-max P) 1)
                                 (real->decimal-string (sprite-y-max P) 1))
            100 10)))
  (thread
   (λ ()
     (collect-garbage)
     (let loop ([t (current-inexact-milliseconds)]
                [P (player (dummy-bitmap 20 20 red) 50. 50. 700.)]
                [fps 0.])
       (collect-garbage 'incremental)
       (define t* (current-inexact-milliseconds))
       (define dt (fl- t* t))
       (define P* (run-simulation P input1 grid (fl/ dt 1000.)))
       (define fps* (fl+ (fl* fps 0.99) (fl/ 1. dt))) ; 100-sample moving average
       (send canvas refresh-now (repaint P* fps*))
       (sleep)
       (loop t* P* fps*)))))
