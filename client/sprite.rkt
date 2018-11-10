#lang racket/base

(require playground/client/camera
         playground/client/colors
         racket/class
         racket/draw
         racket/flonum)

(provide (all-defined-out))

(struct sprite (bitmap x y) #:transparent)

(define (move-sprite S dx dy)
  (struct-copy sprite S [x (fl+ (sprite-x S) dx)] [y (fl+ (sprite-y S) dy)]))

(define (dummy-bitmap w h [bg-color blue] [fg-color white])
  (define bitmap (make-bitmap w h))
  (define dc (send bitmap make-dc))
  (send dc set-pen fg-color 1 'solid)
  (send dc set-brush bg-color 'solid)
  (send dc draw-rectangle 0. 0. (->fl w) (->fl h))
  (send dc draw-line 0. 0. (fl- (->fl w) 1.) (fl- (->fl h) 1.))
  (send dc draw-line 0. (fl- (->fl h) 1.) (fl- (->fl w) 1.) 0.)
  (send dc flush)
  bitmap)

(define (draw-sprite S cam dc)
  (define w (->fl (send (sprite-bitmap S) get-width)))
  (define h (->fl (send (sprite-bitmap S) get-height)))
  (define x (fl- (sprite-x S) (fl/ w 2.)))
  (define y (fl- (sprite-y S) (fl/ h 2.)))
  (define-values (x* y*) (apply-camera cam x y))
  (send dc draw-bitmap (sprite-bitmap S) x* y*))

(define (sprite-x-min S)
  (fl- (sprite-x S) (fl/ (->fl (send (sprite-bitmap S) get-width)) 2.)))

(define (sprite-y-min S)
  (fl- (sprite-y S) (fl/ (->fl (send (sprite-bitmap S) get-height)) 2.)))

(define (sprite-x-max S)
  (fl+ (sprite-x S) (fl/ (->fl (send (sprite-bitmap S) get-width)) 2.)))

(define (sprite-y-max S)
  (fl+ (sprite-y S) (fl/ (->fl (send (sprite-bitmap S) get-height)) 2.)))

(define (sprites-collide? sprite1 sprite2)
  (let ([d1x (fl- (sprite-x-min sprite2) (sprite-x-max sprite1))]
        [d1y (fl- (sprite-y-min sprite2) (sprite-y-max sprite1))]
        [d2x (fl- (sprite-x-min sprite1) (sprite-x-max sprite2))]
        [d2y (fl- (sprite-y-min sprite1) (sprite-y-max sprite2))])
    (and (fl> 0. d1x) (fl> 0. d1y) (fl> 0. d2x) (fl> 0. d2y))))
