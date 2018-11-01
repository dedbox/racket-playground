#lang racket/base

(require playground/client/camera
         playground/client/colors
         racket/class
         racket/draw
         racket/flonum)

(provide (all-defined-out))

(struct sprite (bitmap x1 x2) #:mutable #:transparent)

(define (make-sprite w h x1 x2)
  (define bitmap (make-bitmap w h))
  (define dc (send bitmap make-dc))
  (send dc set-pen white 1 'solid)
  (send dc set-brush blue 'solid)
  (send dc draw-rectangle 0. 0. (->fl w) (->fl h))
  (send dc draw-line 0. 0. (fl- (->fl w) 1.) (fl- (->fl h) 1.))
  (send dc draw-line 0. (fl- (->fl h) 1.) (fl- (->fl w) 1.) 0.)
  (send dc flush)
  (sprite bitmap x1 x2))

(define (draw-sprite spr cam dc)
  (define w (->fl (send (sprite-bitmap spr) get-width)))
  (define h (->fl (send (sprite-bitmap spr) get-height)))
  (define x1 (fl- (sprite-x1 spr) (fl/ w 2.)))
  (define x2 (fl- (sprite-x2 spr) (fl/ h 2.)))
  (define-values (x1* x2*) (apply-camera cam x1 x2))
  (send dc draw-bitmap (sprite-bitmap spr) x1* x2*))
