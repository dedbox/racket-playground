#lang racket/base

(require racket/flonum)

(provide (all-defined-out))

(struct camera (x y) #:transparent)

(define (apply-camera cam x y)
  (values
   (fl+ x (camera-x cam))
   (fl+ y (camera-y cam))))
