#lang racket/base

(require racket/flonum)

(provide (all-defined-out))

(struct camera (x1 x2) #:transparent)

(define (apply-camera cam x1 x2)
  (values
   (fl+ x1 (camera-x1 cam))
   (fl+ x2 (camera-x2 cam))))
