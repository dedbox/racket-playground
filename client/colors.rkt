#lang racket/base

(require racket/class
         racket/draw)

(provide (all-defined-out))

(define (hex-color rgb-hex)
  (define rgb
    (cdr (reverse (bytes->list (integer->integer-bytes rgb-hex 4 #f)))))
  (apply make-color rgb))

(define white (hex-color #xffffff))
(define black (hex-color #x000000))
(define red   (hex-color #xff0000))
(define green (hex-color #x00ff00))
(define blue  (hex-color #x0000ff))
