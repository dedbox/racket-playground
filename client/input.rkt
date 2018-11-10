#lang racket/base

(require racket/match)

(provide (all-defined-out))

(struct input (go-left? go-right? go-up? go-down?) #:mutable #:transparent)

(define input->v2
  (match-lambda
    [(input L R U D)
     (match* (L R U D)
       ;; axes
       [(#t #f #f #f) (values -1.  0.)]
       [(#f #t #f #f) (values  1.  0.)]
       [(#f #f #t #f) (values  0. -1.)]
       [(#f #f #f #t) (values  0.  1.)]
       ;; diagonals
       [(#t #f #t #f) (values -1.  -1.)]
       [(#t #f #f #t) (values -1.   1.)]
       [(#f #t #t #f) (values  1.  -1.)]
       [(#f #t #f #t) (values  1.   1.)]
       ;; everything else
       [( _  _  _  _) (values  0. 0.)])]))
