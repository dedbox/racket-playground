#lang racket/base

(require racket/flonum
         racket/list
         racket/match
         racket/math)

(provide (all-defined-out))

(struct grid (cells x-min y-min x-max y-max dx dy num-cols) #:transparent)

(define (make-grid x-min y-min x-max y-max dx dy)
  (define num-rows (inexact->exact (ceiling (fl/ (fl- y-max y-min) dy))))
  (define num-cols (inexact->exact (ceiling (fl/ (fl- x-max x-min) dx))))
  (define cells (make-vector (* num-rows num-cols) null))
  (grid cells x-min y-min x-max y-max dx dy num-cols))

(define (grid-index G x y)
  (match G
    [(grid _ x-min y-min x-max y-max dx dy _)
     (and
      (not (or (fl< x x-min) (fl< y y-min) (fl> x x-max) (fl> y x-max)))
      (let ([row (exact-floor (fl/ (fl- y y-min) dy))]
            [col (exact-floor (fl/ (fl- x x-min) dx))])
        (+ col (* row (grid-num-cols G)))))]))

(define (grid-ref G x y)
  (define k (grid-index G x y))
  (and k (vector-ref (grid-cells G) k)))

(define (grid-add! G x y v)
  (define k (grid-index G x y))
  (and k (vector-set! (grid-cells G) k (cons v (vector-ref (grid-cells G) k)))))

(define (build-grid x-min y-min x-max y-max dx dy f)
  (define G (make-grid x-min y-min x-max y-max dx dy))
  (for ([y (in-range y-min y-max dy)]
        [j (in-naturals)])
    (for ([x (in-range x-min x-max dx)]
          [i (in-naturals)])
      (grid-add! G x y (f i j))))
  G)

(define (grid-area-ref G x-min y-min x-max y-max)
  (flatten
   (for*/list ([y (in-range y-min (fl+ y-max 1.) (grid-dy G))]
               [x (in-range x-min (fl+ x-max 1.) (grid-dx G))])
     (grid-ref G x y))))

(define (grid-area-add! G x-min y-min x-max y-max v)
  (for* ([y (in-range y-min (fl+ y-max 1.) (grid-dy G))]
         [x (in-range x-min (fl+ x-max 1.) (grid-dx G))])
    (grid-add! G x y v)))

(module+ test
  (require rackunit
           racket/match)

  (test-case "make-grid"
    (match (make-grid -300. -300. 300. 300. 100. 100.)
      [(grid cells x-min y-min _ _ dx dy num-cols)
       (check-equal? cells (make-vector 36 null))
       (check = x-min -300.)
       (check = y-min -300.)
       (check = dx 100.)
       (check = dy 100.)
       (check = num-cols 6)]))

  (test-case "grid-index"
    (define G (make-grid -300. -300. 300. 300. 100. 100.))
    (for ([y (in-range -300. 300. 100.)]
          [j (in-naturals)])
      (for ([x (in-range -300. 300. 100.)]
            [i (in-naturals)])
        (check = (grid-index G x y) (+ i (* j 6))
               (format "x=~a y=~a i=~a j=~a" x y i j)))))

  (test-case "grid-add! 6x6"
    (define G (make-grid -300. -300. 300. 300. 100. 100.))
    (for ([y (in-range -300. 300. 100.)]
          [j (in-naturals)])
      (for ([x (in-range -300. 300. 100.)]
            [i (in-naturals)])
        (grid-add! G x y (+ i (* j 6)))))
    (check-equal? (grid-cells G) (build-vector 36 (compose list values))))

  (test-case "grid-add! 12x6"
    (define G (make-grid -300. -300. 300. 300. 100. 100.))
    (for ([y (in-range -300. 300. 100.)]
          [j (in-naturals)])
      (for ([x (in-range -300. 300. 50.)]
            [i (in-naturals)])
        (grid-add! G x y (+ i (* j 12)))))
    (for ([k (in-range 36)])
      (check-equal? (vector-ref (grid-cells G) k)
                    (list (+ 1 (* 2 k)) (* 2 k)))))

  (test-case "grid-add! 6x12"
    (define G (make-grid -300. -300. 300. 300. 100. 100.))
    (for ([x (in-range -300. 300. 100.)]
          [i (in-naturals)])
      (for ([y (in-range -300. 300. 50.)]
            [j (in-naturals)])
        (grid-add! G x y (+ j (* i 12)))))
    (for* ([i (in-range 6)]
           [j (in-range 6)])
      (check-equal? (vector-ref (grid-cells G) (+ i (* j 6)))
                    (list (+ 1 (* 12 i) (* 2 j))
                          (+ (* 12 i) (* 2 j))))))

  (test-case "grid-ref"
    (define G (build-grid -300. -300. 300. 300. 100. 100. (λ (i j) (+ i (* j 6)))))
    (for ([y (in-range -300. 300. 100.)]
          [j (in-naturals)])
      (for ([ε (in-range 0. 100. 10.)])
        (for ([x (in-range -300. 300. 100.)]
              [i (in-naturals)])
          (for ([δ (in-range 0. 100. 10.)])
            (check-equal? (grid-ref G (fl+ x δ) (fl+ y ε))
                          (list (+ i (* j 6))))))))))
