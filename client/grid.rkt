#lang racket/base

(provide (all-defined-out))

(struct grid (cells x-min y-min dx dy num-cols) #:transparent)

(define (make-grid x-min y-min x-max y-max dx dy)
  (define num-rows (ceiling (/ (- y-max y-min) dy)))
  (define num-cols (ceiling (/ (- x-max x-min) dx)))
  (define cells (make-vector (* num-rows num-cols) null))
  (grid cells x-min y-min dx dy num-cols))

(define (grid-index G x y)
  (define row (floor (/ (- y (grid-y-min G)) (grid-dy G))))
  (define col (floor (/ (- x (grid-x-min G)) (grid-dx G))))
  (+ col (* row (grid-num-cols G))))

(define (grid-ref G x y)
  (vector-ref (grid-cells G) (grid-index G x y)))

(define (grid-add! G x y v)
  (define index (grid-index G x y))
  (vector-set! (grid-cells G) index (cons v (vector-ref (grid-cells G) index))))

(define (build-grid x-min y-min x-max y-max dx dy f)
  (define G (make-grid -300 -300 300 300 100 100))
  (for ([y (in-range -300 300 100)]
        [j (in-naturals)])
    (for ([x (in-range -300 300 100)]
          [i (in-naturals)])
      (grid-add! G x y (f i j))))
  G)

(module+ test
  (require rackunit
           racket/match)

  (test-case "make-grid"
    (match (make-grid -300 -300 300 300 100 100)
      [(grid cells x-min y-min dx dy num-cols)
       (check-equal? cells (make-vector 36 null))
       (check = x-min -300)
       (check = y-min -300)
       (check = dx 100)
       (check = dy 100)
       (check = num-cols 6)]))

  (test-case "grid-index"
    (define G (make-grid -300 -300 300 300 100 100))
    (for ([y (in-range -300 300 100)]
          [j (in-naturals)])
      (for ([x (in-range -300 300 100)]
            [i (in-naturals)])
        (check = (grid-index G x y) (+ i (* j 6))
               (format "x=~a y=~a i=~a j=~a" x y i j)))))

  (test-case "grid-add! 6x6"
    (define G (make-grid -300 -300 300 300 100 100))
    (for ([y (in-range -300 300 100)]
          [j (in-naturals)])
      (for ([x (in-range -300 300 100)]
            [i (in-naturals)])
        (grid-add! G x y (+ i (* j 6)))))
    (check-equal? (grid-cells G) (build-vector 36 (compose list values))))

  (test-case "grid-add! 12x6"
    (define G (make-grid -300 -300 300 300 100 100))
    (for ([y (in-range -300 300 100)]
          [j (in-naturals)])
      (for ([x (in-range -300 300 50)]
            [i (in-naturals)])
        (grid-add! G x y (+ i (* j 12)))))
    (for ([k (in-range 36)])
      (check-equal? (vector-ref (grid-cells G) k)
                    (list (+ 1 (* 2 k)) (* 2 k)))))

  (test-case "grid-add! 6x12"
    (define G (make-grid -300 -300 300 300 100 100))
    (for ([x (in-range -300 300 100)]
          [i (in-naturals)])
      (for ([y (in-range -300 300 50)]
            [j (in-naturals)])
        (grid-add! G x y (+ j (* i 12)))))
    (for* ([i (in-range 6)]
           [j (in-range 6)])
      (check-equal? (vector-ref (grid-cells G) (+ i (* j 6)))
                    (list (+ 1 (* 12 i) (* 2 j))
                          (+ (* 12 i) (* 2 j))))))

  (test-case "grid-ref"
    (define G (build-grid -300 -300 300 300 100 100 (λ (i j) (+ i (* j 6)))))
    (for ([y (in-range -300 300 100)]
          [j (in-naturals)])
      (for ([ε (in-range 0 100 10)])
        (for ([x (in-range -300 300 100)]
              [i (in-naturals)])
          (for ([δ (in-range 0 100 10)])
            (check-equal? (grid-ref G (+ x δ) (+ y ε)) (list (+ i (* j 6))))))))))
