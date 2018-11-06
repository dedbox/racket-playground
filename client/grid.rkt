#lang racket/base

(require playground/client/camera
         playground/client/colors
         racket/class
         racket/flonum
         racket/function
         racket/list
         racket/math)

(provide (all-defined-out))

(struct grid (cells x-min y-min x-max y-max dx dy num-cols) #:transparent)

(define (make-grid x-min y-min x-max y-max dx dy)
  (define num-rows (inexact->exact (ceiling (fl/ (fl- y-max y-min) dy))))
  (define num-cols (inexact->exact (ceiling (fl/ (fl- x-max x-min) dx))))
  (define cells (make-vector (* num-rows num-cols) null))
  (grid cells x-min y-min x-max y-max dx dy num-cols))

(define (grid-row-index G y)
  (define y-min (grid-y-min G))
  (define y-max (grid-x-max G))
  (define dy (grid-dy G))
  (exact-floor (fl/ (fl- y y-min) dy)))

(define (grid-col-index G x)
  (define x-min (grid-x-min G))
  (define x-max (grid-x-max G))
  (define dx (grid-dx G))
  (exact-floor (fl/ (fl- x x-min) dx)))

(define (grid-index G x y)
  (define col (grid-col-index G x))
  (define row (grid-row-index G y))
  (define num-cols (grid-num-cols G))
  (+ col (* row num-cols)))

(define (grid-cell-x-min G x)
  (define x-min (grid-x-min G))
  (define col (->fl (grid-col-index G x)))
  (define dx (grid-dx G))
  (fl+ x-min (fl* col dx)))

(define (grid-cell-y-min G y)
  (define y-min (grid-y-min G))
  (define row (->fl (grid-row-index G y)))
  (define dy (grid-dy G))
  (fl+ y-min (fl* row dy)))

(define (grid-indices G x-min y-min x-max y-max)
  (define xs (let loop ([x (grid-cell-x-min G x-min)] [xs null])
               (if (fl> x x-max) xs (loop (fl+ x (grid-dx G)) (cons x xs)))))
  (define ys (let loop ([y (grid-cell-y-min G y-min)] [ys null])
               (if (fl> y y-max) ys (loop (fl+ y (grid-dy G)) (cons y ys)))))
  (for*/list ([y (in-list ys)]
              [x (in-list xs)])
    (grid-index G x y)))

(define (grid-ref G x y)
  (define x-min (grid-x-min G))
  (define x-max (grid-x-max G))
  (define y-min (grid-y-min G))
  (define y-max (grid-y-max G))
  (and (fl>= x x-min) (fl<= x x-max)
       (fl>= y y-min) (fl<= y y-max)
       (vector-ref (grid-cells G) (grid-index G x y))))

(define (grid-add! G x y v)
  (define x-min (grid-x-min G))
  (define x-max (grid-x-max G))
  (define y-min (grid-y-min G))
  (define y-max (grid-y-max G))
  (define cells (grid-cells G))
  (and (fl>= x x-min) (fl<= x x-max)
       (fl>= y y-min) (fl<= y y-max)
       (let ([k (grid-index G x y)])
         (vector-set! cells k (cons v (vector-ref cells k))))))

(define (build-grid x-min y-min x-max y-max dx dy f)
  (define G (make-grid x-min y-min x-max y-max dx dy))
  (for ([y (in-range y-min y-max dy)]
        [j (in-naturals)])
    (for ([x (in-range x-min x-max dx)]
          [i (in-naturals)])
      (grid-add! G x y (f i j))))
  G)

(define (grid-area-ref G x-min y-min x-max y-max)
  (define G-x-min (grid-x-min G))
  (define G-x-max (grid-x-max G))
  (define G-y-min (grid-y-min G))
  (define G-y-max (grid-y-max G))
  (and (fl>= x-min G-x-min) (fl<= x-max G-x-max)
       (fl>= y-min G-y-min) (fl<= y-max G-y-max)
       (flatten (map (curry vector-ref (grid-cells G))
                     (grid-indices G x-min y-min x-max y-max)))))

(define (grid-area-add! G x-min y-min x-max y-max v)
  (define G-x-min (grid-x-min G))
  (define G-x-max (grid-x-max G))
  (define G-y-min (grid-y-min G))
  (define G-y-max (grid-y-max G))
  (and (fl>= x-min G-x-min) (fl<= x-max G-x-max)
       (fl>= y-min G-y-min) (fl<= y-max G-y-max)
       (for* ([y (in-range y-min (fl+ y-max 1.) (grid-dy G))]
              [x (in-range x-min (fl+ x-max 1.) (grid-dx G))])
         (grid-add! G x y v))))

(define (draw-grid G cam dc)
  (define x-min (grid-x-min G))
  (define x-max (grid-x-max G))
  (define y-min (grid-y-min G))
  (define y-max (grid-y-max G))
  (define dx (grid-dx G))
  (define dy (grid-dy G))
  (send dc set-pen red 1 'solid)
  (send dc set-brush red 'transparent)
  (for* ([y (in-range y-min y-max dy)]
         [x (in-range x-min x-max dx)])
    (define width (flmin dx (fl- x-max x)))
    (define height (flmin dy (fl- y-max y)))
    (define-values (x* y*) (apply-camera cam x y))
    (send dc draw-rectangle x* y* width height)))

(define (draw-grid-ref-index G cam dc k)
  (define x-min (grid-x-min G))
  (define x-max (grid-x-max G))
  (define y-min (grid-y-min G))
  (define y-max (grid-y-max G))
  (define dx (grid-dx G))
  (define dy (grid-dy G))
  (define num-cols (grid-num-cols G))
  (define-values (row col) (quotient/remainder k num-cols))
  (define x (fl+ x-min (fl* (->fl col) dx)))
  (define y (fl+ y-min (fl* (->fl row) dy)))
  (define-values (x* y*) (apply-camera cam x y))
  (define width (flmin dx (fl- x-max x)))
  (define height (flmin dy (fl- y-max y)))
  (send dc set-pen green 2 'solid)
  (send dc set-brush green 'transparent)
  (send dc draw-rectangle x* y* width height))

(define (draw-grid-ref G cam dc x y)
  (define x-min (grid-x-min G))
  (define x-max (grid-x-max G))
  (define y-min (grid-y-min G))
  (define y-max (grid-y-max G))
  (and (fl>= x x-min) (fl<= x x-max)
       (fl>= y y-min) (fl<= y y-max)
       (draw-grid-ref-index G cam dc (grid-index G x y))))

(define (draw-grid-area-ref G cam dc x-min y-min x-max y-max)
  (define G-x-min (grid-x-min G))
  (define G-x-max (grid-x-max G))
  (define G-y-min (grid-y-min G))
  (define G-y-max (grid-y-max G))
  (and (fl>= x-min G-x-min) (fl<= x-max G-x-max)
       (fl>= y-min G-y-min) (fl<= y-max G-y-max)
       (for ([k (grid-indices G x-min y-min x-max y-max)])
         (draw-grid-ref-index G cam dc k))))

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
