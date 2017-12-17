#lang racket
(require math)

(define (lowest-square num acc)
  (if (<= num (* acc acc))
      acc
      (lowest-square num (add1 (add1 acc)))))

(define (get-circle cell-number)
  (add1 (floor (/ (lowest-square cell-number 1) 2))))

(define (day3 num)
  (if (= 1 num) 1 (day3* num)))

(define (day3* num)
  (let* ([circle (get-circle num)]
         [inner-square-side (- (* 2 circle) 3)]
         [inner-square-cells (* inner-square-side inner-square-side)]
         [moves-along-outside (- num inner-square-cells)]
         [mod-val (modulo moves-along-outside (* (sub1 circle ) 2))])
    (cons num (+ (sub1 circle) (modded->steps mod-val circle)))))

(define (modded->steps mod-val circle-size)
  (abs (- (add1 mod-val) circle-size)))



#;(begin 
(day3 1024)
(day3 12)
(day3 23)
(day3 325489)
(map day3 (build-list 9 (λ (x) (+ 2 x))))
(day3 1))


(define (expand-square sq)
   (let* ([old-size (vector-length sq)]
         [new-square (make-vector (+ 2 old-size) 0)])
     (define (expand-row row)
       (let ([new-row (make-vector (+ 2 old-size) 0)])
         (vector-copy! new-row 1 row)
         new-row))
     (vector-set! new-square 0 (make-vector (+ 2 old-size ) 0))
     (vector-copy! new-square 1 (vector-map expand-row sq))
     (vector-set! new-square (sub1 (+ 2 old-size)) (make-vector (+ 2 old-size ) 0))
     new-square))

(define (day3-2 steps)
  (let ([init-square '#(#(1))])
    (day3-2* init-square 0 0 'right steps)))
(define (day3-2* sq x y dir steps)
  (if (not (zero? steps))
  (if (and
       (= x (sub1 (vector-length sq)))
       (= y 0))
      (if
       (has-larger-than-val 325489 sq)
       sq
       (day3-2* (expand-square sq) (add1 x) (add1 y) dir steps))
      (let ([next-loc (move-and-set sq x y dir 325489)])
        (day3-2* sq (first next-loc) (second next-loc) (third next-loc) (sub1 steps))))
  sq))

(define (has-larger-than-val val sq)
  (< 0 (vector-length (vector-filter (λ (x) (< val x)) (vector-ref sq 0)))))

(define (move-and-set sq x y dir val)
  (let* ([next-loc (get-next-square x y dir sq)]
         [x* (first next-loc)]
         [y* (second next-loc)]
         [dir* (third next-loc)])
    (vector-set! (vector-ref sq y*)  x* (get-value x* y* sq))
    next-loc))

(define (go-left x y)
  (list (sub1 x) y 'left))

(define (go-right x y)
  (list (add1 x) y 'right))

(define (go-up x y)
  (list x (add1 y) 'up))

(define (go-down x y)
  (list x (sub1 y) 'down))

(define (get-next-square x y dir sq)
  (cond
    [(eq? dir 'up) (if (< (add1 y) (vector-length sq))
                     (go-up x y)
                     (go-left x y))]
    [(eq? dir 'down) (if (<= 0 (sub1 y))
                     (go-down x y)
                     (go-right x y))]
    [(eq? dir 'left) (if (<= 0 (sub1 x))
                     (go-left x y)
                     (go-down x y))]
    [(eq? dir 'right) (if (< (add1 x) (vector-length sq) )
                     (go-right x y)
                     (go-up x y))]))

(define (get-value x y sq)
  (foldr + 0 (map (λ (p) (vector-ref (vector-ref sq (cdr p) ) (car p))) (get-adjacent x y (vector-length sq)))))

(define (get-adjacent x y max-dims)
  (filter (λ (p) (not (eq? p (cons x y)))) (filter (λ (p) (out-of-bounds p max-dims))
          (apply append (map
   (λ (f) (map f (build-list 3 (λ (y*) (sub1(+ y y*))))))
   (map (λ (e) (λ (z) (cons e z))) (build-list 3 (λ (x*) (sub1 (+ x x*)))))
  )))))

(define (out-of-bounds pair max-dims)
  (and
   (<= 0 (car pair) (sub1 max-dims))
   (<= 0 (cdr pair) (sub1 max-dims))))
  

(define (part2-space-efficient loops)
  (if (= 1 loops)
      '#(1)
      (next-loop (part2-space-efficient (sub1 loops)))))

(define (next-loop loop)
  (let* ([loop-num (+ 2 (sqrt (vector-length loop)))]
        [loop* (build-vector (* loop-num loop-num) (const 0))])
    (println loop-num)
    loop*))
