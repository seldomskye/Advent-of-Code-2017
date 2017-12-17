#lang racket

(define real-input "0: 3
1: 2
2: 4
4: 8
6: 5
8: 6
10: 6
12: 4
14: 6
16: 6
18: 17
20: 8
22: 8
24: 8
26: 9
28: 8
30: 12
32: 12
34: 10
36: 12
38: 12
40: 8
42: 12
44: 12
46: 10
48: 12
50: 12
52: 14
54: 14
56: 12
58: 14
60: 14
62: 14
64: 14
66: 14
68: 12
70: 14
72: 14
74: 14
76: 14
80: 18
82: 14
90: 18")

(define test-input "0: 3
1: 2
4: 4
6: 4")

(define (parse inp)
  (map (λ (l) (map string->number l)) (map (λ (s) (string-split s ": ")) (string-split inp "\n"))))


(define (pair->severity p)
  (pair->severity* p 0))

(define (get-mod depth)
  (* 2 (sub1 depth)))

(define (pair->severity* p offset)
  (if (zero? (modulo (+ offset (first p)) (get-mod (second p))))
      (* (first p) (second p))
      0))

(map pair->severity (parse test-input))

(define (get-mod-pairs pairs)
  (map (λ (p) (cons (modulo (first p) (get-mod (second p))) (get-mod (second p)))) pairs))

(define sort-f (λ (p1 p2) (or
                                                (< (cdr p1) (cdr p2))
                                                (and
                                                 (= (cdr p1) (cdr p2))
                                                 (< (car p1) (car p2))))))

(define (part2 inp)
  (let ([group-filters (group-by car (get-mod-pairs (parse inp)))])
    (map (λ (g) (sort g sort-f)) group-filters)
    #;(part2* group-filters 0)))



(define (get-truth falses truths)
  (or
   (findf (λ (truth) (= (cdr truth) (gcd (cdr truth) (cdar falses)))) truths)
   '(0 . 2)))

(define (coll falses truth)
  (coll* falses truth (car truth)))

(define (coll* falses truth acc)
  (cond
    [(< (cdar falses) acc) (error "no possible values found")]
    [(memf (λ (f) (= acc (car f))) falses) (coll* falses truth (+ acc (cdr truth)))]
    [else (cons acc (cdar falses))])) 
      

(coll '((0 . 4)) '(0 . 2))

(define (part2* gs n)
  (if (passes-filters gs n)
      n
      (part2* gs (add1 n))))

(define (passes-filters grouped-filters n)
  (cond
    [(empty? grouped-filters) #t]
    [(passes-group (first grouped-filters) n) (passes-filters (rest grouped-filters) n)]
    [else #f]))

(define (passes-group g n)
  (andmap (λ (f) (passes-filter f n)) g))

(define (passes-filter f n)
  (not (zero? (modulo (+ n (car f)) (cdr f)))))

(define (part2-opt inp)
  (foldl (λ (falses truths) (cons (coll falses (get-truth falses truths)) truths)) '()
              (sort (group-by cdr (get-mod-pairs (parse inp))) (λ (g1 g2) (< (cdar g1) (cdar g2))))))

(define mod-constraints '((0 . 34) (2 . 32) (16 . 26) (18 . 22) (4 . 18) (2 . 14) (2 . 10) (0 . 2)))

(define (find-solution times cong mod)
  (

(define (crt p1 p2)
  1)

(part2-opt real-input)
