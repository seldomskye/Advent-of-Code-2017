#lang racket

(define (main args)
  (print args))
(provide (all-defined-out))

(define-struct c (n ne se s sw nw) #:transparent)

(define input (string-trim (file->string "/home/skye/day11.txt")))

(define (parse-inp inp)
  (string-split inp ","))

(define (get-counts l)
  (make-c (count (λ (x) (equal? x "n")) l)
  (count (λ (x) (equal? x "ne"))l)
  (count (λ (x) (equal? x "se"))l)
  (count (λ (x) (equal? x "s"))l)
  (count (λ (x) (equal? x "sw"))l)
  (count (λ (x) (equal? x "nw"))l)))

(define (apply-dir counts dir)
  (cond
    [(equal? dir "n") (struct-copy c counts [n (add1 (c-n counts))])]
    [(equal? dir "ne") (struct-copy c counts [ne (add1 (c-ne counts))])]
    [(equal? dir "se") (struct-copy c counts [se (add1 (c-se counts))])]
    [(equal? dir "s") (struct-copy c counts [s (add1 (c-s counts))])]
    [(equal? dir "sw") (struct-copy c counts [sw (add1 (c-sw counts))])]
    [(equal? dir "nw") (struct-copy c counts [nw (add1 (c-nw counts))])]))

(define (cancel-equivalent counts)
  (make-c
   (max 0 (- (c-n counts) (c-s counts))) ; n
   (max 0 (- (c-ne counts) (c-sw counts))) ; ne
   (max 0 (- (c-se counts) (c-nw counts))) ; se
   (max 0 (- (c-s counts) (c-n counts))) ; s
   (max 0 (- (c-sw counts) (c-ne counts))) ; sw
   (max 0 (- (c-nw counts) (c-se counts))))) ; nw

(define (cancel-diag counts)
  
  (add-counts counts (make-c
   (- (min (c-n counts) (max (c-se counts) (c-sw counts)))) ;n
   (- (min (c-n counts) (c-se counts)) (min (c-s counts) (c-ne counts))) ;ne
   (- (min (c-s counts) (c-ne counts)) (min (c-n counts) (c-se counts))) ;se
   (- (min (c-s counts) (max (c-ne counts) (c-nw counts)))) ;s
   (- (min (c-s counts) (c-nw counts)) (min (c-n counts) (c-sw counts))) ;sw
   (- (min (c-n counts) (c-sw counts)) (min (c-s counts) (c-nw counts))) ;nw
   )))

(define (add-counts c1 c2)
  (apply make-c
   (map (λ (acc) (+ (acc c1) (acc c2))) (list c-n c-ne c-se c-s c-sw c-nw))))

(define (get-dist counts)
  (foldr (λ (acc z) (+ (acc counts) z)) 0 (list c-n c-ne c-se c-s c-sw c-nw)))

;; Replace with a macro that creates the update lists?
;; 
(define (cancel-ew counts)
  (make-c
   (+ (c-n counts) (min (c-nw counts) (c-ne counts)))
   (max 0 (- (c-ne counts) (c-nw counts)))
   (max 0 (- (c-se counts) (c-sw counts)))
   (+ (c-s counts) (min (c-se counts) (c-sw counts)))
   (max 0 (- (c-sw counts) (c-se counts)))
   (max 0 (- (c-nw counts) (c-ne counts)))
   ))

(define (cancel counts)
  (cancel-diag (cancel-equivalent (cancel-ew (cancel-equivalent counts)))))
     
(define (part1 inp)
  (cancel (cancel (get-counts (parse-inp inp)))))

(define (part2 inp)
  (let ([max-dist (box 0)])
  (foldl (λ (dir c) (let ([counts* (cancel (apply-dir c dir))])
                      (set-box! max-dist (max (unbox max-dist) (get-dist counts*)))
                      counts*)) (make-c 0 0 0 0 0 0) (parse-inp inp))
    max-dist)
  )

#;(begin 
(part1 "ne,ne,ne")
(part1 "ne,ne,sw,sw")
(part1 "ne,ne,s,s")
(part1 "se,sw,se,sw,sw"))


(define-struct axial (a b) #:transparent)

(define (add-axial a1 a2)
  (make-axial (+ (axial-a a1) (axial-a a2))
              (+ (axial-b a1) (axial-b a2))))

(define (dir->axial-offset dir)
  (cond
    [(equal? dir "n") (make-axial 1 -1)]
    [(equal? dir "ne") (make-axial 1 0)]
    [(equal? dir "se") (make-axial 0 1)]
    [(equal? dir "s") (make-axial -1 1)]
    [(equal? dir "sw") (make-axial -1 0)]
    [(equal? dir "nw") (make-axial 0 -1)]))

(define (axial->cube axi)
  (list (axial-a axi) (axial-b axi) (- (- (axial-a axi)) (axial-b axi))))

(define (cube-distance c1 c2)
  (max
   (abs (- (first c1) (first c2)))
   (abs (- (second c1) (second c2)))
   (abs (- (third c1) (third c2)))))

(define (get-axial-dist-origin axi)
  (cube-distance (axial->cube axi) '(0 0 0)))

;; Optimized by switching to use a hex coordinate system
;; This makes all of the calculations much simpler to do, rather than having to define everything
;; I mean, really it's just a cleaner version of what you had
;; Eg, the addition and subtraction now make sense, rather than you doing a dig cancelation step
;; Where you basically fold things up, though that was interesting
(define (day11-optimal inp)
  (cube-distance (axial->cube (foldl (λ (dir b) (add-axial b (dir->axial-offset dir))) (make-axial 0 0) (parse-inp inp))) '(0 0 0)))

(define (day11-2-optimal inp)
   (let ([max-dist (box 0)])
     (foldl (λ (dir c) (let ([next-axi (add-axial c (dir->axial-offset dir))])
                      (set-box! max-dist (max (unbox max-dist) (get-axial-dist-origin next-axi)))
                      next-axi)) (make-axial 0 0) (parse-inp inp))
    max-dist))

(print (current-command-line-arguments))
