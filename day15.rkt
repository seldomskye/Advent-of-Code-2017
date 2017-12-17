#lang racket

(define factor-a 16807)
(define factor-b 48271)
(define mod-val 2147483647)

(define (day15 start-a start-b loops)
  (day15* start-a start-b 0 loops))

(define (day15* start-a start-b acc loop)
  (let ([next-a (next-gen factor-a start-a)]
        [next-b (next-gen factor-b start-b)])
    (if (zero? loop)
        acc
        (day15* next-a next-b (judge next-a next-b acc) (sub1 loop)))))

(define (judge a b acc)
  (if (zero? (bitwise-bit-field (bitwise-xor a b) 0 16))
      (add1 acc)
      acc))

(define (next-gen factor val)
  (modulo (* factor val) mod-val))

(define (day15-2 start-a start-b loops)
  (day15-2* start-a start-b 0 loops))

(define (day15-2* start-a start-b acc loops)
  (let ([next-a (next-gen-2 factor-a start-a 4)]
        [next-b (next-gen-2 factor-b start-b 8)])
    (if (zero? loops)
        acc
    (day15-2* next-a next-b (judge next-a next-b acc) (sub1 loops)))))

(define (next-gen-2 factor start divisible)
  (let ([val-maybe (next-gen factor start)])
    (if (zero? (bitwise-bit-field (bitwise-xor val-maybe divisible) 0 (integer-length divisible)))
        val-maybe
        (next-gen-2 factor val-maybe divisible))))
  