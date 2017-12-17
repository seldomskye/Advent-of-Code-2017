#lang racket
(require racket/set)

(apply vector '(1 2 3))
(define sample '(0 2 7 0))
(define data-set '(10	3	15	10	5	15	5	15	9	2	5	8	5	2	3	6))
(define part-2 '(1 1 0 15 14 13 12 10 10 9 8 7 6 4 3 5))
(define (max-index vec)
  (let ([max-value (vector-argmax (λ (x) x) vec)])
    (cons (vector-member max-value vec) max-value)))

(define (part1-main l)
  (define (part1-main* v seen-states acc)
    (let ([v* (part1 v)])
      (if (set-member? seen-states  v*)
          (begin
            (print v*)
            acc)
          (part1-main* v* (set-add seen-states v*) (add1 acc)))))
  (let* ([vec (apply vector l)]
         [seen-states (set vec)])
    (part1-main* vec seen-states 1)
  ))

(define (part1 vec)
  (let* ([max-pair (max-index vec)]
         [max-index (car max-pair)]
         [max-value (cdr max-pair)])
    (vector-set! vec max-index 0)
    (part1* vec (get-next-index max-index vec) max-value)))

(define (get-next-index ind vec)
  (modulo (add1 ind) (vector-length vec)))

(define (part1* vec curr-index blocks-held)
  (define (update-vec)
    (vector-set! vec curr-index (add1 (vector-ref vec curr-index)))
    (part1* vec (get-next-index curr-index vec) (sub1 blocks-held)))
  (if (= 0 blocks-held)
      vec
      (update-vec)))

(part1 (apply vector sample))
(part1-main sample)
(part1-main data-set)
(part1-main part-2)