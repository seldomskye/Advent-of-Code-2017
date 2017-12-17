#lang typed/racket
(require racket/cmdline)
(require typed/racket)

(struct state-1 ([stt : (Listof Nonnegative-Integer)]
                 [cur-pos : Nonnegative-Integer]) #:transparent )

(struct state-2 ([stt : Nonnegative-Integer]
                 [cur-pos : Nonnegative-Integer]) #:transparent)

(define (build-list [reps : Nonnegative-Integer]) : (Listof Nonnegative-Integer)
  (if (zero? reps)
      '()
      (cons reps (build-list (sub1 reps)))))

(define (main [input : Nonnegative-Integer]
              [reps : Nonnegative-Integer]) : Nonnegative-Integer
  (get-after-2017
   (foldl
    (λ ([v : Nonnegative-Integer]
        [acc : state-1]) (main* acc input v))
    (state-1 '(0) 0)
    (build-list reps))))

(define (main* [stt : state-1]
               [skip-extra : Nonnegative-Integer]
               [cur-val : Nonnegative-Integer]) : state-1
  (let* ([stt-len (add1 cur-val)]
         [insert-index (modulo (+ skip-extra (state-1-cur-pos stt)) stt-len)])
    (let-values ([(head tail) (split-at (state-1-stt stt) (add1 insert-index))])
      (state-1 (append head (cons (add1 cur-val) tail)) (add1 insert-index)))))

(define (get-after-2017 [state : state-1]) : Nonnegative-Integer
  (list-ref (state-1-stt state) (add1 (state-1-cur-pos state))))

(define (main2-wrapper
         [input : Nonnegative-Integer]
         [reps : Nonnegative-Integer]) : Nonnegative-Integer
  (main2 input reps (state-2 0 0) 0))

(define (main2 [input : Nonnegative-Integer]
               [reps : Nonnegative-Integer]
               [state : state-2]
               [rep : Nonnegative-Integer]) : Nonnegative-Integer
  (if (zero? reps)
      (state-2-stt state)
      (main2 input (sub1 reps) (main2* state input rep) (add1 rep)))) 

(define (main2* [stt : state-2]
                [skip-extra : Nonnegative-Integer]
                [cur-val : Nonnegative-Integer]) : state-2
  (let* ([stt-len (add1 cur-val)]
         [insert-index (modulo (+ skip-extra (state-2-cur-pos stt)) stt-len)])
    (if (zero? insert-index)
        (state-2 (add1 cur-val) (add1 insert-index))
        (state-2 (state-2-stt stt) (add1 insert-index)))))

(define part-2 : (Parameterof Boolean) (make-parameter #f))

(define (string->int [str : String]) : Nonnegative-Integer
  (let ([n? (string->number str)])
    (cond
      [(exact-nonnegative-integer? n?) n?]
      [else 0])))

(command-line
 #:program "day17"
 #:once-each
 [("-p" "--part2") "Execute Part 2"
                   (part-2 #t)]
 #:args (input loops)
 (cond
   [(and (string? input) (string? loops))
    (if (part-2)
        (main2-wrapper (string->int input) (string->int loops))
        (main (string->int input) (string->int loops)))]
   [else (println "Bad inputs")]
   ))



 #;(define part2-output (main2 370 50000000))