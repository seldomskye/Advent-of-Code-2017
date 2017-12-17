#lang racket
(require racket/cmdline)
(struct state (stt cur-pos) #:transparent)

(define (main input reps)
  (get-after-2017 (foldl (λ (v acc) (main* acc input v)) (state '(0) 0) (build-list reps values))))

(define (main* stt skip-extra cur-val)
  (let* ([stt-len (add1 cur-val)]
         [insert-index (modulo (+ skip-extra (state-cur-pos stt)) stt-len)])
    (let-values ([(head tail) (split-at (state-stt stt) (add1 insert-index))])
      (state (append head (cons (add1 cur-val) tail)) (add1 insert-index)))))

(define (get-after-2017 state)
  (list-ref (state-stt state) (add1 (state-cur-pos state))))

(define (main2-wrapper input reps)
  (main2 input reps (state '(0) 0) 0))

(define (main2 input reps state rep)
  (if (zero? reps)
      (state-stt state)
      (main2 input (sub1 reps) (main2* state input rep) (add1 rep)))) 

(define (main2* stt skip-extra cur-val)
   (let* ([stt-len (add1 cur-val)]
         [insert-index (modulo (+ skip-extra (state-cur-pos stt)) stt-len)])
     (if (zero? insert-index)
         (state (add1 cur-val) (add1 insert-index))
         (state (state-stt stt) (add1 insert-index)))))

(define part-2 (make-parameter #f))

(command-line
 #:program "day17"
 #:once-each
 [("-p" "--part2") "Execute Part 2"
                    (part-2 #t)]
 #:args (input loops)
 (if (part-2)
     (main2-wrapper (string->number input) (string->number loops))
     (main (string->number input) (string->number loops)))
 )



#;(define part2-output (main2 370 50000000))