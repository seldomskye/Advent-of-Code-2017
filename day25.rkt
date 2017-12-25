#lang racket
(struct state (stt curr left right) #:transparent)

(define init-state (state 'A 0 '() '()))

(define (move-left st)
  (struct-copy state st
               [curr (first-state (state-right st))]
               [left (cons (state-curr st) (state-left st))]
               [right (rest-state (state-right st))]))

(define (move-right st)
  (struct-copy state st
               [curr (first-state (state-left st))]
               [left (rest-state (state-left st))]
               [right (cons (state-curr st) (state-right st))]))

(define (one? n)
  (zero? (sub1 n)))

(define (write-value v next-state st)
  (struct-copy state st [curr v]
               [stt next-state]))

(define (main)
  (let ([final-state (ex-input init-state 12261543)])
    (length (filter one? (append (cons (state-curr final-state) (state-left final-state)) (state-right final-state))))))

(define (ex-input stt steps)
  (if (zero? steps) stt
      (let* ([d (list (state-stt stt) (state-curr stt))]
             [stt* 
              (cond 
                [(equal? d (list 'A 0)) (move-right (write-value 1 'B stt))]
                [(equal? d (list 'A 1)) (move-left (write-value 0 'C stt))]
                [(equal? d (list 'B 0)) (move-left (write-value 1 'A stt))]
                [(equal? d (list 'B 1)) (move-right (write-value 1 'C stt))]
                [(equal? d (list 'C 0)) (move-right (write-value 1 'A stt))]
                [(equal? d (list 'C 1)) (move-left (write-value 0 'D stt))]
                [(equal? d (list 'D 0)) (move-left (write-value 1 'E stt))]
                [(equal? d (list 'D 1)) (move-left (write-value 1 'C stt))]
                [(equal? d (list 'E 0)) (move-right (write-value 1 'F stt))]
                [(equal? d (list 'E 1)) (move-right (write-value 1 'A stt))]
                [(equal? d (list 'F 0)) (move-right (write-value 1 'A stt))]
                [(equal? d (list 'F 1)) (move-right (write-value 1 'E stt))]
                [else (println "problematic")])])
        (ex-input stt* (sub1 steps)))
      ))

(define (first-state l)
  (if (empty? l)
      0
      (first l)))

(define (rest-state l)
  (if (empty? l)
      '()
      (rest l)))

(ex-input init-state 1)
