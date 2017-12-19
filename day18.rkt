#lang racket
(define ex-input-1 "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2")

(define ex-input-2 "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d")

(define given-input "set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 316
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19")

(define (main input)
  (let ([registers (make-hash)]
        [last-sound (box #f)]
        [last-recv (box #f)])
    (exec-instrs (apply vector (string->instr input)) 0 registers last-sound last-recv)
    (unbox last-recv)))

(define (string->instr input)
  (map (λ (l) (string-split l " ")) (string-split input "\n")))

(define (get-value registers num-of-reg)
  (or
   (string->number num-of-reg)
   (hash-ref registers num-of-reg 0)))

(define (mutate-reg f registers curr-instr)
  (hash-set! registers (second curr-instr) (f (hash-ref registers (second curr-instr) 0)
                                              (get-value registers (third curr-instr)))))

(define (exec-instrs instrs curr-instr-index registers last-sound last-recv)
  (if (and (<= 0 curr-instr-index)
           (< curr-instr-index (vector-length instrs)))
      (let* ([curr-instr (vector-ref instrs curr-instr-index)]
             [instr-type (first curr-instr)]
             [next-index 
              (cond
                [(equal? instr-type "snd") (set-box! last-sound (get-value registers (second curr-instr)))
                                           (add1 curr-instr-index)]
                [(equal? instr-type "set") (hash-set! registers (second curr-instr) (get-value registers (third curr-instr)))
                                           (add1 curr-instr-index)]
                [(equal? instr-type "add") (mutate-reg + registers curr-instr)
                                           (add1 curr-instr-index)]
                [(equal? instr-type "mul") (mutate-reg * registers curr-instr)
                                           (add1 curr-instr-index)]
                [(equal? instr-type "mod") (mutate-reg modulo registers curr-instr)
                                           (add1 curr-instr-index)]
                [(equal? instr-type "rcv") (when (not (zero? (unbox last-sound)))
                                             (set-box! last-recv (unbox last-sound)))
                                           (+ 9999999999999 curr-instr-index)]
                [(equal? instr-type "jgz") (if (< 0 (get-value registers (second curr-instr)))
                                               (+ curr-instr-index (get-value registers (third curr-instr)))
                                               (add1 curr-instr-index))])])
        (exec-instrs instrs next-index registers last-sound last-recv))
      curr-instr-index))

(struct state (curr-index registers recieve send) #:transparent)

(define (incr-curr-index stt)
  (cons #t (struct-copy state stt [curr-index (add1 (state-curr-index stt))])))

(define (main2 input)
  (main2* (apply vector (string->instr input))
          (cons #t (state 0 (make-hash) '() '()))
          (cons #f (state 0 (make-hash (list (cons "p" 1))) '() '()))
          0))

(define (move-send-to-recieve s1 s2)
  (cons
   (cons #f (struct-copy state s1 [send '()]))
   (cons #t (struct-copy state s2 [recieve (reverse (state-send s1))]))))

(define (main2* instrs s1 s2 ones)
  (cond [(car s1) (main2* instrs (exec-instrs2 instrs (cdr s1)) s2 ones)]
        [(car s2) (main2* instrs s1 (exec-instrs2 instrs (cdr s2)) ones)]
        [(not (empty? (state-send (cdr s1))))
         (let ([send/recv (move-send-to-recieve (cdr s1) (cdr s2))])
           (main2* instrs (car send/recv) (cdr send/recv) ones))]
        [(not (empty? (state-send (cdr s2))))
         (let ([send/recv (move-send-to-recieve (cdr s2) (cdr s1))])
           (main2* instrs (cdr send/recv) (car send/recv) (+ ones (length (state-send (cdr s2))))))]
        [else ones]))

(define (exec-instrs2 instrs stt)
  (let* ([curr-instr-index (state-curr-index stt)]
         [curr-instr (vector-ref instrs curr-instr-index)]
         [instr-type (first curr-instr)]
         [registers (state-registers stt)])
    (cond          
      [(equal? instr-type "set")
       (hash-set! registers (second curr-instr) (get-value registers (third curr-instr)))
       (incr-curr-index stt)]
      [(equal? instr-type "add")
       (mutate-reg + registers curr-instr)
       (incr-curr-index stt)]
      [(equal? instr-type "mul")
       (mutate-reg * registers curr-instr)
       (incr-curr-index stt)]
      [(equal? instr-type "mod")
       (mutate-reg modulo registers curr-instr)
       (incr-curr-index stt)]
      [(equal? instr-type "rcv")
       (if (not (empty? (state-recieve stt)))
           (begin
             (hash-set! registers (second curr-instr) (first (state-recieve stt)))
             (cons #t (struct-copy state stt [recieve (rest (state-recieve stt))] [curr-index (add1 (state-curr-index stt))])))
           (cons #f stt))]
      [(equal? instr-type "snd") 
       (cons #t
             (struct-copy state stt
                          [send (cons (get-value registers (second curr-instr)) (state-send stt))]
                          [curr-index (add1 (state-curr-index stt))]))]
      [(equal? instr-type "jgz") (if (< 0 (get-value registers (second curr-instr)))
                                     (cons #t (struct-copy state stt [curr-index (+ curr-instr-index (get-value registers (third curr-instr)))]))
                                     (incr-curr-index stt))])))

(main2 given-input)
