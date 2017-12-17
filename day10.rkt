#lang racket
(require rackunit)
(define ex-list (build-list 5 values))

(define ex-inputs '(3 4 1 5))

(define (apply-input list skip inp)
  (let*-values ([(l rst) (split-at list inp)]
                [(inter) (append rst (reverse l))]
                [(skipped rm) (split-at inter (modulo skip (length list)))])
    (append rm skipped)))

(define (main* stt skip inputs)
  (if
   (empty? inputs)
   stt
   (main* (apply-input stt skip (first inputs)) (add1 skip) (rest inputs))))

(define (find-first inputs val-length skip)
  (modulo (- (+ (foldr + 0 (build-list skip values))
                (foldr + 0 inputs))) val-length))
   

(define input '(63 144 180 149 1 255 167 84 125 65 188 0 2 254 229 24))

#;(begin 
    (main* ex-list 0 ex-inputs)
    (find-first ex-inputs 5)
    (define part1 (main* (build-list 256 values) 0 input))
    (define part1-start (find-first input 256)))

(define part2-input "63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24")
(define str->bytes (λ (s) (append (map char->integer (string->list s)) extra-inputs)))
(define extra-inputs '(17 31 73 47 23))
(str->bytes "1,2,3")

(define (main2 inputs)
  (stt->hash
   (main2* (build-list 256 values) 0 (str->bytes inputs) 64)))

(define (stt->hash stt)
  (list->string (flatten
                 (map integer->hex (get-dense-hash stt)))))
(define (main2* stt skip inputs rounds)
  (if
   (zero? rounds)
   (let ([offset (modulo (+ (- (* 64 (foldr + 0 inputs)))
                            (- (foldl + 0 (build-list (* 64 (length inputs)) values)))) (length stt))])
     (append (drop stt offset) (take stt offset)))
   
   (main2* (main* stt skip inputs) (+ (length inputs) skip) inputs (sub1 rounds))))

(define (get-dense-hash l)
  (if (empty? l) '()
      (cons (foldl bitwise-xor 0 (take l 16)) (get-dense-hash (drop l 16)))))

(define (integer->hex i)
  (list 
   (int->hex (floor (/ i 16)))
   (int->hex (modulo i 16))))

(define int-hex  '((0 #\0)
                   (1 #\1)
                   (2 #\2)
                   (3 #\3)
                   (4 #\4)
                   (5 #\5)
                   (6 #\6)
                   (7 #\7)
                   (8 #\8)
                   (9 #\9)
                   (10 #\a)
                   (11 #\b)
                   (12 #\c)
                   (13 #\d)
                   (14 #\e)
                   (15 #\f)))

(define (int->hex i)
  (second (assv i int-hex)))

(check-equal? (main2 "AoC 2017") "33efeb34ea91902bb2f59c9920caa6cd")
(check-equal? (main2 "1,2,3") "3efbe78a8d82f29979031a4aa0b16a9d")
(check-equal? (main2 "1,2,4") "63960835bcdc130f0b66d7ff4f6a5a8e")
(check-equal? (main2 "") "a2582a3a0e66e6e86e3812dcb672a272")
