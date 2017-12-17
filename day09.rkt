#lang racket

(define input (string-trim (file->string "/home/skye/day9.txt")))

(define-struct state (in-garbage? escape-next? rec-score total-score garbage-chars) #:transparent)
(define (get-init-state)
  (make-state #f #f 0 0 0))

(define example "{{{},{},{{}}}}")

(define (part1 inp)
  (foldr part1* (get-init-state) (reverse (string->list inp))))

(define (part1* c stt)
  (cond
    [(state-escape-next? stt) (struct-copy state stt [escape-next? #f])]
    [(and (equal? c #\!) (state-in-garbage? stt))
     (struct-copy state stt [escape-next? #t])]
    [(and (equal? c #\<) (not (state-in-garbage? stt)))
     (struct-copy state stt [in-garbage? #t])]
    [(and (equal? c #\{) (not (state-in-garbage? stt)))
     (struct-copy state stt
                  [rec-score (add1 (state-rec-score stt))]
                  [total-score (+ 1 (state-rec-score stt) (state-total-score stt))])]
    [(and (equal? c #\}) (not (state-in-garbage? stt)))
     (struct-copy state stt
                  [rec-score (sub1 (state-rec-score stt))])]
    [(and (equal? c #\>) (state-in-garbage? stt))
     (struct-copy state stt
                  [in-garbage? #f])]
    [(state-in-garbage? stt)
     (struct-copy state stt
                  [garbage-chars (add1 (state-garbage-chars stt))])]
    [else stt]; handle not in garbage
    ))

(part1 example)
(part1 "{<a>,<a>,<a>,<a>}")
(part1 "{{<ab>},{<ab>},{<ab>},{<ab>}}")
(part1 "{{<!!>},{<!!>},{<!!>},{<!!>}}")
(part1 "{{<a!>},{<a!>},{<a!>},{<ab>}}")
(part1 input)
