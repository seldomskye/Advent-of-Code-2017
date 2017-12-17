#lang racket
(define (init-state) (string-append "abcdefghijklmnop"))


(define input (string-trim (file->string "day16.input")))

(define (main inp)
  (main* inp (init-state)))

(define (main* inp stt)
  (foldl run-step stt (string-split inp ",")))

(define (run-step step stt)
  (cond
    [(equal? (string-ref step 0) #\s) (spin stt (string->number (substring step 1)))]
    [(equal? (string-ref step 0) #\x) (swap stt (map string->number (string-split (substring step 1) "/")))]
    [(equal? (string-ref step 0) #\p) (swap-chars stt (string-split (substring step 1) "/"))]))

(define (spin stt spins)
  (let ([len (string-length stt)])
    (string-append (substring stt (- len spins)) (substring stt 0 (- len spins)))))

(define (swap stt l)
  (let ([ret (string-append stt)])
    (string-set! ret (first l) (string-ref stt (second l)))
    (string-set! ret (second l) (string-ref stt (first l)))
    ret))
(define (swap-chars stt l)
  (string-replace (string-replace (string-replace (string-append stt) (first l) "_") (second l) (first l)) "_" (second l)))

(define (main2 inp loops)
  (let ([seen-states (mutable-set)])
    (main2* seen-states inp (init-state) loops)))

(define (main2* seen-states inp stt loops)
  (cond [(zero? loops) stt]
        [(set-member? seen-states stt) (print "cycle found") (println loops) stt]
        [else (let ([stt* (main* inp stt)])
        (set-add! seen-states stt)
        (main2* seen-states inp stt* (sub1 loops)))]))

