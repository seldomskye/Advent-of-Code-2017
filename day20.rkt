#lang racket
(require math)
(define input (file->string "day20.input"))

(struct coords (x y z) #:transparent)

(struct particle (pos v a) #:transparent)

(define (parse input)
  (map (λ (s) (parse-vals (string-split s ", "))) (string-split (string-trim input) "\n")))

(define (parse-to-assocs input)
  (parse-to-assocs* (string-split (string-trim input) "\n") 0))

(define (parse-to-assocs* rows acc)
  (if (empty? rows)
      '()
      (cons (cons acc (parse-vals (string-split (first rows) ", "))) (parse-to-assocs* (rest rows) (add1 acc)))))

(define (parse-vals row)
  (apply particle (map (λ (t) (apply coords (map string->number (string-split (substring t 3 (sub1 (string-length t))) ",")))) row)))

(define (manhattan-distance c)
  (+ (abs (coords-x c))
     (abs (coords-y c))
     (abs (coords-z c))))

(define (sorted-on-accel particles)
  (sort particles total-distance-sort))

(define (total-distance-sort p1 p2)
  (or
   (< (manhattan-distance (particle-a p1))
        (manhattan-distance (particle-a p2)))
   (and (= (manhattan-distance (particle-a p1))
             (manhattan-distance (particle-a p2)))
          (< (manhattan-distance (particle-v p1))
             (manhattan-distance (particle-v p2))))
   (and (= (manhattan-distance (particle-a p1))
             (manhattan-distance (particle-a p2)))
          (= (manhattan-distance (particle-v p1))
             (manhattan-distance (particle-v p2)))
          (< (manhattan-distance (particle-pos p1))
             (manhattan-distance (particle-pos p2))))))
  
(define (move-particles lop)
  (map move-particle lop))

(define (move-particle p)
  (struct-copy particle p
               [v (coord-add (particle-v p) (particle-a p))]
               [pos (coord-add (particle-v p) (particle-a p) (particle-pos p))]))

(define (get-seen h lop)
  (cond
    [(empty? lop) h]
    [else (get-seen (hash-update
           h
           (particle-pos (first lop))
           add1
           0) (rest lop))]))

(define (remove-crashes lop)
  (let ([dupe-hash (get-seen (hash) lop)])
    (filter (λ (p) (= 1 (hash-ref dupe-hash (particle-pos p)))) lop)))

(define (main2 input)
  (length (main2* (parse input) 0)))

(define (main2* lop since-last-removal)
  (let ([lop* (remove-crashes (move-particles lop))])
    (if (< 100 since-last-removal)
        lop
        (main2* lop* (if (equal? (length lop) (length lop*))
                        (add1 since-last-removal)
                        0)))))


(define coord-add (λ c
  (coords (apply + (map coords-x c))
         (apply + (map coords-y c))
         (apply + (map coords-z c)))))

(main2 input)