#lang racket
(define input "###.#######...#####.#..##
.####...###.##...#..#....
.#.#...####.###..##..##.#
########.#.#...##.#.##.#.
..#.#...##..#.#.##..####.
..#.#.....#....#####..#..
#.#..##...#....#.##...###
.#.##########...#......#.
.#...#..##...#...###.#...
......#.###.#..#...#.####
.#.###.##...###.###.###.#
.##..##...#.#.#####.#...#
#...#..###....#.##.......
####.....######.#.##..#..
..#...#..##.####.#####.##
#...#.#.#.#.#...##..##.#.
#####.#...#.#.#.#.##.####
....###...#.##.#.##.####.
.#....###.#####...#.....#
#.....#....#####.#..#....
.#####.#....#..##.#.#.###
####.#..#..##..#.#..#.###
.##.##.#.#.#.#.#..####.#.
#####..##.#.#..#..#...#..
#.#..#.###...##....###.##")

(struct posn (x y) #:transparent)
(struct state (dir pos) #:transparent)

(define (parse-input input)
  (let* ([lol (string-split input "\n")]
         [packed-string (apply string-append lol)]
         [size (length lol)])
    (foldl (lambda (c h) (hash-set h (get-posn c size) (is-infected (string-ref packed-string c)))) (hash) (range (* size size)))))

(define (is-infected char)
  (equal? #\# char))

(define (bool->inf b)
  (if b #\# #\.))

(define (get-posn n size)
  (posn (modulo n size) (floor (/ n size))))

(define (next-state s h infected)
  (let ([dir* (get-next-dir s h)]
        [h* (hash-update h (state-pos s) (lambda (i) (when (not i)
                                                       (set-box! infected (add1 (unbox infected))))
                                           (not i))
                         #f)])
    (list (state dir* (apply-dir dir* (state-pos s))) h*)))

(define offset `((n . ,(posn 0 -1))
                 (s . ,(posn 0 1))
                 (e . ,(posn 1 0))
                 (w . ,(posn -1 0))))

(define (add-pos p1 p2)
  (posn (+ (posn-x p1) (posn-x p2))
        (+ (posn-y p1) (posn-y p2))))

(define (apply-dir dir pos)
  (add-pos pos (cdr (assoc dir offset))))

(define right '((n . e)
                (e . s)
                (s . w)
                (w . n)))
(define left '((n . w)
               (w . s)
               (s . e)
               (e . n)))


(define (get-next-dir state h)
  (if (hash-ref h (state-pos state) #f)
      (cdr (assoc (state-dir state) right))
      (cdr (assoc (state-dir state) left))))


(define ex-input "..#
#..
...")

(define (main input loops)
  (let ([infected (box 0)])
    (main* (state 'n (posn 12 12)) (parse-input input) loops infected)
    infected))

(define (main* s h loops infected)
  #;(print-h s h)
  (if (zero? loops)
      (list s h)
      (let ([rez (next-state s h infected)])
        (main* (first rez) (second rez) (sub1 loops) infected))))

(define (print-h s h)
  (println "------")
  (println s)
  (let* ([pairs (hash->list h)]
         [min-x (posn-x (car (argmin (lambda (p) (posn-x (car p))) pairs)))]
         [max-x (posn-x (car (argmax (lambda (p) (posn-x (car p))) pairs)))]
         [min-y (posn-y (car (argmin (lambda (p) (posn-y (car p))) pairs)))]
         [max-y (posn-y (car (argmax (lambda (p) (posn-y (car p))) pairs)))])
    (display (string-join (map (lambda (r) (list->string (map (lambda (p) (bool->inf (hash-ref h p #f))) r)))
         (map (lambda (y) (map (lambda (x) (posn x y)) (range min-x (add1 max-x)))) (range min-y (add1 max-y)))) "\n"
                                                                                                                 #:after-last "\n"))))

(main ex-input 10000)