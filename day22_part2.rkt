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
    (foldl (lambda (c h) (hash-set h (get-posn c size) (char->state (string-ref packed-string c)))) (hash) (range (* size size)))))

(define (char->state char)
  (if (equal? #\# char)
      'infected
      'clean))

(define (status->char b)
  (case b
    ['clean #\.]
    ['flagged #\F]
    ['weakened #\W]
    ['infected #\#]))

(define (get-posn n size)
  (posn (modulo n size) (floor (/ n size))))

(define (next-state s h infected)
  (let ([dir* (get-next-dir s h)]
        [h* (hash-update h (state-pos s) (lambda (i) (when (equal? 'weakened i)
                                                       (set-box! infected (add1 (unbox infected))))
                                           (get-next-status i))
                         'clean)])
    (list (state dir* (apply-dir dir* (state-pos s))) h*)))

(define status '((clean . weakened)
                 (weakened . infected)
                 (infected . flagged)
                 (flagged . clean)))

(define (get-next-status s)
  (cdr (assoc s status)))

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
(define flagged '((n . s)
                  (s . n)
                  (w . e)
                  (e . w)))

(define (get-next-dir state h)
  (case (hash-ref h (state-pos state) 'clean)
    ['clean (cdr (assoc (state-dir state) left))]
    ['weakened (state-dir state)]
    ['infected (cdr (assoc (state-dir state) right))]
    ['flagged (cdr (assoc (state-dir state) flagged))]
    [else (println "this is happening")]))
     


(define ex-input "..#
#..
...")

(define (main input loops start)
  (let ([infected (box 0)])
    (main* (state 'n start) (parse-input input) loops infected)
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
    (display (string-join (map (lambda (r) (list->string (map (lambda (p) (status->char (hash-ref h p 'clean))) r)))
         (map (lambda (y) (map (lambda (x) (posn x y)) (range min-x (add1 max-x)))) (range min-y (add1 max-y)))) "\n"
                                                                                                                 #:after-last "\n"))))

(main ex-input 100 (posn 1 1))
#;(main ex-input 10000000 (posn 1 1))
(main input 10000000 (posn 12 12))