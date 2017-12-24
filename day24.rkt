#lang racket
(require math)
(define input "42/37
28/28
29/25
45/8
35/23
49/20
44/4
15/33
14/19
31/44
39/14
25/17
34/34
38/42
8/42
15/28
0/7
49/12
18/36
45/45
28/7
30/43
23/41
0/35
18/9
3/31
20/31
10/40
0/22
1/23
20/47
38/36
15/8
34/32
30/30
30/44
19/28
46/15
34/50
40/20
27/39
3/14
43/45
50/42
1/33
6/39
46/44
22/35
15/20
43/31
23/23
19/27
47/15
43/43
25/36
26/38
1/10")

(define ex-input "0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10")
(struct state (conns next poss-conns) #:transparent)

(define (part1 input)
  (let ([v (make-hash)])
  (part1* (state '() 0 (map (lambda (r) (map string->number (string-split r "/"))) (string-split input "\n"))) v)
    v))

(define (part1* stt v)
  (let ([lstt* (next-states stt v)])
    (when (not (empty? lstt*))
        (map (lambda (l) (part1* l v)) lstt*))))

(define (next-states stt v)
  (hash-update! v (length (state-conns stt)) (lambda (e) (max (sum (map (lambda (l) (apply + l)) (state-conns stt))) e)) 0)
  (let ([next-cons (possible-connecs (state-next stt) (state-poss-conns stt))])
    (map (lambda (c) (state
                      (cons c (state-conns stt))
                      (first (remove (state-next stt) c))
                      (remove c (state-poss-conns stt))))
         next-cons)))

(define (possible-connecs n conns)
  (filter (lambda (c) (or (= (first c) n) (= (second c) n))) conns))

(define input-assocs (hash->list (part1 input)))
(define part1-out (sort input-assocs < #:key cdr))
(define part2-out (sort input-assocs < #:key car))
