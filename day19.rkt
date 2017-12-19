#lang racket

(define real-input (file->string "day19.input"))

(define path-dim 201)

(define (main input)
  (let* ([letters (regexp-match* #px"\\w" input)]
         [output (main* (find-entrance input) 0 'down (string-replace input "\n" "") letters '() 0)])
    (print (cdr output))
    (list->string (reverse (car output)))))

(define (main* x y dir path final-letters letters steps)
  (if (equal? (length letters) (length final-letters))
      (cons letters steps)
      (let ([dumb (get-next-square x y dir path letters)])
        (main* (second dumb) (third dumb) (fourth dumb) path final-letters (first dumb) (add1 steps)))))
      

(define (find-entrance input)
  (index-of (string->list (first (string-split input "\n"))) #\|))

(define (go-left x y)
  (list (sub1 x) y 'left))

(define (go-right x y)
  (list (add1 x) y 'right))

(define (go-up x y)
  (list x (sub1 y) 'up))

(define (go-down x y)
  (list x (add1 y) 'down))

(define (go-left-or-right x y path)
  (if (on-path (sub1 x) y path)
      (go-left x y)
      (go-right x y)))

(define (go-up-or-down x y path)
  (if (on-path x (add1 y) path)
      (go-down x y)
      (go-up x y)))

(define (get-next-square x y dir path letters)
  (let ([letters* (if  (regexp-match #px"\\w" (string (string-ref path (+ x (* 201 y)))))
                       (cons (string-ref path (+ x (* 201 y))) letters)
                       letters)])
    (cons letters* (cond
                     [(eq? dir 'up) (if (on-path x (sub1 y) path)
                                        (go-up x y)
                                        (go-left-or-right x y path))]
                     [(eq? dir 'down) (if (on-path x (add1 y) path)
                                          (go-down x y)
                                          (go-left-or-right x y path))]
                     [(eq? dir 'left) (if (on-path (sub1 x) y path)
                                          (go-left x y)
                                          (go-up-or-down x y path))]
                     [(eq? dir 'right) (if (on-path (add1 x) y path)
                                           (go-right x y)
                                           (go-up-or-down x y path))]))))

(define (on-path x y path)
  (let ([cell (+ x (* 201 y))])
    (if (and (< -1 cell (string-length path))
             (< -1 x 201)
             (< -1 y 201))
        (not (equal? #\space (string-ref path (+ x (* 201 y)))))
        #f)))

(define test-input "     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ ")

(define test-input-2 (string-replace test-input "\n" ""))

