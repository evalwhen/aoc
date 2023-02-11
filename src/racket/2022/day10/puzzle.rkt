;;--- Day 10: Cathode-Ray Tube ---
#lang racket

(require "../util.rkt")

(define (parse-line line)
  (cond
    [(equal? line "noop") (list 'noop)]
    [else (let ([parts (string-split line " ")])
            (list (string->symbol (first parts))
                  (string->number (second parts))))]))
(define (run ops)
  (letrec ([A (lambda (ops xs)
                (cond
                  [(null? ops) xs]
                  [else (A (cdr ops) (step (car ops) xs))]))])
    (A ops (vector 1))))

(define (step op xs)
  (let ([curr (vector-ref xs (sub1 (vector-length xs)))])
    (match op
      [(list 'noop) (vector-append xs (vector curr))]
      [(list 'addx x) (vector-append xs (vector curr (+ curr x)))])))

(define (signal-strengths xs cycles)
  (map
   (lambda (cycle) (* cycle (vector-ref xs (sub1 cycle))))
   cycles))

(define (puzzle1 path)
  (let* ([ops (reverse (parse-inputs path parse-line))]
         [xs (run ops)]
         [strengths (signal-strengths xs (list 20 60 100 140 180 220))]
         )
    ;; (println ops)
    ;; (println (vector-ref xs 219))
    ;; (println xs)
    (apply + strengths)
    ))

(define (render path)
  (let* ([ops (reverse (parse-inputs path parse-line))]
         [xs (run ops)]
         [in-sprite (lambda (i x) (member i (list (sub1 x) x (add1 x))))])
    (for/vector ([i (in-range 0 240)])
      (let ([rowi (remainder i 40)])
        (if (in-sprite rowi (vector-ref xs i))
            "#"
            ".")))))

(define (display-image xxs)
  (for ([i (in-range 0 240)])
    (if (= (remainder (add1 i) 40) 0)
        (begin
          (display (vector-ref xxs i))
          (display "\n"))
        (display (vector-ref xxs i)))))

;; (display-image (render "input.txt"))
(display-image (render "input1.txt"))
