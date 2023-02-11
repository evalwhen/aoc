;; --- Day 8: Treetop Tree House ---
#lang racket

(require (file "../util.rkt"))

(define (parse-line line)
  (for/vector ([c line])
    c))

;; remove global variable
(define tree-map (list->vector (reverse (parse-inputs "input1.txt" parse-line))))
;; (define tree-map (list->vector (reverse (parse-inputs "input.txt" parse-line))))

(define (tree-high pos)
  (vector-ref (vector-ref tree-map (car pos)) (cdr pos)))

(define (traverse-map tm pos-processor)
  (let ([col-len (vector-length tree-map)]
        [row-len (vector-length (vector-ref tree-map 0))])
    (values col-len
            row-len
            (for*/fold ([visited (hash)])
                       ([ci (in-range 1 (sub1 col-len))]
                        [ri (in-range 1 (sub1 row-len))])
              (hash-set visited (cons ci ri) (pos-processor ci ri row-len col-len))))))

(define (lower-neighbor i j rowlen collen)
  (filter
   (lambda (direct)
     (let-values ([(res _) (visible (cons i j) direct rowlen collen)])
       res))
   (list 'left 'right 'top 'down)))

day8 part2

(define (pos-score i j rowlen collen)
  (apply * (map
            (lambda (direct)
              (let-values ([(_ score) (visible (cons i j) direct rowlen collen)])
                (if (= score 0) 1 score)))
            ;; (list 'left 'right 'top 'down)
            (list 'top 'left 'right 'down)
            )))


(define (make-step direct)
  (lambda (pos)
    (case direct
      ['right (cons (car pos)
                    (add1 (cdr pos)))]
      ['left (cons (car pos)
                   (sub1 (cdr pos)))]
      ['top (cons (sub1 (car pos))
                  (cdr pos))]
      ['down (cons (add1 (car pos))
                   (cdr pos))])))

(define (make-done? direct rowlen collen)
  (lambda (pos)
    (case direct
      ['right (>= (cdr pos) rowlen)]
      ['left (= (cdr pos) -1)]
      ['top (= (car pos) -1)]
      ['down (>= (car pos) collen)]
      [else (error 'unkown-pos)])))

day8 part3

(define visible
  (lambda (pos direct rowlen collen)
    (letrec ([step (make-step direct)]
             [done? (make-done? direct rowlen collen)]
             [A (lambda (neibor score)
                  ;; (println neibor)
                  ;; (println (tree-high-2 neibor))
                  (cond
                    [(done? neibor) (values true score)]
                    [(char>? (tree-high pos)
                             (tree-high neibor)) (A (step neibor) (add1 score))]
                    [else (values false (add1 score))]))])
      (A (step pos) 0))))

;; (traverse-map tree-map)
;; (visible (cons 2 1) 'right 5 5)
;; (visible (cons 2 1) 'top 5 5)

(define puzzle1
  (lambda ()
    (let-values ([(collen rowlen hs) (traverse-map tree-map lower-neighbor)])
      (+ (- (* 2 (+ collen rowlen))
            4)
         (for/fold ([total 0])
                   ([v (in-hash-values hs)]
                    #:when (> (length v) 0))
           (add1 total))))))

(time (puzzle1))

(define puzzle2
  (lambda ()
    (let-values ([(_ x hs) (traverse-map tree-map pos-score)])
      (for/max ([v (in-hash-values hs)])
        v))))

(puzzle2)
;; (traverse-map tree-map pos-score)

;; (pos-score 3 2 5 5)
