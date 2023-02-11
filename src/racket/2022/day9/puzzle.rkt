;; --- Day 9: Rope Bridge ---
#lang racket

(require "../util.rkt")

(define (step direction paths)
  (letrec ([A (lambda (paths)
                ;; (println paths)
                (cond
                  [(null? (cdr paths)) paths]
                  [(adjacent? (car paths) (cadr paths)) paths]
                  [else (let ([next (tailnext (car paths) (cadr paths))])
                          (cons (car paths) (A (cons next (cddr paths)))))]))]
           [S (headernext direction (car paths))])
    (A (cons S (cdr paths)))))

;; i is row num, j is col num.
(define (adjacent-poses pos)
  (let* ([i (car pos)]
         [j (cdr pos)]
         [up (cons (sub1 i) j)]
         [upright (cons (sub1 i) (add1 j))]
         [right (cons i (add1 j))]
         [rightdown (cons (add1 i) (add1 j))]
         [down (cons (add1 i) j)]
         [downleft (cons (add1 i) (sub1 j))]
         [left (cons i (sub1 j))]
         [upleft (cons (sub1 i) (sub1 j))])
    (list up upright right rightdown down downleft left upleft)))

(define (adjacent? path1 path2)
  (let ([headerpos (car path1)]
        [tailpos (car path2)])
    (or (equal? headerpos tailpos)
        (member tailpos
                (adjacent-poses headerpos)))))

(define (headernext direction path)
  (let* ([header (car path)]
         [i (car header)]
         [j (cdr header)])
    (cons (case direction
            ['R (cons i (add1 j))]
            ['D (cons (add1 i) j)]
            ['L (cons i (sub1 j))]
            ['U (cons (sub1 i) j)])
          path)))

(define (tailnext  headerpath tailpath)
  (let* ([headercurr (car headerpath)]
         [tailcurr (car tailpath)]
         [next (next headercurr tailcurr)])
    ;; (println "here")
    (cons next
          tailpath)))

(define (next hd tl)
  (let-values ([(m1 m2) (modifier2 (list (- (car hd) (car tl))
                                         (- (cdr hd) (cdr tl))))])
    (cons (m1 (car tl))
          (m2 (cdr tl)))))

(define (id x)
  x)
(define (modifier diff)
  (match diff
    ;; [0 id]
    [1 add1]
    [-1 sub1]
    [2 add1]
    [-2 sub1]))

(define (modifier2 diffs)
  (match diffs
    [(list 0 diff) (values id (modifier diff))]
    [(list diff 0) (values (modifier diff) id)]
    [(list diff1 diff2) (values (modifier diff1) (modifier diff2))]))

(define (move steps paths)
  (cond
    [(null? steps) paths]
    [else (move (cdr steps) (execute-step (car steps) paths))]))

(define (execute-step st paths)
  (letrec ([direction (car st)]
           [number (cdr st)]
           [A (lambda (n paths)
                (cond
                  [(= n 0) paths]
                  [else (A (sub1 n) (step direction paths))]))])
    (A number paths)))

(define (parse-line line)
  (match (string-split line " ")
    [(list d n) (cons (string->symbol d) (string->number n))]
    [else (error 'parse-line-failed)]))

(define (puzzle filename knots-size)
  (let ([steps (reverse (parse-inputs filename parse-line))])
    (let ([paths (move steps (generate-paths knots-size))])
      (length (remove-duplicates (last paths))))))

(define (generate-paths knots-size)
  (for/list
      ([_ (in-range 0 knots-size)])
    (list (cons 0 0))))

;; (puzzle "input.txt" 2)
(puzzle "input1.txt" 10)
