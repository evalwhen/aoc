;; --- Day 11: Monkey in the Middle ---
#lang racket

(require "../util.rkt")

(struct Monkey (id items trans test visited)
  #:transparent)

(define (op sym)
  (match sym
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]))

(define (make-trans monkey)
  (lambda (item)
    (match (Monkey-trans monkey)
      [(list o #f) ((op o) item item)]
      [(list o v) ((op o) item v)])))

(define (make-test monkey)
  (lambda (item)
    (match (Monkey-test monkey)
      [(list c t f) (if (= (remainder item c) 0) t f)])))

(define (turn monkey monkeys)
  (cond
    [(empty? (Monkey-items monkey)) monkeys]
    [else (let ([m (throw monkey
                          (floor (/ ((make-trans monkey) (car (Monkey-items monkey)))
                                    3))
                          monkeys)])
            (turn (dict-ref m (Monkey-id monkey))
                  m))]))

(define (throw monkey item monkeys)
  (let* ([id ((make-test monkey) item)]
         [target (dict-ref monkeys id)]
         [mk  (struct-copy Monkey target
                           [items (append (Monkey-items target)
                                          (list item))])])
    (dict-set (dict-set monkeys id mk)
              (Monkey-id monkey)
              (struct-copy Monkey monkey
                           [items (cdr (Monkey-items monkey))]
                           [visited (add1 (Monkey-visited monkey))]))))

(define (round monkeys)
  (letrec ([A (lambda (i monkeys)
                (cond
                  [(= i (length monkeys)) monkeys]
                  [else (A (add1 i)
                           (turn (dict-ref monkeys i) monkeys))]))])
    (A 0 monkeys)))

;; (parse-inputs "input.txt" (lambda (x) x))

(define (split-list lst seperator)
  (letrec ([A (lambda (lst part res)
                (cond
                  [(empty? lst) (cons part res)]
                  [(equal? (car lst) seperator)
                   (A (cdr lst)
                      '()
                      (cons part res))]
                  [else (A (cdr lst)
                           (cons (car lst) part)
                           res)]))])
    (A lst '() '())))

(define (parse-monkey lst)
  (let ([id (parse-id (car lst))]
        [items (parse-items (cadr lst))]
        [trans (parse-trans (caddr lst))]
        [test (parse-test (cdddr lst))])
    (Monkey id items trans test 0)))

(define (parse-id line)
  (string->number (last (string-split (substring line 0 (sub1 (string-length line)))
                                      " "))))
(define (parse-items line)
  (map string->number
       (string-split (cadr (string-split line ": "))
                     ", ")))

(define (parse-trans line)
  (match (string-split line " ")
    [(list _ _ _ "=" "old" op v2) (list (string->symbol op) (string->number v2))]))

(define (parse-test lines)
  (let ([v (string->number (last (string-split (car lines) " ")))]
        [t (string->number (last (string-split (cadr lines) " ")))]
        [f (string->number (last (string-split (caddr lines) " ")))])
    (list v t f)))

(define (parse-monkeys path)
  (let ([lsts (split-list (parse-inputs path (lambda (x) x)) "")])
    (define-values (_ monkeys )
      (for*/fold ([i 0]
                  [monkeys '()])
                 ([lst lsts])
        (values (add1 i) (dict-set monkeys i (parse-monkey lst)))))
    monkeys))


(define (round-times n path)
  (letrec ([A (lambda (n monkeys)
                (cond
                  [(= n 0) monkeys]
                  [else (A (sub1 n) (round monkeys))]))])
    (A n (parse-monkeys path))))


(define (puzzle1 path)
  (let* ([monkeys (round-times 20 path)]
         [visits (dict-map monkeys (lambda (_ m)
                                     (Monkey-visited m)))])
    (apply * (take (sort visits >) 2))))


;; (define v (round-times 1 "example.txt"))
;; (puzzle1 "example.txt")
(puzzle1 "input.txt")
