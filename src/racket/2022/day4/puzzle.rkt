#lang racket

(require (file "../util.rkt"))


(define/contract include?
  (-> (listof cons?) boolean?)
  (lambda (ps)
    (let ([pair1 (car ps)]
          [pair2 (cadr ps)])
      (or (and (<= (car pair1) (car pair2))
               (>= (cdr pair1) (cdr pair2)))

          (and (>= (car pair1) (car pair2))
               (<= (cdr pair1) (cdr pair2)))))))

(define/contract overlap?
  (-> (listof cons?) boolean?)
  (lambda (ps)
    (let ([pair1 (car ps)]
          [pair2 (cadr ps)])
      (or (include? ps)
          (<= (car pair2) (cdr pair1) (cdr pair2))
          (<= (car pair2) (car pair1) (cdr pair2))))))


(define/contract parse-line
  (-> string? (listof cons?))
  (lambda (line)
    (match (string-split line ",")
      [(list p1 p2) (list (parse-pair p1)
                          (parse-pair p2))])))
(define/contract parse-pair
  (-> string? cons?)
  (lambda (p)
    (match (string-split p "-")
      [(list s e) (cons (string->number s)
                        (string->number e))])))

(parse-inputs "input.txt" parse-line)

(define puzzle1
  (lambda (path)
    (length (filter include?
                    (parse-inputs path parse-line)))))

(puzzle1 "input.txt")
(puzzle1 "input1.txt")

(define puzzle2
  (lambda (path)
    (length (filter overlap?
                    (parse-inputs path parse-line)))))

(puzzle2 "input.txt")
(puzzle2 "input1.txt")
