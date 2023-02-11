#lang racket

(require (file "../util.rkt"))

(define/contract parse-line
  (-> string? (cons/c string? string?))
  (lambda (line)
    (let* ([len (string-length line)]
           [mid (/ len 2)])
      (cons (substring line 0 mid)
            (substring line mid len)))))

(define/contract build-priority-data
  (-> char? integer? hash?)
  (lambda (char start)
    (let ([start-char (char->integer char)])
      (for/fold ([data (hash)])
                ([sc (in-range start-char (+ start-char 26))]
                 [i (in-range start (+ start 26))])
        (hash-set data (integer->char sc) i)))))

(define/contract priority
  (-> char? integer?)
  (let ([lowercase-priority (build-priority-data #\a 1)]
        [uppercase-priority (build-priority-data #\A 27)])
    (lambda (item)
      (if (char-lower-case? item)
          (hash-ref lowercase-priority item 0)
          (hash-ref uppercase-priority item 0)))))

(define/contract common-items
  (-> string? string? set?)
  (lambda (part1 part2)
    (define set1 (for/set ([c part1])
                   c))
    (define set2 (for/set ([c part2])
                   c))
    (set-intersect set1 set2)))

(define/contract sum-priority
  (-> set? integer?)
  (lambda (s)
    (apply + (set-map s priority
                      ))))

(parse-inputs "input.txt" parse-line)

;; puzzle1
(define puzzle1
  (lambda (path)
    (apply + (map (lambda (parts)
                    (sum-priority (common-items (car parts)
                                                (cdr parts))))
                  (parse-inputs path parse-line)))))

(puzzle1 "input.txt")
(puzzle1 "input1.txt")

(define/contract group-elf
  (-> (listof any/c) integer? (listof (listof any/c)))
  (lambda (lst len)
    (letrec ([select (lambda (l i res)
                       (cond
                         ;; [(empty? l)  res]
                         [(= i 0) (values res l)]
                         [else (select (cdr l)
                                       (sub1 i)
                                       (cons (car l) res))]))]
             [group (lambda (l res)
                      (cond
                        [(empty? l) res]
                        [else (let-values ([(triple rem) (select l len '())])
                                (group rem (cons triple res)))]))])
      (group lst '()))))

(define intersect
  (lambda (lines)
    (apply set-intersect (map (lambda (line)
                                (for/set ([c line])
                                  c))
                              lines))))

(define puzzle2
  (lambda (path)
    (apply + (map (lambda (triple)
                    (apply + (set-map (intersect triple) priority)))
                  (group-elf (parse-inputs path (lambda (x) x))
                             3)))))

(puzzle2 "input.txt")
(puzzle2 "input1.txt")
