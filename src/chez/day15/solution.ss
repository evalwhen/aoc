;; --- Day 15: Beacon Exclusion Zone ---
;; range based solution

(import
  (rnrs)
  ;; (rnrs hashtables)
  (utils)
  (strings)
  (list)
  )

;; (define MAXI 20)
(define MAXI 4000000)

;; parser
(define (list->pair lst)
  (if (= (length lst) 2)
    (cons (car lst)
          (cadr lst))
    (error 'list->pair "invalid arguments" lst)))

;; Sensor at x=2, y=18: closest beacon is at x=-2, y=15
(define (parse-line line)
  (let* ([idxes (substr-indexes "at " line)]
         [parse-pos (lambda (raw)
                      (list->pair (map (lambda (p) (string->number (cadr (string-split p "="))))
                                       (string-split raw ", "))))])
    (map parse-pos (list (substring line (car idxes) (- (substr-index ":" line) 1))
                         (substring line (cadr idxes) (string-length line))))))

(define (parse-input path)
  (map parse-line (get-lines path)))


;; utils
(define (distance start end)
  (+ (abs (- (car start) (car end)))
     (abs (- (cdr start) (cdr end)))))

;; ranges
(define (sort-ranges rs)
  (list-sort (lambda (a b) (< (car a) (car b)))
        rs))

;; rs(ranges) should be sorted by start
(define (collapse rs)
  (letrec* ([iter (lambda (rs)
                    (cond
                     [(or (null? rs)
                          (null? (cdr rs))) rs]
                     [(merge (car rs) (cadr rs)) => (lambda (r)
                                                      (iter (cons r (cddr rs))))]
                     [else (cons (car rs) (iter (cdr rs)))]))]
            [merge (lambda (a b)
                     (let* ([as (car a)] [ae (cdr a)] [bs (car b)] [be (cdr b)])
                       (cond
                        [(<= (dec bs) ae) (if (< ae be) (cons as be) a)]
                        [else #f])))])
    (iter (sort-ranges rs))))

(define (ranges y sensors)
  (letrec* ([range (lambda (sensor)
                     (let* ([spos (car sensor)]
                            [bpos (cadr sensor)]
                            [d (distance spos bpos)]
                            [middle (car spos)]
                            [yd (abs (- y (cdr spos)))]
                            [diff (- d yd)])
                       (if (<= yd d)
                         (cons (- middle diff) (+ middle diff))
                         #f)))]
            [iter (lambda (sensors res)
                    (cond
                     [(null? sensors) res]
                     [(range (car sensors)) => (lambda (v) (iter (cdr sensors)
                                                             (cons v res)))]
                     [else (iter (cdr sensors) res)]))])
    (iter sensors '())))

(define (beacons pred? sensors)
  (letrec ([iter (lambda (sensors res)
                   (cond
                    [(null? sensors) res]
                    [(pred? (cadar sensors)) (iter (cdr sensors)
                                                    (cons (cadar sensors) res))]
                    [else (iter (cdr sensors)
                                res)]))])

    (dedupe (iter sensors '()))))

;; limit ranges to 0 - 2000000
;; need a utils for filter and change
(define (narrow ranges m)
  (letrec* ([iter (lambda (ranges res)
                    (cond
                     [(null? ranges) res]
                     [(change (car ranges)) => (lambda (r) (iter (cdr ranges) (cons r res)))]
                     [else (iter (cdr ranges) res)]))]
            [change (lambda (r)
                      (let* ([start (car r)]
                             [end (cdr r)]
                             [ns (max 0 start)]
                             [ne (min m end)])
                        (if (<= ns ne)
                          (cons ns ne)
                          #f)))])
    (sort-ranges (iter ranges '()))))

;; puzzle1
(define (count-nobeacons y sensors)
  (let ([range-size (lambda (r) (inc (- (cdr r) (car r))))]
        [beacon-count (length (beacons (lambda (beacon)
                                         (= (cdr beacon) y))
                                       sensors))])

    (map (lambda (r) (- (range-size r) beacon-count))
         (collapse (sort-ranges (ranges y sensors))))))

;; puzzle2
(define (find-beacon path m)
  (letrec* ([iter (lambda (i)
                    (cond
                     [(< m i) #f]
                     [(double-range? i) => (lambda (v) v)]
                     [else (iter (inc i))]))]
            [sensors (parse-input path)]
            [double-range? (lambda (i)
                             (let ([rs (narrow (collapse (sort-ranges (ranges i sensors))) m)])
                               (if (> (length rs) 1)
                                 (cons (dec (caadr rs)) i)
                                 #f)
                               ))])
    (iter 0)))

(define (tuning pos m)
  (+ (* (car pos) m)
     (cdr pos)))
;; test
(displayln (find-beacon "example.txt" 20))
(displayln (tuning (find-beacon "input.txt" 4000000) 4000000))