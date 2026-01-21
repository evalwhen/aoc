;; 选定一个小行星 A 作为监测站：
;; 对于地图上的其他小行星，计算他们相对于 A 的化简向量，并把向量放入集合，最终集合的大小就是，从 A 能看到的所有其他行星，
;; 感觉这是一道数学题，而不是编程题目

(declare (standard-bindings) (extended-bindings))

(include "./myenv-gambit.scm")

(define get-data
  (lambda (infile)
    (call-with-input-file infile
      (lambda (p)
        (let ((queue (make-queue)))
          (let lp ((y 0) (line (read-line p)))
            (if (eof-object? line)
                (queue->list queue)
                (let lp-inner ((x 0))
                  (cond
                   ((= x (string-length line)) (lp (+ y 1) (read-line p)))
                   ((equal? #\# (string-ref line x))
                    (enqueue! queue (cons x y))
                    (lp-inner (+ x 1)))
                   (else (lp-inner (+ x 1))))))))))))

(define count-single-point
  (lambda (a points)
    (let ((table (make-table test: equal?)))
      (for-each (lambda (b) (let* ((dx (- (car a) (car b)))
                                   (dy (- (cdr a) (cdr b)))
                                   (v (abs (gcd dx dy))))
                              (when (not (= v 0))
                                ;; (displayln (cons (/ dx v) (/ dy v)))
                                ;; (displayln b)
                                (table-set! table (cons (/ dx v) (/ dy v)) #f))))
                points)
      (table-length table))))


;; (displayln (get-data "day10.input"))
;; (displayln (count-single-point (cons 3 4) (get-data "day10.input")))

(define (run-part1 infile)
  (let ((points (get-data infile)))
    (map (lambda (p) (count-single-point p points))
         points)))

(displayln  (apply max (run-part1 "day10.input")))
