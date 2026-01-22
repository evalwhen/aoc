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

;; part2
;
(define (table-keys ktbl)
  (map car (table->list ktbl)))

(define (find-best-station infile)
  (let* ((pts (get-data infile))
         (counts (map (lambda (p) (cons p (count-single-point p pts))) pts)))
    ;; 找到 count 最大的那个 pair: ((x . y) . count)
    (car (list-sort (lambda (a b) (> (cdr a) (cdr b))) counts))))
;; 计算距离的平方（用于排序，没必要开根号）
(define (dist-sq a b)
  (let ((dx (- (car a) (car b)))
        (dy (- (cdr a) (cdr b))))
    (+ (* dx dx) (* dy dy))))

;; 计算顺时针角度 (0 为正上方)
;; 使用 atan2(dx, -dy) 可以直接得到 12点钟方向为0，顺时针增长的弧度
(define (get-angle station target)
  (let* ((dx (- (car target) (car station)))
         (dy (- (cdr target) (cdr station)))
         ;; 默认 atan2 在 Scheme 里通常是 (atan dy dx)
         ;; 我们想要 0 弧度在正上方，顺时针。
         ;; 数学技巧：(atan dx (- dy))
         (angle (atan dx (- dy))))
    ;; atan 返回 (-pi, pi]，转换为 [0, 2pi)
    (if (< angle 0) (+ angle (* 2 (acos -1))) angle)))

(define (run-part2 infile target-n)
  (let* ((best-data (find-best-station infile))
         (station (car best-data))
         (all-pts (get-data infile))
         (others (remove (lambda (p) (equal? p station)) all-pts))
         (angle-table (make-table test: =))) ; Key 是角度 (float)

    ;; 1. 将所有点按角度分组
    (for-each (lambda (p)
                (let ((ang (get-angle station p)))
                  (table-set! angle-table ang (cons p (table-ref angle-table ang '())))))
              others)

    ;; 2. 角度内的点按距离排序 (由近到远)
    (for-each (lambda (ang)
                (let ((pts (table-ref angle-table ang)))
                  (table-set! angle-table ang
                              (list-sort (lambda (p1 p2) (< (dist-sq station p1) (dist-sq station p2))) pts))))
              (table-keys angle-table))

    ;; 3. 循环扫射
    (let* ((sorted-angles (list-sort < (table-keys angle-table))))
      (let loop ((count 0)
                 (angles sorted-angles)
                 (last-vaporized '()))
        (cond
         ((= count target-n) last-vaporized)
         ((null? angles) (loop count sorted-angles last-vaporized)) ; 转完一圈，开始下一圈
         (else
          (let* ((ang (car angles))
                 (targets (table-ref angle-table ang)))
            (if (null? targets)
                (loop count (cdr angles) last-vaporized) ; 这个方向打光了，看下一个角度
                (let ((target (car targets)))
                  ;; 气化掉最近的一个
                  (table-set! angle-table ang (cdr targets))
                  (loop (+ count 1) (cdr angles) target))))))))))

;; 运行结果
(let* ((p200 (run-part2 "day10.input" 200)))
  (displayln  p200)
  (displayln (+ (* (car p200) 100) (cdr p200))))
