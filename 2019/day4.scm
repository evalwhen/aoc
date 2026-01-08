(load "./myenv-chez.scm")

;; part1 检查是否存在相邻元素相等的情况
(define has-same-adjacent
  (lambda (str)
    (let lp ([i 0] [j 1])
        (cond
         [(= i 5) #f]
         [(char=? (string-ref str i)
                  (string-ref str j)) #t]
         [else (lp (inc i) (inc j))]))))

;; l 指当前发现的 group 长度
;; part2 只找出是否存在长度仅为 2 的字串
(define has-same-pair-adjacent
  (lambda (str)
    (let lp ([i 0] [j 1] [l 0])
        (cond
         [(= j 6) (if (= l 2) #t #f)] ;; j 有可能等于 6 的 case 是最后两个元素相等
         [(char=? (string-ref str i)
                  (string-ref str j))
          (lp i (inc j) (inc (- j i)))]
         [(= l 2) #t]
         [else (lp j (inc j) 0)]))))

(define is-increase
  (lambda (str)
    (apply char<=? (string->list str))))

(define run
  (lambda (has-needed-adjacent)
    (let lp ([n 236491] [acc 0])
      (let ([m (number->string n)])
        (cond
         [(> n 713787) acc]
         [(and (has-needed-adjacent m)
               (is-increase m))
          (lp (inc n) (inc acc))]
         [else (lp (inc n) acc)])))))


(run has-same-adjacent)
(run has-same-pair-adjacent)
