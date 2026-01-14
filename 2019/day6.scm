(declare (standard-bindings) (extended-bindings))

;; 以下 TODO 必须要写完
;;TODO 优化实现，找到最优的写法，按理说应该在一开始就知道写法了,
;;TODO 引入 myenv-gambit.scm, 让 inc 等生效
;;找到满意的之后和 AI 探索
(import (srfi 69))
;; 用了 next-token, inc 等辅助函数
(include "./myenv-gambit.scm")


;; 方案：
;; 从输入构建一个 hashtable, 包含所有数据，values 之间围绕 key

(define parse-line
  (lambda (line)
    (let ((idx (string-contains line ")")))
      (if idx
          (values (substring line 0 idx)
                  (substring line (inc idx) (string-length line)))
          (error 'pare-line "invalid line" line)))))

(define parse-paths
  (lambda (infile)
    (call-with-input-file infile
      (let ((data1 (make-hash-table eq?))
            (data2 (make-hash-table eq?)))
        (lambda (p)
          (let lp ((line (read-line p)))
            (if (eof-object? line)
                (cons data1 data2)
                (let*-values (((k v) (parse-line line))
                              ((k-sym) (string->symbol k))
                              ((v-sym) (string->symbol v)))
                  (begin
                    (hash-table-update!/default data1 k-sym (lambda (vs) (cons v-sym vs)) '())
                    (hash-table-set! data2 v-sym k-sym)
                    (lp (read-line p)))))))))))

;; 构建从 COM 出发的所有 path, path 之间可能重合
(define build-paths
  (lambda (data init)
    (letrec ((recur (lambda (init)
                      (let ((vs (hash-table-ref/default data init '())))
                        (if (null? vs)
                            (list (list init))
                            (map (lambda (path) (cons init path))
                                 ;; 多个 vs 之间的路径是平级，因此 append 起来
                                 (apply append (map recur vs))))))))
      (recur init))))


(define data (car (parse-paths "day6.input")))

(define run
  (lambda (paths)
    (let ((visited (make-table 'test: eq?)))
      (letrec ((recur (lambda (path i t)
                        (if (null? path)
                            t
                            (recur (cdr path)
                                   (+ 1 i)
                                   (if (table-ref visited (car path) #f)
                                       t
                                       (begin
                                         (table-set! visited (car path) #t)
                                         (+ i t)))))))
               (lp (lambda (paths t)
                     (if (null? paths)
                         t
                         (lp (cdr paths)
                             (recur (car paths) 0 t))))))
        (lp paths 0)))))

;; basic test
;; (display (run '((COM B  C D I SAN) (COM B  C D E J K YOU))))

;; part1
;; (display (run (build-paths data 'COM)))

;; part2
;;

;; data1 values 围绕 key
;; data2 key 围绕 value
;; 方案1: 如下 find-paths 递归寻找从 start 开始的所以路径
;; 方案2: 从 build-paths 的结果里面，分别找出包含 start 和 end 的 path, 去掉公共字串，它们的长度 + 起来即可,
;; 为什么这可行呢？因为从题目的规定，每个轨道只能直接环绕一个轨道，这意味着 每条从 COM 到 叶子节点的路径都是唯一存在的
(define run-part2
  (lambda (data1 data2)
    (let ((start (hash-table-ref/default data2 'YOU 'NO))
          (end (hash-table-ref/default data2 'SAN 'NO))
          (visited (make-hash-table eq?)))
      (if (or (eq? start 'NO)
              (eq? end 'NO))
          (error 'run-part2 "start or end not found")
          ;; 找出从 init 出发的可能 path, 结尾不一定是 end
          ;; 之后过滤以下，只拿到
          (letrec ((find-paths (lambda (init)
                                 (cond
                                  ((eq? init end) (list (list end)))
                                  (else
                                   (hash-table-set! visited init #t)
                                   (let ((vs (get-vs init)))
                                     (if (null? vs)
                                         '(())
                                         (map (lambda (path)
                                                (cons init path))
                                              (apply append (map find-paths vs)))))))))
                   (get-vs (lambda (star)
                             (filter (lambda (star) (not (hash-table-ref/default visited star #f)))
                                     (if (hash-table-exists? data2 star)
                                         (cons (hash-table-ref data2 star)
                                               (hash-table-ref/default data1 star '()))
                                         (hash-table-ref/default data1 star '()))))))
            (dec (length (car (filter (lambda (path)
                                        (and (eq? start (car path))
                                             (eq? end (last path))))
                                      (find-paths start))))))))))

(define data2 (cdr (parse-paths "day6.input")))

(displayln  (run-part2 data data2))


(define run-part2-v2
  (lambda (infile)
    (let* ((data (car (parse-paths infile)))
           (paths (build-paths data 'COM))
           (target-paths (filter (lambda (path)
                                   (or (memq 'YOU path)
                                       (memq 'SAN path)))
                                 paths)))
      (when (= 2 (length target-paths))
        (let lp ((path1 (car target-paths))
                 (path2 (cadr target-paths)))
          (cond
           ((null? path1) '())
           ((null? path2) '())
           ((eq? (car path1) (car path2)) (lp (cdr path1) (cdr path2)))
           (else (- (length (append path1 path2))
                    2))))))))

(displayln (run-part2-v2 "day6.input"))
