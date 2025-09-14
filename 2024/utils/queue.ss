;; a queue 实现 from https://www.scheme.com/tspl4/start.html#./start:h9
;; tconc structure
(define make-queue
  (lambda ()
    (let ([end (cons 'ignored '())])
      (cons end end))))

;; 可以画一下图，很好理解
(define putq!
  (lambda (q v)
    (let ([end (cons 'ignored '())])
      (set-car! (cdr q) v)
      (set-cdr! (cdr q) end)
      (set-cdr! q end))))

;; 查看队列首的元素
(define getq
  (lambda (q)
    (car (car q))))

;; 删除队首的元素
(define delq!
  (lambda (q)
    (set-car! q (cdr (car q)))))
