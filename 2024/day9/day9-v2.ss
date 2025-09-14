;; utils
(define 1+ (lambda (v) (+ v 1)))
(define 1- (lambda (v) (- v 1)))
;; 由于整个输入是一行文件，一次 get-line 会返回整个字符串，且返回值不包含换行符
;; 另外一个函数是 get-string-all 字符串末尾会带上换行字符 #\newline
(define (read-input-string path)
  (call-with-input-file path
    (lambda (p)
      (get-line p))))

;; 0 -> 9 to #\0 -> #\9
(define digit->char (lambda (i) (integer->char (+ (char->integer #\0) i))))

(define char->digit (lambda (c) (- (char->integer c)
                                   (char->integer #\0))))

;; string->utf8 string to vu8
;;
(define test-simple "0..111....22222")
(define test-data "00...111...2...333.44.5555.6666.777.888899")

(define dot #\.)

;; 这里我们把 string 当成 vector 来操作
(define swap
  (lambda (s i j)
    (let ((tmp (string-ref s i)))
      (string-set! s i (string-ref s j))
      (string-set! s j tmp))))

(define dot?
  (lambda (s i)
    (char=? (string-ref s i) dot)))

;; abc: available blocks count
;; c： 移动次数 + 遇到的 dot 次数
;; i: 左边的 index
;; j: 右边的 index
(define move
  (lambda (abc)
    (letrec ((main-loop (lambda (s i j c)
                          (cond
                            ((= c abc) s)
                            ((dot? s j) (main-loop s i (1- j) (1+ c)))
                            (else (find-left-dot s i j c)))))
             (find-left-dot (lambda (s i j c)
                              (cond
                                ((>= i j) s)
                                ((dot? s i)
                                 (swap s i j)
                                 (main-loop s (1+ i) (1- j) (1+ c)))
                                (else (find-left-dot s (1+ i) j c))))))
      (lambda (s i j c) (main-loop s i j c)))))

;; abc: available blocks count
(define abc
  (lambda (s)
    (let ([c 0])
      (string-for-each (lambda (e) (when (char=? e dot) (set! c (1+ c)))) s)
      c)))

;; 来自 https://scheme.com/tspl4/objects.html#./objects:s223
;; 提前分配了内存，利用递归的结构，巧妙的实现了 string-append
;; 递的过程 (wind)，构建了最终的字符串长度 n，以及初始化好的 string
;; 归的过程 (unwind)，将对应的 string set 到上一步创建好的 string 上
(define string-append
  (lambda args
    (let f ([ls args] [n 0])
      (if (null? ls)
          (make-string n)
          (let* ([s1 (car ls)]
                 [m (string-length s1)]
                 [s2 (f (cdr ls) (+ n m))])
            (do ([i 0 (+ i 1)] [j n (+ j 1)])
                ((= i m) s2)
              (string-set! s2 j (string-ref s1 i))))))))

;; 把 string-append 的实现推广到了 expand-dense
;; dense-format to expanded format
;; 12345 转化成 0..111....22222, 为什么这样转，见 day9.txt
;; 优化: 合并了 expand-single, 并且在一次遍历中同时生成字符串和计算 dot 的数量。
(define expand-dense
  (lambda (s)
    (let f ([i 0] [n 0] [dot-count 0])
      (if (= i (string-length s))
          (values (make-string n) dot-count)
          (let* ([is-odd (odd? i)]
                 [c (if is-odd #\. (digit->char (/ i 2)))]
                 [len (char->digit (string-ref s i))]
                 [next-dot-count (if is-odd (+ dot-count len) dot-count)])
            (let-values ([(s2 final-dot-count) (f (1+ i) (+ n len) next-dot-count)])
              (string-fill! s2 c n (+ n len))
              (values s2 final-dot-count)))))))

;; 练习下用 do 实现
(define check-sum
  (lambda (s)
    (let ([res 0])
      (do ([i 0 (1+ i)])
          ((= i (string-length s)) res)
        (if (not (dot? s i))
            (set! res (+ res (* i (char->digit (string-ref s i))))))))))

(define solve1
  (lambda (s)
    (let-values ([(es dot-count) (expand-dense s)])
      (let* ((mabc (move dot-count))
             (moved (mabc es 0 (1- (string-length es)) 0)))
        (check-sum moved)))))

;; part2
;; move-2 将当前磁盘状态 s "00...111...2...333.44.5555.6666.777.888899"，从末尾开始按文件，
;; 比如 99 移动到最左边合适的位置，如果没有合适的位置，就什么也不做
;; 参数说明
;; i 开头的第一个 dot 位置
;; j 从字符串末尾开始迭代
;; c 当前积累的连续的文件 block id
(define move-2
  (lambda (s)
    (let m ([s s] [i 0] [j (1- (string-length s))] [c 0])
      (cond
       ((< j i) s)
       ((not (dot? s i)) (m s (1+ i) j c))
       ((dot? s j) (m s i (1- j) 0))
       ((and (> j 0) (char=? (string-ref s j) (string-ref s (1- j))))
        (m s i (1- j) (1+ c)))
       (else (m (move-f s i j (1+ c)) i (1- j) 0))))))


;; move-f 将文件移动到左边合适的空余位置，可以容纳下文件 f,该文件从 s 的序号 i 开始，长度为 c 个block
;; d 从磁盘开头扫描可以容纳下文件 f 的位置
(define move-f
  (lambda (s i j c)
    (let d ([di i] [dc 0])
      (cond
       ((= c dc) (move-to! s j (- di c) c))
       ((= di j) s)
       ((dot? s di) (d (1+ di) (1+ dc)))
       (else (d (1+ di) 0))))))

(define move-to!
  (lambda (s i j c)
    ;; (display (substring s i (+ i c)))
    ;; (display "\n")
    ;; (display (substring s j (+ j c)))
    ;; (display "\n")
    (do ([ii i (1+ ii)] [jj j (1+ jj)])
        ((= (- ii i) c) s)
      (swap s ii jj))))

(define solve2
  (lambda (path)
    (let* ((s (read-input-string path))
           )
      (let-values ([(es _) (expand-dense s)])
        (check-sum (move-2 es))))))

;;tests
(define move-6 (move 6))

(move-6 test-simple 0 (- (string-length test-simple) 1) 0)

;;test2 part2
(define test-s1 "00...111...2...333.44.5555.6666.777.888899")
(move-2 test-s1)

;;
(display (solve2 "input.txt"))
