(declare (standard-bindings) (extended-bindings))

;; 以下 TODO 必须要写完
;;TODO 优化实现，找到最优的写法，按理说应该在一开始就知道写法了,
;;TODO 引入 myenv-gambit.scm, 让 inc 等生效
;;找到满意的之后和 AI 探索
(import (srfi 69))
;; 用了 next-token, inc 等辅助函数
;; (include "myenv-gambit.scm")


(define displayln
  (lambda args
    (apply display args)
    (display "\n")))

;; 方案：
;; 从输入构建一个 hashtable, 包含所有数据，values 之间围绕 key

(define parse-line
  (lambda (line)
    (let ((idx (string-contains line ")")))
      (if idx
          (values (substring line 0 idx)
                  (substring line (+ 1 idx) (string-length line)))
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
            (- (length (car (filter (lambda (path)
                                      (or (null? path)
                                          (and (eq? start (car path))
                                               (eq? end (last path)))
                                          ))
                                    (find-paths start))))
               1))))))

(define data2 (cdr (parse-paths "day6.input")))
;; (displayln (hash-table-ref data2 'K))
;; (displayln (hash-table-ref data 'K))

(display  (run-part2 data data2))

;; (displayln (filter (lambda (path) (memq 'YOU path))
                   ;; (build-paths data 'COM)))

;; (displayln (filter (lambda (path) (memq 'SAN path))
                   ;; (build-paths data 'COM)))

(define x1 '(FRK 6PM BNT 3Q8 TNM JVS LL3 CRZ X4L XB6 F56 VKQ T6Y R3T RRR 28P K4Z SYB B6S C46 RBX KZJ 1MJ 692 LF8 8JX LWX ZRF KGW QTF 13L 61J 1P3 D8V 5L1 JJ6 KLD F7Z CZ9 3D2 FWP 4HJ 4C3 8K2 LST X22 JBW W9M PHZ 38M PCB SC1 ZP4 BCX 6M8 2VZ 8YW D33 4L2 CDG YYB XP3 SYD 72J M19 TH9 7FY 3BF 6DT LN3 JHZ YJB ZX2 V6L SYJ BQQ 4GC KB5 SX1 583 GZJ MC9 4PC 4ZQ PJG 2RS C6L M63 XW3 N1Y 4J6 N5S GWD S7X 1LW WZX WKR PHX VM6 FQD JLZ 1TX C8B DX4 BPB G46 JX6 Q3F ZST GL1 V26 QRB CZQ VML BHG TBG 479 CFR NCP 43X HMM TM4 G8V ZXS ZSH MRK 9V8 XBB VFV S2N J1C 3VP Z3Q G7H LXZ X8T BD8 GMH NPR Y8J 8NY 8RM C3J TCC BLQ J5Q T6B D8K JN4 611 5DS JS6 9F9 9HS 772 BRK NWM
DFD 92T HVS 41H Z5M YM9 BBT 7QW LGX D9T BPZ Y42 N28 Q3P KVC Q5Y Q8B HYP YLC WC8 KTZ 48G GVK RV9 MJJ VC1 KS5 DN7 YS4 5LQ XBX XP9 FBW HWJ V9X 7MZ PDW 16N NFK 5C8 KZ2 9X2 TSF 6F7))

(define x2 '(FRK D7F 43S 9CQ KR4 T5R GP9 RZB C1F M34 RN9 W9V 5LH TY6 GY3 FQT MN9 RTK HPL 2HW 9FZ P9T 966 HXQ 27Z YVH 6JN P67 BC5 GY6 GW9 Y4L 7C2 HD5 VTS PF1 J3T 7H1 F7Y HQS 4TW 4HC YQM CVF G8Q 358 KMC 7SC NZS 1BD GJP VDS X1X 6K1 5NX PL2 71Y 958 F2X H5S WZH 246 SFG 4RN D5Q NR3 CVK HTR QRC RYV HTQ F9B L4V YPD 6JS M79 73S 9Q7 VJ3 RGP C55 HGL 97B Y3Q GZ8 P7C KCB LWT J6L 4LK B2H L3G KKP B7H ZS4 DZM 2B3 P3D J31 KK9 V63 FC7 B3Z 7WL SSQ HC6 6FG T8C CKH NQK NLJ KWV 8FW 84M HMX 3SB 729 C4G Z5C 3B7 GQJ GY7 6R6 V11 L4K JSF 97X XRM J1V 26X YNC 26J SM5 S81 9K1 P4G V6D CNL HK8 BZY YZC HV4 J3F W1D 72Q FHY 7PL CQT J48 JH9 458 F6N KP1 CFW R5K JKZ Q6V
Q1X 4L9 ZRN VVD KBY 9W8 7YM P5R DLL RGV HFK YVP 3X3 VKL ZLY X5H NYS DXM CC6 J37 6LZ 79K YJT 7K6 P55 GX2 DC2 R89 2S1 B7M LVQ B1J 1SG 2VR WFF))

