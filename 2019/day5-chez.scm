(include "input-parse.scm")
;; 1. 开启 Inspector 信息生成
(generate-inspector-information #t)

(eval-when (compile load eval)
  (optimize-level 0)           ;; 关闭所有优化（包括尾调用优化带来的栈帧丢失）
  (generate-inspector-information #t) ;; 强制生成变量名和源码行号映射
  (compile-profile 'source))   ;; 让报错时能指向源代码

;; 2. (可选) 关闭部分优化，防止代码被过度内联导致栈帧合并
(optimize-level 0) ;; 默认为 2，调试时若还看不清，可设为 0
(debug-on-exception #t)
;; 数据结构
;; 1. 整个代码放在一个 fxvector 中, ip 即其 index
;; 2. parse-optype 返回两个值，第一个为操作码，第二个为 fxvector, 按顺序对应每个参数的 mode
;;
;;

(define parse-code
  (lambda (infile)
    (call-with-input-file infile
      (lambda (p)
        (list->fxvector (parse-code-helper p '()))))))

(define parse-code-helper
  (lambda (p acc)
    (let* ([token (next-token '() '(#\, *eof* #\newline) "parse code helper" p)])
      (cond
       [(equal? token "") (reverse acc)]
       [else (begin (read-char p)
                    (parse-code-helper p
                                       (cons (string->number token) acc)))]))))

;; 每个指令参数个数不同
;; TODO: 看看如何定义常量枚举
(define get-parameter-count
  (lambda (c)
    (case c
      [(1 2) 3]
      [(3 4) 1]
      [(99) 0])))

;; 或者指令类别以及参数 mode
;; return opcode paramaters modes
;; fxint fxvector
(define parse-optype
  (lambda (opcode)
    (let* ([c (fxmod opcode 100)]
           [pc (get-parameter-count c)]
           [ps (make-fxvector pc)]
           [modes (fx/ opcode 100)]
           )
      (let lp ([i 0] [modes modes])
        (if (fx= i pc)
            (values c ps)
            (begin
              (fxvector-set! ps i (fxmod modes 10))
              (lp (inc i) (fx/ modes 10))))))))

(define run
  (lambda (code input)
    (let loop ([i 0])
      (cond
       [(fx>= i (fxvector-length code)) code]
       [else
        (let-values ([(opcode modes) (parse-optype (fxvector-ref code i))])
          (cond
           [(equal? opcode 99) code]
           [else
            (loop (exe-instruction i input code opcode modes))]
           ))]))))

;; i 对应 ip
;; j 对于当前指令的参数位置，从 0 开始
(define get-op-arg
  (lambda (i j code modes)
    (if (= 0 (fxvector-ref modes j))
        (fxvector-ref code (fxvector-ref code (+ j i 1)))
        (fxvector-ref code (+ j i 1)))))

;; 执行 sum / product 指令，返回下一个 ip
;; TODO: 优化逻辑，这里的 + 3 那里很不优雅，看看如何处理
(define exe-sum-product
  (lambda (i code op modes)
    (let ([opf (case op
                 [(1) fx+]
                 [(2) fx*]
                 [else (error 'exe-sum-product "unkwon op" op)])])
      (begin
        (fxvector-set! code
                       (fxvector-ref code (+ 3 i))
                       (opf (get-op-arg i 0 code modes)
                           (get-op-arg i 1 code modes)))
        (fx+ i (fxvector-length modes) 1)))))

(define exe-instruction
  (lambda (i input code op modes)
    (case op
      [(1 2) (exe-sum-product i code op modes)]
      [(3) (begin
             (fxvector-set! code
                            (fxvector-ref code (+ 1 i))
                            input)
             (+ i 2))]
      [(4) (begin (display (get-op-arg i 0 code modes))
                  (+ i 2))]
      )))

;; (trace exe-instruction exe-sum-product get-op-arg)
(run (parse-code "day5.input") 2)
