;; 以下实现了 aoc 2019 的 intcode computer
;; 核心函数见 run-vm， 该函数设计遵循函数式设计，每次返回新的计算机状态，返回类型，以及返回值
;; 具体见该函数实现

(declare (standard-bindings) (extended-bindings))

;; 用了 next-token, inc 等辅助函数
(include "input-parse.scm")
;; ==========================================
;; 核心逻辑
;; ==========================================
;; 1. 整个代码放在一个 fxvector 中, ip 即其 index
;; 2. parse-optype 返回两个值，第一个为操作码，第二个为 vector, 按顺序对应每个参数的 mode, 这个 vector 的长度对应了参数的个数
;;
;;

(define load-code
  (lambda (infile)
    (call-with-input-file infile
      (lambda (p)
        (list->vector (parse-code-helper p '())))))) ;; list->fxvector -> list->vector

(define parse-code-helper
  (lambda (p acc)
    (let* ((token (next-token '() '(#\, *eof* #\newline) "parse code helper" p)))
      (cond
       ((equal? token "") (reverse acc))
       (else (begin (read-char p)
                    (parse-code-helper p
                                       (cons (string->number token) acc))))))))


(define get-parameter-count
  (lambda (c)
    (case c
      ((1 2 7 8) 3)
      ((3 4) 1)
      ((5 6) 2)
      ((99) 0)
      (else 0))))

(define parse-optype
  (lambda (opcode)
    ;; fxmod -> fxmodulo, fx/ -> fxquotient
    (let* ((c (fxmodulo opcode 100))
           (pc (get-parameter-count c))
           (ps (make-vector pc)) ;; make-fxvector -> make-vector
           (modes (fxquotient opcode 100)))
      (let lp ((i 0) (modes modes))
        (if (fx= i pc)
            (values c ps)
            (begin
              ;; fxvector-set! -> vector-set!
              (vector-set! ps i (fxmodulo modes 10))
              (lp (inc i) (fxquotient modes 10))))))))

;; 根据参数 modes 获取参数值
;; i 对于当前 ip 指针，它执行当前指令的开头
;; j 对于第几个参数，从 0 开始
;; TODO: 应该再加一个参数，用来控制是源，还是目的
(define get-op-arg
  (lambda (i j code modes)
    ;; fxvector-ref -> vector-ref
    (if (= 0 (vector-ref modes j))
        (vector-ref code (vector-ref code (+ j i 1)))
        (vector-ref code (+ j i 1)))))

(define exe-sum-product
  (lambda (i code op modes)
    (let ((opf (case op
                 ((1) fx+)
                 ((2) fx*)
                 (else (error "exe-sum-product unknown op" op)))))
      (begin
        ;; fxvector-set! -> vector-set!, fxvector-ref -> vector-ref
        (vector-set! code
                       (vector-ref code (+ 3 i))
                       (opf (get-op-arg i 0 code modes)
                            (get-op-arg i 1 code modes)))
        ;; fxvector-length -> vector-length
        (fx+ i (vector-length modes) 1)))))

;; vm 状态，code 内存，ip 指令指针， input 当前计算机的输入
;; 每次计算机返回时，返回新的状态以及返回类型，等待输入，output 指令，halt 指令
(define-structure vmstate code ip inputs)

(define append-input
  (lambda (state v)
    (vmstate-inputs-set! state (append (vmstate-inputs state) (list v)))))

;; 运行 vm, 返回 (values new-state return-type output-value)
;; return-type 包括 input, output, halt, wrong-memory
;; 分别代表，等待输入，输出内容，遇到停机指令，错误的内容
(define run-vm
  (lambda (state)
    (let ((code (vmstate-code state))
          (ip (vmstate-ip state))
          (inputs (vmstate-inputs state)))
      (let loop ((ip ip) (inputs inputs))
        ;; (displayln ip)
        ;; (displayln (vector-ref code ip))
        (cond
         ((fx>= ip (vector-length code)) (values (make-vmstate code ip inputs) 'wrong-memory 'void))
         (else
          (call-with-values
              (lambda () (parse-optype (vector-ref code ip)))
            (lambda (opcode modes)
              (case opcode
                ((99) (values (make-vmstate code ip inputs) 'halt 'void))
                ;; sum/product
                ((1 2) (loop (exe-sum-product ip code opcode modes) inputs))
                ;; input
                ((3) (if (null? inputs)
                         (values (make-vmstate code ip inputs) 'input 'void)
                         (begin
                           (vector-set! code
                                        (vector-ref code (+ 1 ip))
                                        (car inputs))
                           (loop (+ ip 1 (vector-length modes))
                                 (cdr inputs)))))
                ;; ouput
                ((4) (values (make-vmstate code
                                           (+ ip 1 (vector-length modes))
                                           inputs)
                             'output
                             (get-op-arg ip 0 code modes)))
                ;; jump-if-true
                ((5) (if (not (= 0 (get-op-arg ip 0 code modes)))
                         (loop (get-op-arg ip 1 code modes) inputs)
                         (loop (+ ip 1 (vector-length modes)) inputs)))
                ;; jump-if-false
                ((6) (if (= 0 (get-op-arg ip 0 code modes))
                         (loop (get-op-arg ip 1 code modes) inputs)
                         (loop  (+ ip 1 (vector-length modes)) inputs)))
                ;; less than
                ((7) (begin (if (< (get-op-arg ip 0 code modes)
                                   (get-op-arg ip 1 code modes))
                                (vector-set! code (vector-ref code (+ 3 ip)) 1)
                                (vector-set! code (vector-ref code (+ 3 ip)) 0))
                            (loop (+ ip 1 (vector-length modes)) inputs)))
                ;; equals
                ((8) (begin (if (= (get-op-arg ip 0 code modes)
                                   (get-op-arg ip 1 code modes))
                                (vector-set! code (vector-ref code (+ 3 ip)) 1)
                                (vector-set! code (vector-ref code (+ 3 ip)) 0))
                            (loop (+ ip 1 (vector-length modes)) inputs)))
                (else (error "Unknown opcode in exe-instruction" op)))))))))))

