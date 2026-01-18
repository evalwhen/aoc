;; ==========================================
;; Gambit Scheme 设置
;; ==========================================
(declare (standard-bindings) (extended-bindings))

;; ==========================================
;; 1. 定义数据结构 (使用 Gambit Native Structure)
;; ==========================================

;; 定义虚拟机结构体
;; 字段:
;;  ip: 指令指针 (int)
;;  mem: 内存 (vector)
;;  inputs: 输入缓冲区 (list)
;;  status: 当前状态 ('running 'output 'halted 'need-input)
;;  last-out: 最后一次输出的值 (int/false)
(define-structure vm ip mem inputs status last-out)

;; 辅助：函数式更新 VM 状态
;; 使用 Gambit 的关键字参数 #!key
(define (update-vm vm
                   #!key
                   (ip 'no-change)
                   (mem 'no-change)
                   (inputs 'no-change)
                   (status 'no-change)
                   (last-out 'no-change))
  (make-vm
   (if (eq? ip 'no-change)      (vm-ip vm)      ip)
   (if (eq? mem 'no-change)     (vm-mem vm)     mem)
   (if (eq? inputs 'no-change)  (vm-inputs vm)  inputs)
   (if (eq? status 'no-change)  (vm-status vm)  status)
   (if (eq? last-out 'no-change)(vm-last-out vm) last-out)))

;; 辅助：向 VM 注入输入信号，并唤醒它
(define (vm-add-input vm val)
  (update-vm vm
             inputs: (append (vm-inputs vm) (list val))
             status: 'running)) ;; 唤醒机器

;; 辅助：Vector 复制
(define (vector-copy v)
  (let* ((len (vector-length v)) (new (make-vector len)))
    (let lp ((i 0))
      (if (= i len) new
          (begin (vector-set! new i (vector-ref v i)) (lp (+ i 1)))))))

;; ==========================================
;; 2. 核心虚拟机逻辑
;; ==========================================

;; 参数模式解析
(define (get-arg mem ip idx modes)
  (let ((val (vector-ref mem (+ ip idx 1)))
        (mode (modulo (quotient modes (expt 10 idx)) 10)))
    (if (= mode 0)
        (vector-ref mem val) ;; Position mode
        val)))               ;; Immediate mode

;; 单步执行 (Step)
(define (step-vm vm)
  (let* ((ip (vm-ip vm))
         (mem (vm-mem vm))
         (instruction (vector-ref mem ip))
         (opcode (modulo instruction 100))
         (modes (quotient instruction 100)))

    (case opcode
      ;; Add
      ((1) (let ((v1 (get-arg mem ip 0 modes))
                 (v2 (get-arg mem ip 1 modes))
                 (dest (vector-ref mem (+ ip 3))))
             (vector-set! mem dest (+ v1 v2))
             (update-vm vm ip: (+ ip 4))))

      ;; Mul
      ((2) (let ((v1 (get-arg mem ip 0 modes))
                 (v2 (get-arg mem ip 1 modes))
                 (dest (vector-ref mem (+ ip 3))))
             (vector-set! mem dest (* v1 v2))
             (update-vm vm ip: (+ ip 4))))

      ;; Input (关键)
      ((3) (let ((inputs (vm-inputs vm)))
             (if (null? inputs)
                 ;; 没数据 -> 状态变为 need-input，IP 不动 (等待下次唤醒)
                 (update-vm vm status: 'need-input)
                 ;; 有数据 -> 消耗输入，继续
                 (let ((val (car inputs))
                       (dest (vector-ref mem (+ ip 1))))
                   (vector-set! mem dest val)
                   (update-vm vm
                              ip: (+ ip 2)
                              inputs: (cdr inputs))))))

      ;; Output (关键)
      ((4) (let ((val (get-arg mem ip 0 modes)))
             ;; 产生输出 -> 状态变为 output，暂停，让出 CPU
             (update-vm vm
                        ip: (+ ip 2)
                        status: 'output
                        last-out: val)))

      ;; Jumps
      ((5) (let ((v1 (get-arg mem ip 0 modes))
                 (v2 (get-arg mem ip 1 modes)))
             (update-vm vm ip: (if (not (= v1 0)) v2 (+ ip 3)))))

      ((6) (let ((v1 (get-arg mem ip 0 modes))
                 (v2 (get-arg mem ip 1 modes)))
             (update-vm vm ip: (if (= v1 0) v2 (+ ip 3)))))

      ;; Compare
      ((7) (let ((v1 (get-arg mem ip 0 modes))
                 (v2 (get-arg mem ip 1 modes))
                 (dest (vector-ref mem (+ ip 3))))
             (vector-set! mem dest (if (< v1 v2) 1 0))
             (update-vm vm ip: (+ ip 4))))

      ((8) (let ((v1 (get-arg mem ip 0 modes))
                 (v2 (get-arg mem ip 1 modes))
                 (dest (vector-ref mem (+ ip 3))))
             (vector-set! mem dest (if (= v1 v2) 1 0))
             (update-vm vm ip: (+ ip 4))))

      ;; Halt
      ((99) (update-vm vm status: 'halted))

      (else (error "Unknown Opcode" opcode)))))

;; 连续运行直到遇到事件 (Run Until Event)
(define (run-vm-until-event vm)
  (let loop ((current-vm vm))
    (if (not (eq? (vm-status current-vm) 'running))
        current-vm ;; 返回最新的 VM 快照
        (loop (step-vm current-vm)))))

;; ==========================================
;; 3. 调度层 (Orchestrator)
;; ==========================================

(define (replace-nth lst n val)
  (if (= n 0) (cons val (cdr lst))
      (cons (car lst) (replace-nth (cdr lst) (- n 1) val))))

(define (run-feedback-loop raw-code phases)
  ;; 初始化 5 个 VM
  (define init-vms
    (map (lambda (phase)
           ;; 初始 phase 放入输入队列
           (make-vm 0 (vector-copy raw-code) (list phase) 'running #f))
         phases))

  ;; 循环调度
  ;; idx: 当前是哪个放大器 (0-4)
  ;; signal: 当前传递的信号
  (let loop ((vms init-vms)
             (idx 0)
             (signal 0))

    (let* ((curr-vm (list-ref vms idx))
           ;; 1. 注入信号 (Wake up)
           (ready-vm (vm-add-input curr-vm signal))
           ;; 2. 运行直到 Yield
           (next-vm (run-vm-until-event ready-vm))
           (status (vm-status next-vm)))

      (case status
        ((output)
         (let ((out-val (vm-last-out next-vm)))
           ;; A 产出 -> 给 B
           (loop (replace-nth vms idx next-vm)
                 (modulo (+ idx 1) 5)
                 out-val)))

        ((halted)
         (if (= idx 4)
             signal ;; E 停机，返回最后的信号
             ;; 某中间机器停机了（理论上不应发生，但为了防死锁，传旧信号给下家）
             (loop (replace-nth vms idx next-vm)
                   (modulo (+ idx 1) 5)
                   signal)))

        ((need-input)
         (error "Deadlock detected: Amplifier needs input but none provided."))))))

;; ==========================================
;; 4. 工具函数 (排列与文件加载)
;; ==========================================

(define (permutations s)
  (cond ((null? s) '(()))
        ((null? (cdr s)) (list s))
        (else (let loop ((lst s) (res '()))
                (if (null? lst) res
                    (let ((x (car lst)))
                      (loop (cdr lst)
                            (append res
                                    (map (lambda (p) (cons x p))
                                         (permutations (remove-item x s)))))))))))

(define (remove-item x lst)
  (if (equal? x (car lst)) (cdr lst) (cons (car lst) (remove-item x (cdr lst)))))

(define (load-code-from-file filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((acc '()) (curr ""))
        (let ((c (read-char port)))
          (cond ((eof-object? c)
                 (list->vector (reverse (cons (string->number curr) acc))))
                ((char=? c #\,)
                 (loop (cons (string->number curr) acc) ""))
                ((or (char-numeric? c) (char=? c #\-))
                 (loop acc (string-append curr (string c))))
                (else (loop acc curr)))))))) ;; 忽略换行

;; ==========================================
;; 5. 主程序
;; ==========================================

(define (solve-day7-part2 filename)
  (if (not (file-exists? filename))
      (print "Error: Input file not found.\n")
      (let ((raw-code (load-code-from-file filename))
            (max-signal 0))

        (display "Calculating...") (newline)

        (let loop ((perms (permutations '(5 6 7 8 9))))
          (if (null? perms)
              (begin
                (display "----------------------") (newline)
                (display "Max Thruster Signal: ")
                (display max-signal) (newline))
              (let* ((phases (car perms))
                     (signal (run-feedback-loop raw-code phases)))
                (if (> signal max-signal)
                    (set! max-signal signal))
                (loop (cdr perms))))))))

;; 运行 (请确保当前目录下有 day7.input)
(solve-day7-part2 "day7.input")
