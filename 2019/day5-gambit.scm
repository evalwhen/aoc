;; ==========================================
;; Gambit Scheme Intcode VM (Day 5)
;; ==========================================

;; 开启 Gambit 的标准绑定和扩展绑定优化
(declare (standard-bindings) (extended-bindings))

;; 辅助函数：Gambit 默认没有 inc
(define (inc x) (fx+ x 1))

;; ==========================================
;; 核心逻辑
;; ==========================================

(define (next-token acc delimiters msg p)
  (let loop ((res '()))
    (let ((c (peek-char p)))
      (cond
       ((eof-object? c) (list->string (reverse res)))
       ((memv c delimiters) (list->string (reverse res)))
       (else
        (read-char p)
        (loop (cons c res)))))))

(define parse-code
  (lambda (infile)
    (call-with-input-file infile
      (lambda (p)
        (list->vector (parse-code-helper p '())))))) ;; list->fxvector -> list->vector

(define parse-code-helper
  (lambda (p acc)
    ;; 直接使用 #!eof
    (let* ((token (next-token '() `(#\, ,#!eof #\newline) "parse code helper" p)))
      (cond
       ((equal? token "") (reverse acc))
       (else (begin
               (read-char p)
               (parse-code-helper p
                                  (cons (string->number token) acc))))))))

(define get-parameter-count
  (lambda (c)
    (case c
      ((1 2) 3)
      ((3 4) 1)
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

(define exe-instruction
  (lambda (i input code op modes)
    (case op
      ((1 2) (exe-sum-product i code op modes))
      ((3) (begin
             (vector-set! code
                            (vector-ref code (+ 1 i))
                            input)
             (+ i 2)))
      ((4) (begin
             (display (get-op-arg i 0 code modes))
             (newline)
             (+ i 2)))
      (else (error "Unknown opcode in exe-instruction" op)))))

(define run
  (lambda (code input)
    (let loop ((i 0))
      (cond
       ;; fxvector-length -> vector-length
       ((fx>= i (vector-length code)) code)
       (else
        (call-with-values
            (lambda () (parse-optype (vector-ref code i)))
          (lambda (opcode modes)
            (cond
             ((equal? opcode 99) code)
             (else
              (loop (exe-instruction i input code opcode modes)))))))))))

;; ==========================================
;; 运行
;; ==========================================

(if (file-exists? "day5.input")
    (begin
      (display "Starting run...") (newline)
      (run (parse-code "day5.input") 1))
    (display "Please provide day5.input file."))
