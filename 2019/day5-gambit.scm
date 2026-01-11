;; ==========================================
;; Gambit Scheme 兼容层
;; ==========================================

(declare (standard-bindings) (extended-bindings))

(define *eof* #!eof)

(define fx+ +)
(define fx* *)
(define fx= =)
(define fx>= >=)
(define fxmod modulo)
(define fx/ quotient)
(define (inc x) (+ x 1))

(define fxvector-ref vector-ref)
(define fxvector-set! vector-set!)
(define fxvector-length vector-length)
(define list->fxvector list->vector)
(define make-fxvector make-vector)
(define (fxvector . args) (apply vector args))

(define (next-token acc delimiters msg p)
  (let loop ((res '()))
    (let ((c (peek-char p)))
      (cond
       ((eof-object? c) (list->string (reverse res)))
       ((memv c delimiters) (list->string (reverse res)))
       (else
        (read-char p)
        (loop (cons c res)))))))

;; ==========================================
;; 核心逻辑 (已将所有 [] 替换为 ())
;; ==========================================

(define parse-code
  (lambda (infile)
    (call-with-input-file infile
      (lambda (p)
        (list->fxvector (parse-code-helper p '()))))))

(define parse-code-helper
  (lambda (p acc)
    (let* ((token (next-token '() `(#\, ,*eof* #\newline) "parse code helper" p)))
      (cond
       ((equal? token "") (reverse acc))
       (else (begin
               (read-char p)
               (parse-code-helper p
                                  (cons (string->number token) acc))))))))

(define get-parameter-count
  (lambda (c)
    (case c
      ((1 2) 3)    ;; <--- 注意这里改成了 (( ))
      ((3 4) 1)
      ((99) 0)
      (else 0))))

(define parse-optype
  (lambda (opcode)
    (let* ((c (fxmod opcode 100))
           (pc (get-parameter-count c))
           (ps (make-fxvector pc))
           (modes (fx/ opcode 100)))
      (let lp ((i 0) (modes modes))
        (if (fx= i pc)
            (values c ps)
            (begin
              (fxvector-set! ps i (fxmod modes 10))
              (lp (inc i) (fx/ modes 10))))))))

(define get-op-arg
  (lambda (i j code modes)
    (if (= 0 (fxvector-ref modes j))
        (fxvector-ref code (fxvector-ref code (+ j i 1)))
        (fxvector-ref code (+ j i 1)))))

(define exe-sum-product
  (lambda (i code op modes)
    (let ((opf (case op
                 ((1) fx+)  ;; <--- 这里也改成了 (( ))
                 ((2) fx*)
                 (else (error "exe-sum-product unknown op" op)))))
      (begin
        (fxvector-set! code
                       (fxvector-ref code (+ 3 i))
                       (opf (get-op-arg i 0 code modes)
                            (get-op-arg i 1 code modes)))
        (fx+ i (fxvector-length modes) 1)))))

(define exe-instruction
  (lambda (i input code op modes)
    (case op
      ((1 2) (exe-sum-product i code op modes)) ;; <--- 这里也改成了 (( ))
      ((3) (begin
             (fxvector-set! code
                            (fxvector-ref code (+ 1 i))
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
       ((fx>= i (fxvector-length code)) code)
       (else
        (call-with-values
            (lambda () (parse-optype (fxvector-ref code i)))
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
      (display (run (parse-code "day5.input") 1)))
    (display "Please provide day5.input file."))
