;; 给予当前位置 current 往不同方向走
(define L
  (lambda (n current)
    (fxmodulo (fxabs (fx+ 100 (fx- current (fxmodulo n 100))))
            100)))

(define R
  (lambda (n current)
    (fxmodulo (fx+ current n) 100)))


;; 计算当前位置往左走距离 0 的长度
(define LD
  (lambda (current)
    current))

(define RD
  (lambda (current)
    (fx- 100 current)))


(define passzero
  (lambda (step current)
    (let ([df (if (eq? (car step) 'L) LD RD)])
      (let-values ([(d m) (fxdiv-and-mod (cdr step) 100)])
        (if (and  (fx>= m (df current))
                  (not (fx= current 0)))
            (fx+ 1 d)
            d)))))



;; step like (cons 'R 5)
(define pass-zero?
  (lambda (step current)
    (let ([df (if (eq? (car step) 'L) LD RD)])
      (fx>= (cdr step) (df current)))))

(define move
  (lambda (step current)
    (let ([mf (if (eq? (car step) 'L) L R)])
      (mf (cdr step) current))))

;; 假设 ls 的元素类型为 (cons 'L 5)
;; s 是 fold 的状态 (cons zero-count current)
(define run
  (lambda (ls current)
    (fold-left (lambda (s a) (let ([nc (move a (cdr s))]
                                   [zc (passzero a (cdr s))])
                               (display a)
                               (display "\n")
                               (display nc)
                               (display "\n")
                               ;; (display "\n")
                               ;; (display zc)
                               ;; (display "\n")
                               (cons (fx+ zc (car s)) nc)))
               (cons 0 current)
               ls)))


(define parse-line
  (lambda (line)
    (let* ([start (string-ref line 0)]
           [rl (if (char=? start #\R) 'R 'L)]
           [ss (string->number (substring line 1 (string-length line)))])
      (cons rl ss))))

(define parse-input
  (lambda (fname)
    (call-with-input-file fname
      (lambda (port)
        (let loop ([line (get-line port)] [acc '()])
          (if (eof-object? line)
              (reverse acc)
              (loop (get-line port) (cons (parse-line line) acc))))))))

(run (parse-input "input.txt") 50)
