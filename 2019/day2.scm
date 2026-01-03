;; code is a fxvector
;;
(load "input-parse.scm")

(define run
  (lambda (code)
    (let loop ([i 0])
      (cond
       [(fx>= i (fxvector-length code)) code]
       [(equal? (fxvector-ref code i) 1)
        (loop (exe-sum i code))]
       [(equal? (fxvector-ref code i) 2)
        (loop (exe-product i code))]
       [(equal? (fxvector-ref code i) 99)
        code]))))

(define exe-general
  (lambda (i code op)
    (begin
      (fxvector-set! code
                     (fxvector-ref code (+ 3 i))
                     (op (fxvector-ref code (fxvector-ref code (+ 2 i)))
                        (fxvector-ref code (fxvector-ref code (+ 1 i)))))
      (fx+ 4 i))))

(define exe-sum
  (lambda (i code)
    (exe-general i code fx+)))

(define exe-product
  (lambda (i code)
    (exe-general i code fx*)))

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

(define replace-1-2
  (lambda (code p1 p2)
    (assert (>= (fxvector-length code) 3))
    (fxvector-set! code 1 p1)
    (fxvector-set! code 2 p2)
    code))

(run (replace-1-2 (parse-code "day2.input") 12 2))

(define run-get-result
  (lambda (code p1 p2)
    (fxvector-ref (run (replace-1-2 code p1 p2)) 0)))

;; nested named let
(define run-part2-v1
  (lambda (code return)
    (let lp-outer ([i 0])
        (if (< i 100)
            (let lp-inner ([j 0])
              (if (< j 100)
                  (if (= (run-get-result (fxvector-copy code) i j) 19690720)
                      (return (cons i j))
                      (lp-inner (inc j)))
                  (lp-outer (inc i))))
            'done))))

;; single named let
(define run-part2-v2
  (lambda (code return)
    (let lp ([i 0] [j 0])
      (cond
       [(= i 100) #f]
       [(= j 100) (lp (inc i) 0)]
       [else
        (if (= (run-get-result (fxvector-copy code) i j) 19690720)
            (return (cons i j))
            (lp i (inc j)))]))))

;; do macro
(define run-part2-v3
  (lambda (code return)
    (do ([i 0 (inc i)])
        ((= i 100))
      (do ([j 0 (inc j)])
          ((= j 100))
        (if (= (run-get-result (fxvector-copy code) i j) 19690720)
            (return (cons i j)))))))

;; TODO: 这个写完写一个 generator, 产生必要的 pair
;;
(call/cc (lambda (return)
           (run-part2-v3 (parse-code "day2.input") return)))
