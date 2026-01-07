(load "input-parse.scm")

(define (skip-whitespace port)
  (let lp ([c (peek-char port)])
    (cond
     [(eof-object? c) c]
     [(char-whitespace? c) (read-char port) (lp (peek-char port))]
     [else c])))

(define (next-tok port)
  (skip-whitespace port)
  (let ([c (peek-char port)])
    (cond
     [(eof-object? c) c]
     [(char=? c #\() (read-char port) 'left-paren]
     [(char=? c #\)) (read-char port) 'right-paren]
     [else ;; string
      (let lp ([acc '()])
        (let ([c (peek-char port)])
          (if (or (eof-object? c)
                  (char-whitespace? c)
                  (memv c '(#\( #\))))
              (list->string (reverse acc))
              (begin (read-char port)
                     (lp (cons c acc))))))])))

(define (parse-s-expression port fdown fup fvalue seed)
  (letrec ([parse (lambda (tok seed)
                    (cond
                     [(eof-object? tok) seed]
                     [(string? tok) (fvalue tok seed)]
                     [(eq? tok 'left-paren)
                      (let ([new-seed (fdown seed)]
                            [next-tk (next-tok port)])
                        (if (eq? 'right-paren next-tk)
                            seed
                            (let loop ([child-seed (parse next-tk new-seed)])
                              (let ([next-tk (next-tok port)])
                                (cond
                                 [(eq? 'right-paren next-tk) (fup seed child-seed)]
                                 [(eof-object? next-tk) (fup seed child-seed)]
                                 [else (loop (parse next-tk child-seed))])))))]
                     [else (error 'unkown-token tok)]))])
    (parse (next-tok port) seed)))

(define p (open-input-string "(1 2 3 (4 (5 6 (7)"))

(parse-s-expression p
                    (lambda (seed) '())
                    (lambda (parent-seed child-seed)
                      (if (null? parent-seed)
                          (reverse child-seed)
                          (cons (reverse child-seed) parent-seed)))
                    (lambda (tok seed)
                      (cons tok seed))
                    '())


(define p (open-input-string "((1) 2 3 ((5)) (4))"))
;; (define p (open-input-string "(1)"))

;; ssax 核心
(define (make-parser fdown fup fvalue)
  ;; 此时开头的 ( 必须已经被消耗掉了
  (define (run-pair parent-seed port)
    ;; 这个 lp 始终是在构建当前层的元素，当它发现达到当前层的结束标志之后，就向上返回，由 fup 结合当前层和上层的 seed,返回给更上层
    (let lp ([current-level-seed (fdown parent-seed)])
        (let ([tok (next-tok port)])
          (cond
           [(string? tok) (lp (fvalue tok current-level-seed))]
           [(eq? 'right-paren tok) (fup current-level-seed parent-seed)]
           [(eq? 'left-paren tok) (lp (run-pair current-level-seed port))]
           [else (error 'unkown-token tok)]))))
  (lambda (p init-seed)
    (let ([tok (next-tok p)])
      (if (eq? 'left-paren tok)
          (run-pair init-seed p)
          (error 'must-start-by-left-pair)))))

(define s-parser (make-parser
                  (lambda (seed) '())
                  (lambda (child-seed parent-seed)
                    (if (eq? 'empty-seed parent-seed)
                        (reverse child-seed)
                        (cons (reverse child-seed) parent-seed)))
                  (lambda (tok seed)
                      (cons tok seed))))

(s-parser p 'empty-seed)
