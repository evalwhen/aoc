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
