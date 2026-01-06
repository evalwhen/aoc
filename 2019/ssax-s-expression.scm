(load "input-parse.scm")

(define (next-tok port)
  (if (eof-object? (peek-char port))
      (read-char port)
      (let lp ([c (peek-char port)] [acc '()])
        (cond
         ;; [(eof-object? c) (list->string (reverse acc))]
         [(char=? c #\() (begin (read-char port)
                                'left-paren)]
         [(char=? c #\)) (if (> (length acc) 0)
                             (list->string (reverse acc))
                             (begin (read-char port)
                                    'right-paren))]
         [(char=? c #\space) (if (> (length acc) 0)
                                 (begin (read-char port)
                                        (list->string (reverse acc)))
                                 (begin (read-char port)
                                        (lp (peek-char port) '())))]
         [else (begin
                 (read-char port)
                 (lp (peek-char port) (cons c acc)))]))))

;; parse-s-expression to scheme list
(define (parse-s-expression port tok fdown fup fchar seed)
  (cond
   [(eof-object? tok) (begin (displayln tok)  seed)]
   [(string? tok) (fchar tok seed)]
   [(eq? tok 'left-paren)
    (let ([new-seed (fdown seed)]
          [next-tk (next-tok port)])
      (if (eq? 'right-paren next-tk)
          seed
          (let loop ([child-seed (parse-s-expression port next-tk fdown fup fchar new-seed)])
            (let ([next-tk (next-tok port)])
              (cond
               [(eq? 'right-paren next-tk) (fup seed child-seed)]
               [(eof-object? next-tk) (fup seed child-seed)]
               [else (loop (parse-s-expression port next-tk fdown fup fchar child-seed))])))))]
   [(eq? tok 'right-paren) seed]))

(define p (open-input-string "(1 2 3 (4)"))

(parse-s-expression p
                    (next-tok p)
                    (lambda (seed) '())
                    (lambda (parent-seed child-seed)
                      (if (null? parent-seed)
                          (reverse child-seed)
                          (cons (reverse child-seed) parent-seed)))
                    (lambda (tok seed)
                      (cons tok seed))
                    '())
