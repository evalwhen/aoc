#lang racket

(provide parse-inputs)

(define parse-inputs
  (lambda (path consume)
    (call-with-input-file path
      (lambda (in)
        (letrec ([loop (lambda (res)
                         (let ([line (read-line in)])
                           (cond
                             [(equal? line eof) res]
                             [else (loop (cons (consume line) res))])))])
          (loop '()))))))
