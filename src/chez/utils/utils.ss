#!r6rs

(library (utils)
  (export id
          inc
          dec
          append-map
          displayln
          get-lines
          string-split)
  (import (rnrs (6)))

  (define (inc x) (+ x 1))
  (define (dec x) (- x 1))
  (define id (lambda (x) x))

  (define (append-map proc lst)
    (letrec ((iter (lambda (lst res)
                     (cond
                      ((null? lst) res)
                      (else (iter (cdr lst) (append res (proc (car lst)))))))))
      (iter lst '())))

  (define (displayln thing)
    (display thing)
    (display "\n"))

  (define (string-split str sep)
    (letrec ((n (string-length str))
             (sn (string-length sep))
             (iter (lambda (str n curr res)
                     (cond
                      ((< n sn)
                       (reverse (cons (string-append curr str) res)))

                      ((string=? sep (substring str 0 sn))
                       (iter (substring str sn n)
                             (- n sn) 
                             ""
                             (cons curr res)))

                      (else (iter (substring str 1 n)
                                  (- n 1)
                                  (string-append curr (substring str 0 1))
                                  res))))))
      (iter str n "" '())))

  (define (get-lines path)
    (call-with-input-file path
      (lambda (port)
        (let f ([line (get-line port)] [lines '()])
          (cond
           ((eof-object? line) lines)
           (else (f (get-line port) (cons line lines))))))))

  )