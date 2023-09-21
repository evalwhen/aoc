(library (strings)

  (export substr-indexes
          substr-index)

  (import (rnrs))


  ;; (displayln (substr-index "at" "at at at"))
  ;; will return 2
  ;; return substr end index
  (define (substr-index substr str)
    (letrec* ([strlen (string-length str)]
              [sublen (string-length substr)]
              [iter (lambda (i j)
                      (cond
                       [(= sublen j) i]
                       [(<= strlen i) #f]
                       [(char-ci=? (string-ref str i)
                                   (string-ref substr j))
                        (iter (+ i 1) (+ j 1))]
                       [else (iter (+ i 1) j)]))])
      (iter 0 0)))

  (define (substr-indexes substr str)
    (letrec* ([strlen (string-length str)]
              [sublen (string-length substr)]
              [iter (lambda (i j res)
                      (cond
                       [(= sublen j) (iter (+ i 1) 0 (cons i res))]
                       [(<= strlen i) (reverse res)]
                       [(char-ci=? (string-ref str i)
                                   (string-ref substr j))
                        (iter (+ i 1) (+ j 1) res)]
                       [else (iter (+ i 1) j res)]))])
      (iter 0 0 '())))
)