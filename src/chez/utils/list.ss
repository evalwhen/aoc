(library (list)

  (export dedupe)

  (import (rnrs))

  ;; TODO: rewrite
  (define (dedupe e)
    (if (null? e) '()
        (cons (car e) (dedupe (filter (lambda (x) (not (equal? x (car e)))) 
                                      (cdr e))))))
 )