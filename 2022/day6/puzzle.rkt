#lang racket

(define process-lines
  (lambda (path consume)
    (call-with-input-file path
      (lambda (in)
        (letrec ([loop (lambda (res)
                         (let ([line (read-line in)])
                           (cond
                             [(equal? line eof) res]
                             [else (loop (cons (consume line) res))])))])
          (loop '()))))))

;; find first not-repeated string, it's length is packlen
(define (resolve packlen)
  (lambda (datastream)
    (define len (string-length datastream))
    (letrec ([iter (lambda (str si ei seen)
                     (cond
                       [(= len ei) 0]
                       [(>= (- ei si) packlen) (begin
                                           ;; (println si)
                                           ;; (println ei)
                                                ei)]

                       [(dict-ref seen (string-ref str ei) false)
                        => (lambda (pos)
                             ;; (println pos)
                             (iter str
                                   (if (>= pos si) (add1 pos) si)
                                   (add1 ei)
                                   (dict-set seen (string-ref str ei) ei)))]
                       [else (iter str
                                   si
                                   (add1 ei)
                                   (dict-set seen (string-ref str ei) ei))]))])
      (iter datastream 0 0 (hash)))))

(module+ test
  ;; (resolve "bvwbjplbgvbhsrlpgdmjqwftvncz")
  ;; (resolve "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
  ;; (resolve "abca")
  )

(define (puzzle1 path)
  (process-lines path (resolve 4)))

(define (puzzle2 path)
  (process-lines path (resolve 14)))

(puzzle1 "input1.txt")
(puzzle2 "input1.txt")
