#lang racket

(define parse-inputs
  (lambda (filepath)
    (call-with-input-file filepath
      (lambda (port)
        (letrec ([elf-reader (lambda (res)
                               (let ([line (read-line port)])
                                 (cond
                                   [(or (equal? line "")
                                        (eq? line eof)) res]
                                   [else (elf-reader (cons (string->number line) res))])))]
                 [elves-reader (lambda (res)
                                 (let ([elves (elf-reader '())])
                                   (cond
                                     [(empty? elves) res]
                                     [else (elves-reader (cons elves res))])))])
          (elves-reader '()))))))

(apply + (take (sort (map (lambda (nums) (apply + nums))
                          (parse-inputs "input1.txt"))
                     >)
               3))
