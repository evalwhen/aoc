(define fule
  (lambda (n)
    (- (fx/ n 3) 2)))

(define cacul-fule
  (lambda (part init)
    (if (eq? part 'part1)
        init
        (let loop ([n init] [sum 0])
          (if (<= n 0)
              sum
              (let ([fu (fule n)])
                (loop fu (+ sum n))))))))

(define run
  (lambda (infile part)
    (call-with-input-file infile
      (lambda (port)
        (let loop ([line (get-line port)] [sum 0])
          (if (eof-object? line)
              sum
              (loop (get-line port)  (+ sum (cacul-fule part (fule (string->number line)))))))))))


(run "input.txt" 'part1)
(run "input.txt" 'part2)
