#lang racket

(define (parse-inputs path line-consume)
  (call-with-input-file path
    (lambda (in)
      (letrec ([iter-lines (lambda (res)
                             (let ([line (read-line in)])
                               (cond
                                 [(eq? line eof) res]
                                 [else (iter-lines (cons (line-consume line) res))])))])
        (iter-lines '())))))

(define (line-consume line)
  (string-split line))

;; (parse-inputs "input1.txt" line-consume)

(define score
  (lambda (data)
    (apply + (map (lambda (round)
                    (match round
                      [(list "A" "X") (+ 3 1)]
                      [(list "B" "Y") (+ 3 2)]
                      [(list "C" "Z") (+ 3 3)]
                      [(list "C" "X") (+ 6 1)]
                      [(list "A" "Y") (+ 6 2)]
                      [(list "B" "Z") (+ 6 3)]
                      [(list _ "X") (+ 0 1 )]
                      [(list _ "Y") (+ 0 2)]
                      [(list _ "Z") (+ 0 3)]))
                  data))))

;; puzzle2, X => lose; Y => draw; Z => win
(define decrypt-round
  (lambda (round)
    (match round
      [(list "A" "X") (list "A" "Z")]
      [(list "A" "Y") (list "A" "X")]
      [(list "A" "Z") (list "A" "Y")]
      [(list "B" "X") (list "B" "X")]
      [(list "B" "Y") (list "B" "Y")]
      [(list "B" "Z") (list "B" "Z")]
      [(list "C" "X") (list "C" "Y")]
      [(list "C" "Y") (list "C" "Z")]
      [(list "C" "Z") (list "C" "X")])))

(score (parse-inputs "input1.txt" line-consume))
(score (parse-inputs "input2.txt" line-consume))

;; puzzle2
(score (map decrypt-round (parse-inputs "input1.txt" line-consume)))
(score (map decrypt-round (parse-inputs "input2.txt" line-consume)))
