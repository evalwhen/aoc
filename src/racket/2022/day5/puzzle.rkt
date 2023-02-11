#lang racket

(require (file "../util.rkt"))
(require (file "../stack.rkt"))

(struct action (from to cnt) #:transparent)

;; TODO: imp fold
(define/contract parse-stacks
  (-> string? (hash/c integer? (listof string?)))
  (lambda (input)
    (call-with-input-file input
      (lambda (port)
        (letrec ([iter-lines
                  (lambda (stacks)
                    (let ([line (read-line port)])
                      (cond
                        [(equal? line "") stacks]
                        [else (iter-lines
                               (for/fold ([stacks stacks])
                                         ([i (in-naturals 1)]
                                          [part (parse-line line)]
                                          #:unless (equal? part "   "))
                                 (hash-set stacks i (cons part (hash-ref stacks i null)))))])))])
          (iter-lines (hash)))))))

(define/contract (build-stacks raw-data)
  (-> (hash/c integer? (listof string?))
      (hash/c integer? stack?))
  (for/fold ([stacks (hash)])
            ([(k lst) (in-hash raw-data)])
    (define stk (for/fold ([stack (stack null)])
                          ([v lst])
                  (push stack v)))
    (hash-set stacks k stk)))

(define/contract parse-line
  (-> string? (listof string?))
  (lambda (line)
    (letrec ([select (lambda (str res)
                       (cond
                         [(equal? str "") (reverse res)]
                         [else (select (substring str 4)
                                       (cons (substring str 1 4) res))]))])
      (select (string-append " " line) '()))))

(define/contract parse-action
  (-> string? (or/c action? null?))
  (lambda (line)
    (match (string-split line)
      [(list "move" cnt "from" x "to" y) (action (string->number x)
                                                 (string->number y)
                                                 (string->number cnt))]
      [else null])))

(define/contract (move stacks act)
  (-> (hash/c integer? stack?)
      action?
      (hash/c integer? stack?))
  (match act
    [(struct action (from to cnt))
     (for/fold ([stacks stacks])
               ([i (in-range 0 cnt)])
       (step stacks from to))]))

(define/contract (step stacks from to)
  (-> (hash/c integer? stack?)
      integer?
      integer?
      (hash/c integer? stack?))
  (let ([from-stack (hash-ref stacks from)]
        [to-stack (hash-ref stacks to)])
    (let-values ([(v new-from) (pop from-stack)])
      (hash-set (hash-set stacks from new-from)
                to
                (push to-stack v)))))


(define/contract (split act)
  (-> action? (listof action?))
  (match act
    [(struct action (from to cnt)) (list (action from 0 cnt)
                                         (action 0 to cnt))]))
(define/contract same-order-move
  (-> (hash/c integer? stack?)
      action?
      (hash/c integer? stack?))
  (lambda (stacks act)
    (for/fold ([stacks stacks])
              ([act (split act)])
      (move stacks act))))

(module+ test
  (define stacks (hash 1 (stack (list "N" "Z"))
                       2 (stack (list "A" "B"))))
  (define one-move (action 1 2 1))
  ;; (move stacks one-move)
  ;; one-move
  ;; (parse-line " 1   2   3 ")
  ;; (parse-line "    [D]    ")
  ;; (build-stacks (parse-stacks "input.txt"))
  ;; (reverse (parse-inputs "input.txt" parse-action))
  )

(define (puzzle1 path)
  (let ([stacks (build-stacks (parse-stacks path))]
        [actions (reverse (parse-inputs path parse-action))])
    ;; (println stacks)
    (define res (for/fold ([stacks stacks])
                          ([a actions])
                  (move stacks a)))
    (for/fold ([str ""])
              ([i (in-naturals 1)]
               [k (hash-keys res)])
      (string-append str (substring (top (hash-ref res i))
                                    1 2)))))

(puzzle1 "input.txt")
(puzzle1 "input1.txt")

(define (puzzle2 path)
  (let ([stacks (build-stacks (parse-stacks path))]
        [actions (reverse (parse-inputs path parse-action))])
    ;; (println stacks)
    ;; (println actions)
    (define res (for/fold ([stacks (hash-set stacks 0 (stack null))])
                          ([a actions])
                  (same-order-move stacks a)))
    (for/fold ([str ""])
              ([i (in-range 1 (add1 (hash-count stacks)))]
               [k (hash-keys res)])
      (string-append str (substring (top (hash-ref res i))
                                    1 2)))))

(puzzle2 "input.txt")
(puzzle2 "input1.txt")
