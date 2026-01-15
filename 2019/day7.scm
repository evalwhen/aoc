(declare (standard-bindings) (extended-bindings))

;; intcode computer
(include "./day5-gambit.scm")


(define code-file-name "day7.input")

(define run-by-phases
  (lambda (ps)
    ;; i 第几个放大器，s 当前的输入信号
    (let lp ((i 0) (s 0))

      ;; (displayln i)
      (if (= i 5)
          s
          (let ((code (load-code code-file-name))
                (phase (vector-ref ps i))
                (input (make-queue))
                (output (make-vector 1)))
            (displayln code)
            ;; 第 0 个输入是 phase setting
            ;; 第 1 个输入是 input signal
            (enqueue! input phase)
            (enqueue! input s)
            (run-code code input output)
            (lp (inc i) (vector-ref output 0)))))))


(define shuffle
  (lambda (lst)
    (letrec ((recur (lambda (lst)
                      (if (null? lst)
                          '(())
                          (apply append (map (lambda (l) (insert (car lst) l))
                                             (recur (cdr lst)))))))
             (insert (lambda (a lst)
                       (if (null? lst)
                           (list (list a))
                           (cons (cons a lst)
                                 (map (lambda (l) (cons (car lst) l))
                                      (insert a (cdr lst))))))))
      (map list->vector (recur lst)))))

(define insert
  (lambda (a lst)
    (if (null? lst)
        (list (list a))
        (cons (cons a lst)
              (map (lambda (l) (cons (car lst) l))
                   (insert a (cdr lst)))))))

(define solve-part1
  (lambda (pss)
    (apply max (map run-by-phases pss))))


(displayln (solve-part1 (shuffle '(0 1 2 3 4))))

(define run-feedloop
  (lambda (codes ps)
    (let lp ((i 0) (codes codes) (pass 0))
      (let ((input (make-queue))
            (output ()))))))
