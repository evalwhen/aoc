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

(define solve-part1
  (lambda (pss)
    (apply max (map run-by-phases pss))))


;; (displayln (solve-part1 (shuffle '(0 1 2 3 4))))

(define-structure amp idx ip input code)

;; (define init-amps
;;   (lambda (ps)
;;     (let lp ((i 0) (res '()))
;;       (cond
;;        ((= i 5) (reverse res))
;;        (else (let ((q (make-queue)))
;;                (if (= i 0)
;;                    (begin
;;                      (enqueue! q (vector-ref ps i))
;;                      (enqueue! q 0)
;;                      )
;;                    (enqueue! q (vector-ref ps i)))
;;                (lp (inc i) (cons (list 0 q (load-code code-file-name))
;;                                  res))))))))

;; 尽量避免使用以上手动循环的代码来处理逻辑，第一时间用 map fold 等高阶函数或者手动递归，直接处理元素
(define init-amps
  (lambda (ps)
    (let ((amps (map (lambda (p)
                       (let ((q (make-queue)))
                         (enqueue! q p)
                         (list 0 q (load-code code-file-name))))
                     (vector->list ps))))
      ;; 初始化第一个放大器的输入
      (enqueue! (list-ref (car amps) 1) 0)
      amps)))

;; (displayln (init-amps (vector 5 6 7 8 9)))

(define run-feedloop
  (lambda (amps)
    (let lp ((i 0))
      (let* ((amp (list-ref amps i))
             (next-amp (list-ref amps (modulo (inc i) 5)))
             (input (list-ref amp 1))
             (output (list-ref next-amp 1)))
        ;; (displayln amp)
        ;; (displayln (amp-idx next-amp))
        (call-with-values
            (lambda () (run-code-v2 (list-ref amp 0) (list-ref amp 2) input output))
          (lambda (status new-ip)
            (set-car! amp new-ip)
            (cond
             ((and (eq? status 'done)
                   (= i 4))
              (peek-queue-last output))
             (else
              (lp (modulo (inc i) 5))))))
        ))))


(displayln (apply max (map run-feedloop (map init-amps (shuffle '(5 6 7 8 9))))))
