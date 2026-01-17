(include "./intcode-computer.scm")

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
      (recur lst))))



;; 根据 phases 设置，初始化五个虚拟机，依次对应 a b c d e 五个信号放大器
(define init-vm-states
  (lambda (phases)
    (let ((states (map (lambda (p)
                       (make-vmstate (load-code "./day7.input") 0 (list p)))
                     phases)))
      ;; 设置第一台机器的输入为 0
      ;; (append-input (car states) 0)

      (list->vector states))))

(define run-feedloop
  (lambda (vm-states)
    ;; current-signal 上个放大器的输出信号
    (let lp ((current-idx 0) (current-signal 0))
      (let ((current-vm-state (vector-ref vm-states current-idx)))

        ;; 为当前放大器设置输入，以上个放大器的输出为自己的输入
        (append-input current-vm-state current-signal)

        (call-with-values
            (lambda () (run-vm current-vm-state))
          (lambda (new-state return-type new-signal)
            (case return-type

              ;; 当前虚拟机返回了 ouput, 我们设置好当前虚拟机的新状态之后，相当于挂起当前虚拟机，lp 带着当前虚拟机的输出执行下一个虚拟机
              ((output)
               (vector-set! vm-states current-idx new-state)
               (lp (modulo (inc current-idx) 5) new-signal))

              ;; 当前虚拟机返回 halt, 我们先判定是不是最后一台虚拟机停止了，如果是我们返回当前有效的返回值, new-v 在此返回类型下是 void
              ;; 如果不是，我们继续下一台虚拟机的运行在此之前设置当前虚拟机新的状态
              ((halt) (if (= current-idx 4)
                          current-signal
                          (begin
                            (vector-set! vm-states current-idx new-state)
                            (lp (modulo (inc current-idx) 5) current-signal))))
              ((input) (error 'run-feedloop "不应该的状态")))))))))


(define states (init-vm-states (list 9 7 8 5 6)))

;; (displayln amps)
;; (displayln  (run-feedloop states))

(displayln (apply max
                  (map run-feedloop
                       (map (lambda (phases) (init-vm-states phases))
                            (shuffle '( 5 6 7 8 9))))))
