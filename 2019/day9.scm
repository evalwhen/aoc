(declare (standard-bindings) (extended-bindings))

(include "./intcode-computer.scm")


(define run-vm-until-done
  (lambda (state)
    (let lp ((state state))
      ;; (displayln (vmstate-ip state))
      ;; (displayln (vector-ref (vmstate-code state) (vmstate-ip state)))
      (call-with-values
          (lambda () (run-vm state))
        (lambda (new-state return-type value)
          (case return-type
            ((halt) value)
            ((input) (error 'run-vm-untile-done "need input"))
            ((output)
             (displayln return-type)
             (displayln value)
             (lp new-state))))))))

(run-vm-until-done (make-vmstate (load-code "day9.input") 0 0 '(1)))
