(include "intcode-computer.scm")

(let-values (((new-state return-type value) (run-vm (make-vmstate (load-code "day5.input") 0 0 (list 5)))))
  (displayln return-type)
  (displayln value))
