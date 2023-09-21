;; chez scheme version
;; --- Day 14: Regolith Reservoir ---
#!r6rs

;; TODO: refactor code
(import
        (rnrs)
        ;; (rnrs hashtables)
        (utils)
        )

(define start (cons 500 0))

;; x represents distance to the right and y represents distance down
;; (cons x y)
;; TODO: validate pos
(define (maybes pos)
  (let ((x (car pos))
        (y (cdr pos)))
    (list
     (list 'down (cons x
                       (+ y 1)))
     (list 'left-down (cons (- x 1)
                            (+ y 1)))
     (list 'right-down (cons (+ x 1)
                             (+ y 1))))))

;; wraped pos
(define (down? wpos)
  (eq? 'down (car wpos)))

(define (unwrap wpos)
  (cadr wpos))

(define (fall2 pos state maxy)
  ;; (displayln pos)
  (letrec ((iter (lambda (pos downtimes)
                   (cond
                    ;; void detect
                    ;; ((> downtimes 1000) #f)
                    ((next2 pos state maxy) => (lambda (nextpos)
                                                ;; (displayln nextpos)
                                                (if (down? nextpos)
                                                  (iter (unwrap nextpos) (inc downtimes))
                                                  (iter (unwrap nextpos) 0))))
                    (else pos)))))
    (iter pos 0)))

(define (find-one list pred)
  (cond
   ((null? list) #f)
   ((pred (car list)) (car list))
   (else (find-one (cdr list) pred))))

(define (next2 pos state maxy)
  (find-one (maybes pos)
            (lambda (mp)
              (and (miss? state (unwrap mp))
                   (not (equal? (+ maxy 2)
                                (cdr (unwrap mp))))))))

;; todo

(define (miss? state mp)
  (not (assoc mp state)))

(define (update state mp)
  (cons (list mp 'sand) state))

(define (run2 state maxy)
  (cond
   ((fall2 start state maxy) => (lambda (pos)
                                  ;; (displayln pos)
                                  (if (equal? pos start)
                                    (update state pos)
                                    (run2 (update state pos) maxy))))
   (else state)))

;; parse-input
(define (parse-line line)
  (map (lambda (rawpos)
         (let ((parts (string-split rawpos ",")))
           (cons (string->number (car parts))
                 (string->number (cadr parts)))))
       (string-split line " -> ")))

(define (parse-lines lines)
  (map parse-line lines))

(define (expand-segment start end)
  (letrec* ((sx (car start))
            (sy (cdr start))
            (ex (car end))
            (ey (cdr end))
            (xdiff (abs (- sx ex)))
            (ydiff (abs (- sy ey)))
            (enumate (lambda (start count modify)
                       (cond
                        ((= count 0) '())
                        (else (cons start (enumate (modify start) (dec count) modify))))))
            (pairs (lambda (xs ys)
                     (map (lambda (x y)
                            (cons x y))
                          xs
                          ys))))
    (if (= xdiff 0)
      (pairs (enumate sx (inc ydiff) id)
             (enumate (min sy ey) (inc ydiff) inc))
      (pairs (enumate (min sx ex) (inc xdiff) inc)
             (enumate sy (inc xdiff) id)))))

(define (expand poses)
  (cond
   ((null? poses) '())
   ((null? (cdr poses)) '())
   (else (append (expand-segment (car poses) (cadr poses))
                 (expand (cdr poses))))))

(define (init-state lines)
  (map (lambda (pos)
         (list pos 'solid))
       (append-map (lambda (line)
                     (expand (parse-line line)))
                   lines)))

(define (count-sand state)
  (filter (lambda (ae)
            (eq? 'sand (cadr ae)))
          state))

(define (find-max-y state)
  (letrec ([iter (lambda (state mx)
                   (cond
                    [(null? state) mx]
                    [(< mx (cdr (car (car state)))) (iter (cdr state)
                                                          (cdr (car (car state))))]
                    [else (iter (cdr state) mx)]))])
    (iter state 0)))

(define (puzzle2 path)
  (let* [(state (init-state (get-lines path)))
         (maxy (find-max-y state))]
    (length (count-sand (run2 state maxy)))
    ;; (run2 state maxy)
    ))

;; tests
(define example-data (list "498,4 -> 498,6 -> 496,6" "503,4 -> 502,4 -> 502,9 -> 494,9"))

(displayln (puzzle2 "input1.txt"))