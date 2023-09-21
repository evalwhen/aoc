;; chez scheme version
;; --- Day 14: Regolith Reservoir ---
#!r6rs

(import (rnrs)
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

;; will loop forever
;; TODO: void detect, now just use down times
(define (fall pos state downtimes)
  (cond
   ;; void detect
   ((> downtimes 1000) #f)
   ((next pos state) => (lambda (nextpos)
                          (displayln nextpos)
                          (if (down? nextpos)
                            (fall (unwrap nextpos) state (inc downtimes))
                            (fall (unwrap nextpos) state 0))))
   (else (update state pos))))

(define (find-one list pred)
  (cond
   ((null? list) #f)
   ((pred (car list)) (car list))
   (else (find-one (cdr list) pred))))

(define (next pos state)
  (find-one (maybes pos)
            (lambda (mp)
              (miss? state (unwrap mp)))))

(define (miss? state mp)
  (not (assoc mp state)))

(define (update state mp)
  (cons (list mp 'sand) state))

(define (run state)
  (cond
   ((fall start state 0) => (lambda (newstate)
                              (run newstate)))
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

(define (puzzle1 path)
  (length (count-sand (run (init-state (get-lines path))))))

;; tests
(define example-data (list "498,4 -> 498,6 -> 496,6" "503,4 -> 502,4 -> 502,9 -> 494,9"))

(displayln (parse-line "498,4 -> 498,6 -> 496,6"))
(displayln (parse-lines (list "498,4 -> 498,6 -> 496,6" "498,4 -> 498,6 -> 496,6")))
(displayln (expand-segment (cons 498 4) (cons 498 6)))
(displayln (expand-segment (cons 498 6) (cons 496 6)))
(displayln (expand (parse-line "498,4 -> 498,6 -> 496,6")))
(displayln (init-state example-data))
(displayln (assoc (cons 500 9) (init-state example-data)))
(displayln (maybes (cons 500 8)))
(displayln (next (cons 500 8) (init-state example-data)))
(displayln (length (count-sand (run (init-state example-data)))))
;; (displayln (puzzle1 "example.txt"))
(displayln (puzzle1 "input1.txt"))