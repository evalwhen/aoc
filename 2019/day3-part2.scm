(load "input-parse.scm")

(define point
  (lambda (x y)
    (cons x y)))

(define move-point
  (lambda (p d)
    (case d
      [(#\L) (cons (dec (car p))
                   (cdr p))]
      [(#\R) (cons (inc (car p))
                   (cdr p))]
      [(#\U) (cons (car p)
                   (inc (cdr p)))]
      [(#\D) (cons (car p)
                   (dec (cdr p)))])))

;; p poing
;; n step
;; m base step
;; d direction
;; s state, hashtable
(define move
  (lambda (p n m d s)
    (let lp ([i 0] [p p])
      (if (= i n)
          p
          (let ([np (move-point p d)])
            (hashtable-update! s np (lambda (v) v) (+ m i))
            (lp (inc i) np))))))

(define parse-step
  (lambda (intr)
    (values (string-ref intr 0)
            (string->number (substring intr 1 (string-length intr))))))

(define exe-line
  (lambda (line s)
    (call-with-input-string line
      (lambda (ip)
        (let lp ([token (next-token '() '(#\, #\newline *eof*) "parse input" ip)]
                 [sp (point 0 0)]
                 [steps 0])

          ;; (displayln token)
          (if (equal? token "")
              'done
              (let-values ([(d step) (parse-step token)])
                (read-char ip)
                (lp (next-token '() '(#\, #\newline *eof*) "parse input" ip)
                    (move sp step steps d s)
                    (+ steps step)))))
        ))))

(define (merge-hash h1 h2)
  (let ([h1-keys (hashtable-keys h1)])
    (let lp ([i 0] (acc '()))
      (displayln i)
      (displayln h1-keys)
      (cond
       [(= i (vector-length h1-keys)) acc]
       [(hashtable-contains? h2 (vector-ref h1-keys i))
        (lp (inc i)
            (cons (+ (hashtable-ref h1 (vector-ref h1-keys i))
                     (hashtable-ref h2 (vector-ref h1-keys i)))
                  acc))]
       [lp (inc i) acc]))))



(define (collect s)
  (let-values ([(ks vs) (hashtable-entries s)])
    (let lp ([i 0] [c '()])
      (cond
       [(= i (vector-length vs)) c]
       [(>= (vector-ref vs i) 2) (lp (inc i) (cons (vector-ref ks i) c))]
       [else (lp (inc i) c)]
       ))))

(define (m-distance  p1 p2)
  (if (equal? p1 (cons 0 0))
      122222222222222222
      (+ (abs (- (car p1)
                 (car p2)))
         (abs (- (cdr p1)
                 (cdr p2))))))

(define run
  (lambda (infile)
    (call-with-input-file infile
      (lambda (ip)
        (let ([ht1 (make-hashtable equal-hash equal?)]
              [ht2 (make-hashtable equal-hash equal?)])

          (exe-line (get-line ip) ht1)

          (display (hashtable-keys ht1))

          (exe-line (get-line ip) ht2)

          (displayln (hashtable-keys ht2))

          (apply min (merge-hash ht1 ht2))

        )))))


(run "day3.input")

;; TODO:
;; 1. 高效 hashtable 操作
;; 2. 合并 hashtable
;; 3. 简化代码
;; 4. 注意审题
;;
