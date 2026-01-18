(declare (standard-bindings) (extended-bindings))

;; hashtable
(import (srfi 69))

(include "./myenv-gambit.scm")


(define build-line
  (lambda (data layer-start-idx wider linum)
    (let ((line-start-idx (+ layer-start-idx (* wider linum) )))
      (let lp ((i 0) (line '()))
        (cond
         ((= i wider) (reverse line))
         (else (lp (inc i) (cons (string-ref data (+ line-start-idx i))
                                 line))))))))

(define build-layer
  (lambda (data layer-start-idx wider higher)
    (let lp ((linum 0) (layer '()))
      (cond
       ((= linum higher) (reverse layer))
       (else (lp (inc linum) (cons (build-line data layer-start-idx  wider linum)
                                   layer))))))
  )

(define get-layers
  (lambda (data wider higher)
    (let* ((layer-step (* wider higher))
          (layer-count (quotient (string-length data) layer-step)))
      (let lp ((layer 0) (layers '()))
        (cond
         ((= layer layer-count) (reverse layers))
         (else (lp (inc layer)
                   (cons (build-layer data (* layer layer-step) wider higher)
                         layers))))))))


(displayln (get-layers "012345678901" 3 2))

(define count-digits-layer
  (lambda (layer digits)
    (let ((c (make-hash-table)))
      (fold (lambda (line c) (count-digits-line line digits c))
            c
            layer)
      c)))

(define count-digits-line
  (lambda (line digits c)
    (fold (lambda (a c) (if (member a digits)
                            (begin
                              (hash-table-update!/default c a (lambda (d) (inc d)) 0)
                              c)
                            c))
          c
          line)))


(define cnts (map (lambda (layer) (count-digits-layer layer (string->list "0 1 2"))) (get-layers "012345678881" 3 2)))

(define get-input
  (lambda (infile)
    (call-with-input-file infile
      (lambda (p)
        (read-line p)))))

(define run-part1
  (lambda (infile)
    (let* ((line (get-input infile))
           (layers (get-layers line 25 6))
           (cnt-fn (lambda (layer) (count-digits-layer layer (string->list "0 1 2"))))
           (cnts (map cnt-fn layers))
           (sorted (list-sort (lambda (c1 c2)
                                (< (hash-table-ref/default c1 #\0 0)
                                   (hash-table-ref/default c2 #\0 0)))
                              cnts))
           (min-cnt (car sorted)))
      (* (hash-table-ref/default min-cnt #\1 0)
         (hash-table-ref/default min-cnt #\2 0)))))

;; (displayln (run-part1 "day8.input"))

(define run-part2
  (lambda (infile wider higher)
    (let* ((input (get-input infile))
           (layers (get-layers input wider higher)))

      (letrec (;; pxes 是一个像素列表，同上到下 比如 '(0 1 2 0)
               ;; 该函数压缩整个像素为单一像素
               (merge-pxes (lambda (pxes)
                             (cond
                              ((null? (cdr pxes)) (car pxes))
                              ((equal? #\2 (car pxes)) (merge-pxes (cdr pxes)))
                              (else (car pxes)))))

               ;; get-pxes-list 将多个行重叠起来，然后获取纵向的相交像素点
               (get-pxes-list (lambda (lines)
                                (apply map list lines)))

               ;; 先将行排成从上到下，然后压缩同位置的像素点
               (merge-lines (lambda (lines)
                              (map merge-pxes (get-pxes-list lines))))

               ;; 先将整个 layer 排列成从上到下， (apply map (lambda x x) layers)
               ;; 然后压缩每个像素点
               (merge-layers (lambda (layers)
                               (map merge-lines (apply map list layers)))))
        (print-image (merge-layers layers))))))

(define print-image
  (lambda (layer)
    (for-each (lambda (line)
           (for-each (lambda (c) (display (if (equal? #\0 c) " " "1"))) line)
           (display "\n"))
         layer)))

(run-part2 "day8.input" 25 6)
