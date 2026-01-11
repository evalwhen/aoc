(declare (standard-bindings) (extended-bindings))

(import (srfi 69))
;; 用了 next-token, inc 等辅助函数
;; (include "myenv-gambit.scm")


;; 方案：
;; 从输入构建一个 hashtable, 包含所有数据，values 之间围绕 key

(define parse-line
  (lambda (line)
    (let ((idx (string-contains line ")")))
      (if idx
          (values (substring line 0 idx)
                  (substring line (+ 1 idx) (string-length line)))
          (error 'pare-line "invalid line" line)))))

(define parse-paths
  (lambda (infile)
    (call-with-input-file infile
      (let ((data (make-hash-table eq?)))
        (lambda (p)
          (let lp ((line (read-line p)))
            (if (eof-object? line)
                data
                (let*-values (((k v) (parse-line line))
                              ((k-sym) (string->symbol k))
                              ((v-sym) (string->symbol v)))
                  (begin
                    (hash-table-update!/default data k-sym (lambda (vs) (cons v-sym vs)) '())
                    (lp (read-line p)))))))))))

;; 构建从 COM 出发的所有 path, path 之间可能重合
(define build-paths
  (lambda (data init)
    (letrec ((recur (lambda (init)
                      (let ((vs (hash-table-ref/default data init '())))
                        (if (null? vs)
                            (list (list init))
                            (map (lambda (path) (cons init path))
                                 ;; 多个 vs 之间的路径是平级，因此 append 起来
                                 (apply append (map recur vs))))))))
      (recur init))))


(define data (parse-paths "day6.input"))

;; (display (hash-table-size data))
;; (display (hash-table-ref/default data 'COM 'none))
;; (display (hash-table-ref/default data 'H 'none))
;; (display (build-paths data 'COM))

;; TODO: 用 fold 重构下
(define run
  (lambda (paths)
    (let ((visited (make-table 'test: eq?)))
      (letrec ((recur (lambda (path i t)
                        (if (null? path)
                            t
                            (recur (cdr path)
                                   (+ 1 i)
                                   (if (table-ref visited (car path) #f)
                                       t
                                       (begin
                                         (table-set! visited (car path) #t)
                                         (+ i t)))))))
               (lp (lambda (paths t)
                     (if (null? paths)
                         t
                         (lp (cdr paths)
                             (recur (car paths) 0 t))))))
        (lp paths 0)))))

;; basic test
;; (display (run '((COM B G H) (COM B  C D I) (COM B  C D E J K L) (COM B  C D E F))))

(display (run (build-paths data 'COM)))
