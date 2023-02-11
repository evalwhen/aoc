;; day7 no space left on device
#lang racket

(require (file "../util.rkt"))

(struct node (name size parent)
  #:transparent
  #:mutable
  )

(struct tree node (children)
  #:transparent
  #:mutable)

(struct leaf node ()
  #:transparent
  #:mutable)

;; TODO: refactor no set!
(define build-tree
  (lambda (lines node)
    (cond
      [(null? lines) 'done]
      [(dir? (car lines))
       (build-tree (cdr lines) node)]
      [(cd-parent? (car lines))
       (build-tree (cdr lines) (node-parent node))]
      [(cd? (car lines))
       (begin (define curr (tree (car lines) 0 node null))
              (set-tree-children! node (cons curr (tree-children node)))
              (build-tree (cddr lines) curr))]
      [else (begin (set-tree-children! node (cons (leaf (file-name (car lines)) (file-size (car lines)) node)
                                                  (tree-children node)))
                   (build-tree (cdr lines) node))])))

(define parse-file-info
  (lambda (line)
    (let* ([raw (string-split line " ")]
           [size (string->number (car raw))]
           [filename (cadr raw)])
      (values size filename))))

(define file-size
  (lambda (line)
    (let-values ([(s _) (parse-file-info line)])
      s)))

(define file-name
  (lambda (line)
    (let-values ([(_ n) (parse-file-info line)])
      n)))


(define cd?
  (lambda (line) (string=? (substring line 0 4) "$ cd")))

(define cd-parent?
  (lambda (line) (string=? line "$ cd ..")))

(define dir?
  (lambda (line) (string=? (substring line 0 3) "dir")))

(define root (tree "root" 0 null null))

(define lines (reverse (parse-inputs "input1.txt" (lambda (x) x))))

(build-tree lines root)

;; lines
(define caculate-size
  (lambda (nod)
    (match nod
      [(struct leaf (name size _)) size]
      [(struct tree (_ _ _ children))
       (begin
         (define total (sum-children-size children 0))
         (set-node-size! nod total)
         total)])))

(define sum-children-size
  (lambda (children sum)
    (cond
      [(null? children) sum]
      [else (sum-children-size (cdr children) (+ sum (caculate-size (car children))))])))

(define root-size (caculate-size root))

(define is-root
  (lambda (node)
    (or
     (string=? (node-name node) "root")
     ;; (string=? (node-name node) "$ cd /")
     )))


(define filter-tree
  (lambda (tree pred?)
    (letrec ([iter-tree (lambda (tree res)
                          (cond
                            [(is-root tree) (filter-children (tree-children tree) res)]
                            [(leaf? tree) res]
                            [else (cond
                                    [(pred? tree) (filter-children
                                                   (tree-children tree)
                                                   (cons (node-size tree) res))]
                                    [else (filter-children
                                           (tree-children tree)
                                           res)])]))]
             [filter-children (lambda (children res)
                                (for/fold ([lst res])
                                          ([node children])
                                  (append lst (iter-tree node null))))])
      (iter-tree tree null))))

(define puzzle1
  (lambda (tree)
    (define pred? (lambda (tree) (<= (node-size tree) 100000)))
    (apply + (filter-tree tree pred?))))

(define puzzle2
  (lambda (tree)
    (define unused (- 70000000 root-size))
    (define total 30000000)
    (define need (- total unused))
    (define pred? (lambda (tree) (>= (node-size tree) need)))
    (first (sort (filter-tree tree pred?) <))))

;; (puzzle1 root)
(puzzle2 root)

;; root
