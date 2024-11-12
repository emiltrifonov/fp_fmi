#lang racket
;Tree
;1
(define make-tree list)
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree `())
(define empty-tree? null?)
(define tree? list?)

(define t (make-tree 1
                     (make-tree 2 (make-tree 4 `() `()) (make-tree 5 `() `()))
                     (make-tree 3 (make-tree 6 `() `()) (make-tree 7 `() `()))))
(define bst (make-tree 4
                     (make-tree 2 (make-tree 1 `() `()) (make-tree 3 `() `()))
                     (make-tree 8 (make-tree 6 `() `()) (make-tree 9 `() `()))))
;t
;bst

;2
(define (pre-order t)
  (if (empty-tree? t) `()
      (append (list (root-tree t)) (pre-order (left-tree t)) (pre-order (right-tree t)))))

;(pre-order t)

(define (in-order t)
  (if (empty-tree? t) `()
      (append (in-order (left-tree t)) (list (root-tree t)) (in-order (right-tree t)))))

;(in-order t)

(define (post-order t)
  (if (empty-tree? t) `()
      (append (post-order (left-tree t)) (post-order (right-tree t))  (list (root-tree t)))))

;(post-order t)


;3
(define (map-tree f t)
  (if (empty-tree? t) `()
      (make-tree (list (f (root-tree t)))
                 (map-tree f (left-tree t))
                 (map-tree f (right-tree t)))))

;(map-tree (lambda (x) (* x x)) t)

;4
(define (height t)
  (if (empty-tree? t) 0
  (max (+ 1 (height (left-tree t))) (+ 1 (height (right-tree t))))))

;(height t)

;5
(define (level n t) ;no 'n' validation
  (if (= n 0) (list (root-tree t))
      (append (level (- n 1) (left-tree t)) (level (- n 1) (right-tree t)))))

;(level 2 t)

;6
(define (count-leaves t)
  (cond ((empty-tree? t) 0)
        ((and (not (empty-tree? t))
           (empty-tree? (left-tree t))
           (empty-tree? (right-tree t))) 1)
        (else (+ (count-leaves (left-tree t)) (count-leaves (right-tree t))))))

;(count-leaves t)

;7
(define (remove-leaves t)
  (cond ((empty-tree? t) `())
        ((and (not (empty-tree? t))
           (empty-tree? (left-tree t))
           (empty-tree? (right-tree t))) `())
        (else
         (make-tree
          (root-tree t)
          (remove-leaves (left-tree t))
          (remove-leaves (right-tree t))))))

;(remove-leaves t)

;8
(define (invert t)
  (if (empty-tree? t) `()
      (make-tree (root-tree t) (invert (right-tree t)) (invert (left-tree t)))))

;(invert t)

;9

;10

  
;Assoc list
(define (keys alist)
  (map car alist))

(define (values alist)
  (map cdr alist))

;1
(define l (list 'a 'b 'c 'd 'e 'f))

(define (enumerate l)
  (define (helper i l)
    (if (null? l) `()
        (append (list (cons i (car l))) (helper (+ i 1) (cdr l)))))
  (helper 1 l))

;(enumerate l)
 
;2
(define ass (list (cons 1 2) (cons 2 3)))
(define l2 (list 8 7 1 7 8 2 2 8 2 7 8 1))

(define (histogram l)
  (define (helper acc l)
    (cond ((null? l) `())
          ((assoc (car l) acc) (append acc (helper acc (cdr l))))
          (else
           (append acc
            (list (cons (car l) (length (filter (lambda (x) (= x (car l))) l))))
            (helper acc (cdr l))))))
  (helper `() l))

;(histogram l2) .|.
  






















