(define (len n)
  (cond ((< n 0) (len (* n -1)))
        ((< n 10) 1)
        (else (+ 1 (len (floor (quotient n 10)))))))
;(length (list 1 2 3))
;(len 1)

(define (minimum l)
  (cond ((null? (cdr l)) (car l))
        (else
         (let ((rest-min (minimum (cdr l))))
           (if (< (car l) rest-min) (car l) rest-min)))))

(define (maximum l)
  (cond ((null? (cdr l)) (car l))
        (else
         (let ((rest-max (maximum (cdr l))))
           (if (> (car l) rest-max) (car l) rest-max)))))

(define (mymember x l)
  (if (null? l) #f
      (if (equal? (car l) x) l
          (mymember x (cdr l)))))

;(member 2 (list 1 2 3 4))

(define (foldr op init l)
  (if (null? l) init
      (op (car l) (foldr op init (cdr l)))))

(define (foldl op init l)
  (if (null? l) init
      (foldl op (op init (car l)) (cdr l))))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (if (null? (cdr l)) (car l)
      (foldl op (car l) (cdr l))))

;(foldr1 (lambda (x y) (if (> x y) x y)) (list 3 5 2))
;(foldl1 (lambda (x y) (if (< x y) x y)) (list 3 5 2))

;(foldr + 0 (list 1 2 3))

(define (mymap f l)
  (foldr (lambda (x y) (append (list (f x)) y)) `() l))

(define (myfilter p? l)
  (foldr (lambda (x y) (if (p? x) (append (list x) y) y)) `() l))

;(myfilter odd? (list 12  9282748 92 82728 94 8 38 83 84 73 73 94 9 939))

(define l (list 1 2 3 4 5 6 7 8 9 10))
(define l0 (list 0 1 2 3 4 5 6 7 8 9))
(define el `())

;1
(define (numtolist n)
  (if (< n 10) (list n)
      (append (numtolist (floor (quotient n 10))) (list (remainder n 10)))))

(define (palindrome? n)
  (define listn (numtolist n))
  (cond ((or (= (remainder n 10) 0) (< n 0)) #f)
        ((< n 10) #t)
        (else (equal? listn (reverse listn)))))

;2
(define (range a b)
  (cond ((> a b) `())
        ((= a b) (list a))
        (else (append (list a) (range (+ a 1) b)))))

(define (count-pal a b)
  (foldl (lambda (acc x) (+ acc (if (palindrome? x) 1 0))) 0 (range a b)))

;3
(define (prime? n)
  (= (length(myfilter (lambda (x) (= (remainder n x) 0)) (range 2 (sqrt n)))) 0))

(define (sum-primes n k)
  (cond ((<= n 0) 0)
        ((prime? (+ k 1)) (+ (+ k 1) (sum-primes (- n 1) (+ k 1))))
        (else (sum-primes n (+ k 1)))))

;4
(define (next-prime n)
  (if (prime? (+ n 1)) (+ n 1) (next-prime (+ n 1))))

(define (count-power n p)
  (if (not (= 0 (remainder n p))) 0
      (+ 1 (count-power (/ n p) p))))

(define (prime-factors n)
  (define (helper n p)
    (define counter (count-power n p))
    (if (<= n 1) `()
        (if (= counter 0) (helper (/ n (expt p counter)) (next-prime p))
        (append (list (cons p counter)) (helper (/ n (expt p counter)) (next-prime p))))))
  (helper n 2))

;5
(define (increasing? l)
  (define (helper l)
    (if (null? (cdr l)) #t
        (and (< (car l) (cadr l)) (increasing? (cdr l)))))
  (if (null? l) #f
      (helper l)))

(define (progression? l)
  (define (helper l d)
    (if (null? (cdr l)) #t
        (and (= d (- (car l) (cadr l))) (helper (cdr l) d))))
  (cond ((null? l) #f)
        (else (let ((d (- (car l) (cadr l))))
                (helper (cdr l) d)))))

(define dupl (list 1 2 3 2 4 3))

(define (has-duplicates? l)
  (define (unique? l el)
    (if (null? l) #t
        (and (not (= el (car l))) (unique? (cdr l) el))))
  (define (helper l el)
    (if (null? l) #f
        (or (not (unique? l el)) (helper (cdr l) (car l)))))
  (if (or (null? l) (null? (cdr l))) #f (helper (cdr l) (car l))))

;6

(define (deldup l el)
  (cond ((null? l) `())
        ((= (car l) el) (deldup (cdr l) el))
        (else (append (list (car l)) (deldup (cdr l) el)))))

;(deldup `(1 2 3 1 2 1 4 5 5 1) 1)

(define (dedup l)
  (define (helper l)
    (if (null? l) `()
        (append (list (car l)) (helper (deldup l (car l))))))
  (if (or (null? l) (null? (cdr l))) l (helper l)))

;(dedup `(1 2 21 3 1 2 1 4 5 5 1))
          
;7
(define (union l s)
  (dedup (append l s)))

;(union `(1 2 3 4) `(1 3 5 7))

(define (contains? l n)
  (if (null? l) #f
      (or (= (car l) n) (contains? (cdr l) n))))

(define (intersection l s)
  (define (helper l s)
    (cond ((null? l) `())
          ((contains? s (car l)) (append (list (car l)) (helper (cdr l) s)))
          (else (helper (cdr l) s))))
  (if (or (null? l) (null? s)) `() (helper l s)))

;(intersection '(1 2 3 4) '(1 3 5 7))

(define (zip n l2)
  (if (null? l2) `()
      (append (list (cons n (car l2))) (zip n (cdr l2)))))

(define (product l s)
  (define (helper l s)
    (if (null? l) `()
          (append (zip (car l) s) (helper (cdr l) s))))
    (if (or (null? l) (null? s)) `() (helper l s)))

;(product `(1 2) `(3 5 1))

;8
(define (count l n)
  (if (null? l) 0
      (+ (if (= n (car l)) 1 0) (count (cdr l) n))))

(define (maximumvaluepair l)
  (cond ((null? (cdr l)) (car l))
        (else
         (let ((rest-max (maximumvaluepair (cdr l))))
           (if (> (cdar l) (cdr rest-max)) (car l) rest-max)))))

(define (most-common l)
  (define trimmedl (dedup l))
  (define (occurlist l trl)
    (if (null? trl) `()
        (append (list (cons (car trl) (count l (car trl)))) (occurlist l (cdr trl)))))
  (car (maximumvaluepair (occurlist l trimmedl))))

;(most-common '(1 2 3 2 2 1))

;9
(define (scalar-product xs ys)
  (define (helper xs ys)
    (if (null? xs) 0
        (+ (* (car xs) (car ys)) (helper (cdr xs) (cdr ys)))))
  (if (not (= (length xs) (length ys))) 0
      (helper xs ys)))

;10
(define (diag m)
  (if (null? m) `()
      (append (list (caar m)) (diag (map cdr (cdr m))))))

(define (diagonal-product matrix)
  (scalar-product (diag matrix) (diag (map reverse matrix))))

(define m1 '((1 0 1) (0 2 0) (3 0 3)))
(define m2 '((1 0 3) (0 2 0) (3 0 1)))

;(diagonal-product m1)
;(diagonal-product m2)

;11
(define (transpose m)
  (apply map list m))

(define (matrix-multiply m t)
  (map (lambda (row) (map (lambda (col) (apply + (map * row col))) (transpose t))) m))

(define m '((1 2) (3 4)))
(define t '((5 6) (7 8)))

;(matrix-multiply m t)

;15
(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (flatten l)
  (cond ((null? l) `())
        ((atom? l) (list l))
        (else (append (flatten (car l)) (flatten (cdr l))))))

(flatten '(1 2 3))
(flatten '(1 (2 3) (3 (4 (5)))))

















