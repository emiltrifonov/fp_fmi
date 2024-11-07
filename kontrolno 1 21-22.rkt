(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (filter p l)
  (cond ((null? l) l)
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

(define (range a b)
  (if (> a b) `()
      (append (list a) (range (+ a 1) b))))

(define (minimum l)
  (foldr1 (lambda (x y) (if (< x y) x y)) l))

;1
(define (done? n)
  (= (+ n 2) (apply + (filter (lambda (x) (= (remainder n x) 0)) (range 1 (/ n 2))))))

(define (range-done a b)
  (filter done? (range a b)))
;(range-done 5 24)

(define (sum-almost-done beg end)
  (define (helper range dones i size)
    (cond ((null? range) 0)
          ((or (done? (car range))
               (not (= 0 (length (filter (lambda (x) (and (< x (- size i 1)) (<= x i))) (map (lambda (x) (abs (- (car range) x))) dones))))))
           (+ (car range) (helper (cdr range) dones (+ i 1) size))) ;dif <= i
          (else (helper (cdr range) dones (+ i 1) size))))
  (helper (range beg end) (range-done beg end) 0 (length (range beg end))))

(define (list-almost-done beg end)
  (define (helper range dones i size)
    (cond ((null? range) `())
          ((or (done? (car range))
               (not (= 0 (length (filter (lambda (x) (and (<= x i) (< x (- size i 1)))) (map (lambda (x) (abs (- (car range) x))) dones))))))
           (append (list (car range)) (helper (cdr range) dones (+ i 1) size))) ;dif <= i
          (else (helper (cdr range) dones (+ i 1) size))))
  (helper (range beg end) (range-done beg end) 0 (length (range beg end))))

;(sum-almost-done 5 24)
;(list-almost-done 5 24)

;2
(define (intorchar? x)
  (or (number? x) (symbol? x)))

(define (insert x l)
  (append (list x) l))

(define (run-machine l)
  (define (repeat op stack n)
    (if (or (= n 0) (< (length stack) 2) (or (symbol? (car stack)) (symbol? (cadr stack)))) stack
          (repeat op (insert (op (car stack) (cadr stack)) (cddr stack)) (- n 1))))
          
  (define (run l acc)
    (cond ((null? l) acc)
          ((intorchar? (car l)) (run (cdr l) (insert (car l) acc)))
          ((pair? (car l)) (run (cdr l) (repeat (caar l) acc (cdar l))))
          ((procedure? (car l)) (run (cdr l) (map (lambda (x) (if (number? x) ((car l) x) x)) acc)))
          (else (run (cdr l) acc))))

  (run l `()))

(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6)) ;→ (6 5 4 3 a 2 x 1)
(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6 (cons + 2) (cons * 5))) ;→ (45 a 2 x 1)






































