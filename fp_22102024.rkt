(define (id x) x)

;; zad 1
(define (compose f g)
  (lambda (x) (f (g x)))
  )

(define (++ a) (+ a 1))
(define (-- a) (- a 1))

;;((compose ++ ++) 5)

;; zad 2
(define (const c)
  (lambda (x) c))

;;((const 4) 7)

;; zad 3
(define (fmax f g)
  (lambda (x)
    (define fval (f x))
    (define gval (g x))
    (if (> fval gval) fval gval)))

(define (f x) (+ 5 x))
(define (g x) (* 2 x))

;;((fmax f g) 4)
(define (const c)
  (lambda (c) 0))

(define relu (fmax id (const 0)))
;; x < 0 -> return 0
;; x > 0 -> return x
;;(relu 2)
    
;; zad 4

(define (repeated n f x)
  (if (<= n 0) x (repeated (- n 1) f (f x))))

;;(repeated 4 ++ 5)

;; zad 5

(define (repeat n f)
  (if (= n 0)
      (lambda (x) x)
      (let ((g (repeat (- n 1) f)))
        (lambda (x) (f (g x))))
          )
  )

;;((repeat 3 ++) 4)

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (sum-to-n n)
  (accumulate + 0 1 n (lambda (i) i) ++))

;;(sum-to-n 5)

;; zad 6
(define (count p? a b)
  (accumulate + 0 a b (lambda(i) (if (p? i) 1 0)) ++))

;(count odd? 1 10)

;; zad 7
(define (any? p a b)
  (define (op x y) (or x y))
  (accumulate op #f a b (lambda (i) (p i)) ++))

;;(any? (lambda (x) (= x 7)) 8 10)

;; zad 8
(define (all? p a b)
  (define (op x y) (and x y))
  (accumulate op #t a b (lambda (i) (p i)) ++))

;;(all? (lambda (x) (> x 7)) 7 10)

;; zad 9.1 repeated with accumulate

(repeated 4 ++ 5)


  


  

  






  