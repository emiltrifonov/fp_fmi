;;zad 1
(define (sum-digits x)
  (cond
    ((< x 0) (sum-digits (* x -1)))
    ((< x 10) x)
    (else (+ (remainder x 10) (sum-digits (quotient x 10))))
    )
  )

;;(sum-digits 4121)

;;zad 2
(define (count-divisors x)
  (define (helper res div)
    (cond
      ((= div 0) res)
      ((= (remainder x div) 0 ) (helper (+ res 1) (- div 1)))
      (else (helper res (- div 1)))
      )
    )
  (helper 1 (floor(/ x 2)))
  )

;;(count-divisors 9)

;;zad3
(define (prime? x)
  (define (helper div)
        (cond
          ((= div 1) #t)
          ((= (remainder x div) 0 ) #f)
          (else (helper (- div 1)))
          )
         )
  (if (<= x 1) (#f) (helper (floor(expt x 0.5))))
  )

;;(prime? 14)

(define (increasing-digits x)
  (define (helper num prev_digit)
    (cond
       ((= num 0) #t)
       ((> (remainder num 10) prev_digit) #f)
       (else(helper (floor(quotient num 10)) (remainder num 10)))
       )
    )
  (cond
    ((< x 0) (increasing-digits (* x -1)))
    ((< x 10) #t)
    (else (helper (floor(quotient x 10)) (remainder x 10)))
     )
    )

(increasing-digits 15998)










