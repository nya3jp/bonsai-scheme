(define (fib n)
  (cond
   ((<= n 1) n)
   (else (+ (fib (- n 1)) (fib (- n 2))))))

(print (= 55 (fib 10)))
