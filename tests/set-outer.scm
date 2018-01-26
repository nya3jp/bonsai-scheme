(define inc
  ((lambda (x)
     (lambda ()
       (set! x (+ x 1))
       x)) 0))

(inc)
(inc)
(inc)

(print (= 4 (inc)))
