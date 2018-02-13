(define (fact n) -1)

(print
 (and
  (= -5
     (let* ((fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))))
       (fact 5)))
  (= 120
     (letrec ((fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))))
       (fact 5)))))
