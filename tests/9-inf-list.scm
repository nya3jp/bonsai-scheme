(define l '(2))
(set-cdr! l l)

(print (= (car (cdr (cdr (cdr l)))) 2))
