(define p '(1 2 3))
(set-car! p 4)
(set-cdr! (cdr p) 5)

(print (and (= (car p) 4) (= (car (cdr p)) 2) (= (cdr (cdr p)) 5)))
