(define (f x y)
  (set! y (+ y 1))
  (* x y))

(print (= 8 (f 2 3)))
