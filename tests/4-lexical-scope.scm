(define (f)
  (define x 2)
  (define (g) x)
  g)

(define (h g)
  (define x 8)
  (g))

(print (= 2 (h (f))))
