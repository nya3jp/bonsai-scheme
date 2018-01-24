(define (loop) (loop))

(print
 (= 28 (if #t (if #f (loop) 28) (loop))))
