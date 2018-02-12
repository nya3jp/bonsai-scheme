(define (loop) (loop))

(print
 (and
  (= 28 (cond
         (#f (loop))
         (#t 28)
         (#t (loop))))
  (= 28 (cond
         (#f (loop))
         (else 28)))))
