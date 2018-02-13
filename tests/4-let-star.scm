(print
 (and
  (= 3
     (let* ((a 2))
       (let* ((a 3) (b a))
         b)))
  (= 8
     (let* ((f (lambda () 8)))
       (let* ((f (lambda () (f))))
         (f))))))
