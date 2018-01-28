(print
 (and
  (eq? #f #f)
  (eq? #t #t)
  (eq? #f (not #t))
  (eq? #t (not #f))
  (eq? #f (< 2 1))
  (eq? #t (< 1 2))
  (eq? (cdr '(2)) (cdr '(8)))))
