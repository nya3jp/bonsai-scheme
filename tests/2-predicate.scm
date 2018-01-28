(print
 (and
  (eq? '() '())
  (not (eq? '() '(1 2)))
  (not (eq? '(1 2) '(1 2)))
  (eq? #f #f)
  (eq? #t #t)
  (not (eq? #f #t))
  (eq? 'foo 'foo)
  (not (eq? 'foo 'bar))))
