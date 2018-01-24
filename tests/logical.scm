(print
 (and
  (not (and #f #f))
  (not (and #t #f))
  (not (and #f #t))
  (and #t #t)
  (not (or #f #f))
  (or #t #f)
  (or #f #t)
  (or #t #t)
  (not #f)
  (not (not #t))))
