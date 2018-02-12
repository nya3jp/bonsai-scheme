(print
 (= 3
    (let* ((a 2))
      (let* ((a 3) (b a))
        b))))
