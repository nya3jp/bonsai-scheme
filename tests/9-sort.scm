(define (null? x) (eq? '() x))

(define (list-equal? as bs)
  (cond
   ((and (null? as) (null? bs)) #t)
   ((or (null? as) (null? bs)) #f)
   ((not (= (car as) (car bs))) #f)
   (else (list-equal? (cdr as) (cdr bs)))))

(define (filter f xs)
  (cond
   ((null? xs) xs)
   ((f (car xs)) (cons (car xs) (filter f (cdr xs))))
   (else (filter f (cdr xs)))))

(define (concat as bs)
  (if (null? as)
      bs
      (cons (car as) (concat (cdr as) bs))))

(define (quick-sort xs)
  (cond
   ((null? xs) xs)
   ((null? (cdr xs)) xs)
   (else
    (let* ((p (car xs))
           (as (filter (lambda (x) (<= x p)) (cdr xs)))
           (bs (filter (lambda (x) (> x p)) (cdr xs))))
      (concat (quick-sort as) (cons p (quick-sort bs)))))))

(print
 (list-equal?
  '(1 2 3 4 5 6 7 8)
  (quick-sort '(8 3 7 4 5 1 6 2))))
