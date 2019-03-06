;; be liberal in what you accept, and strict in what you output
;; trying to get around the code conflating #f and nil
(define (yes? x)
  (and x (not (null? x))))

(define (no? x)
  (or (not x) (null? x)))

(define (proper-null x)
  (if (no? x) '() x))

(define (consp a b)
  (cons a (proper-null b)))

(define (appendp . args)
  (apply append (map proper-null args)))

(define (first a)  (and (yes? a) (car a)))
(define (second a) (and (yes? a) (first (cdr a))))
(define (third a)  (and (yes? a) (second (cdr a))))
(define (fourth a) (and (yes? a) (third (cdr a))))
(define (fifth a)  (and (yes? a) (fourth (cdr a))))

(define (list-transform-negative xs f)
  (and xs (filter (lambda (xs) (not (f xs))) xs)))

(define (list-transform-positive xs f)
  (and xs (filter f xs)))

(define (list-search-positive xs f)
  (and xs (find f xs)))

(define (split xs f)
  (and xs (list (filter f xs) (filter (lambda (x) (not (f x))) xs))))

(define (delq x xs)
  (remq x xs))
