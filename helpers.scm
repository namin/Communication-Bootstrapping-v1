(define (pp x)
  (begin
    (printf "~a\n" x)
    x))

(define (round->exact x)
  (let ((r (round x)))
    (if (flonum? r) (flonum->fixnum r) r)))

(define (make-log name)
  (let ((fout (open-output-file name 'replace)))
    (lambda (x) (if (eq? x 'flush-buffer)
               (flush-output-port fout)
               (fprintf fout "~a\n" x)))))
;; exa of use:
;; (define thelog (make-log "thefile.out"))
;; (thelog '(this is a list I want to log))
;; To flush to disk:
;; (thelog 'flush-buffer)

(define (random-range n)
  (define (permute ns n)
    (if (null? ns)
        '()
        (let* ((i (random n))
              (r (list-ref ns i)))
          (cons r (permute (remq! r ns) (- n 1))))))
  (permute (iota n) n))

(define (random-permutation xs)
  (let* ((n (length xs))
         (p (random-range n)))
    (map (lambda (i) (list-ref xs i)) p)))

(define (->uniset xs)
  (if (null? xs)
      '()
      (cons (car xs) (->uniset (remove (car xs) (cdr xs))))))

(define (contained-set? a b r)
  (not (find (lambda (x) (not (find (lambda (y) (r x y)) b))) a)))
(define (equal-set? a b r)
  (and (contained-set? a b r)
       (contained-set? b a r)))
