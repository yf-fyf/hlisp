(define (iota n)
  (define (rec i)
    (if (= i n) null
        (cons i (rec (+ i 1)))))
  (rec 0))

(define (fold f v ls)
  (define (rec ls acc)
    (if (null? ls) acc
        (rec (cdr ls) (f (car ls) acc))))
  (rec ls v))

(define (filter f ls)
  (define (rec ls acc)
      (if (null? ls) (reverse acc)
          (rec (cdr ls)
                   (if (f (car ls))
                       (cons (car ls) acc)
                       acc))))
  (rec ls null))

(define (remove f ls)
  (filter (lambda (x) (not (f x))) ls))

(define (last ls) (car (reverse ls)))

(define (zip . lss)
  (define (rec lss acc)
    (if (null? (car lss))
        (reverse acc)
        (rec (map cdr lss) (cons (map car lss) acc))))
  (rec lss null))

(define (unzip1 ls) (map car ls))

(define (unzip2 ls)
  (list (map car ls) (map cadr ls)))

(define (unzip3 ls)
  (list (map car ls)
        (map cadr ls)
        (map caddr ls)))

(define (sum ls) (fold + 0 ls))

(define (length ls)
  (sum (map (lambda (x) 1) ls)))

(define (take ls n)
  (define (rec i ls acc)
    (if (= i 0)
        (reverse acc)
        (rec (- i 1) (cdr ls) (cons (car ls) acc))))
  (rec n ls null))

(define (drop ls n)
  (if (= n 0) ls
      (drop (cdr ls) (- n 1))))

(define (list-ref ls n)
  (if (= n 0) (car ls)
      (list-ref (cdr ls) (- n 1))))

