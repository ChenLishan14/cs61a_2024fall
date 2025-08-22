(define (ascending? s) 
    (if (null? s)
        True
        (if (null? (cdr s))
            True
            (if (> (car s) (car (cdr s)))
            False
            (ascending? (cdr s))))))

(define (my-filter pred s) 
    (if (null? s)
        nil
        (if (null? (cdr s))
            (if (pred (car s))
                (cons (car s) nil)
                nil)
            (if (pred (car s))
                (cons (car s) (my-filter pred (cdr s)))
                (my-filter pred (cdr s))))))

(define (interleave lst1 lst2) 
    (if (and (not (null? lst1)) (not (null? lst2)))
        (cons (car lst1) (cons (car lst2) (interleave (cdr lst1) (cdr lst2))))
        (if (null? lst1)
            lst2
            lst1)))
#|
(define (no-repeats s) 
    (if (null? s)
        nil
        (if (number-in (cdr s) (car s))
            (no-repeats (cdr s))
            (cons (car s) (no-repeats (cdr s))))))
|#
(define (number-in s x)
    (cond 
        ((null? s) False)
        ((= (car s) x) True)
        (else (number-in (cdr s) x))))

(define (no-repeats s)
  (define (help lst result)
    (cond
      ((null? lst) nil)
      ((number-in result (car lst)) (help (cdr lst) result))
      (else (cons (car lst) (help (cdr lst) (cons (car lst) result))))))
  (help s nil))
