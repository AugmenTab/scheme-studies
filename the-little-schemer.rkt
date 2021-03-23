#lang racket

;;--------------------------------------------------------------------------------------------------
; Chapter 2: Do It, Do It Again, and Again, and Again...

; atom? determines if an S-expression is an atom.
(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

; lat? looks at each S-expression in a list, in turn, and asks if each S-expression is an atom, 
; until it runs out of S-expressions. If it runs out without encountering a list, the value is #t.
; If it finds a list, the value is #f - false.
(define lat?
    (lambda (l)
        (cond
            ((null? l) #t)
            ((atom? (car l)) (lat? (cdr l)))
            (else #f))))

; member? looks at each S-expression in a list, in turn, and asks if each S-expression is equal to 
; the S-expression it has been provided. If it runs out of S-expressions before finding a match, 
; the value is #f. Otherwise, it is #t.
(define member?
    (lambda (a lat)
        (cond
            ((null? lat) #f)
            (else (or (eq? (car lat) a) (member? a (cdr lat)))))))
