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

;;--------------------------------------------------------------------------------------------------

; Chapter 3: Cons the Magnificent

; rember looks at each S-expression in a list, in turn, and compares it to the S-expression 
; provided. The first time it discovers an S-expression in that list that matches the provided 
; S-expression, it removes that S-expression from the list.
(define rember
    (lambda (a lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat) (rember a (cdr lat)))))))

; firsts looks at a list of lists of S-expressions, and builds a new list containing only the first 
; S-expression from each list in the original list.
(define firsts
    (lambda (l)
        (cond
            ((null? l) '())
            (else (cons (car (car l)) (firsts (cdr l)))))))

; insertR looks at a list of S-expressions, and compares each to the second provided S-expression. 
; If it finds a matching S-expression, it inserts the first provided S-expression after that 
; matching S-expression in the list.
(define insertR
    (lambda (new old lat)
        (cond
            ((null? lat) (quote ()))
            (else 
                (cond
                    ((eq? (car lat) old) (cons old (cons new (cdr lat))))
                    (else (cons (car lat) (insertR new old (cdr lat)))))))))

; insertL looks at a list of S-expressions, and compares each to the second provided S-expression. 
; If it finds a matching S-expression, it inserts the first provided S-expression before that 
; matching S-expression in the list.
(define insertL
    (lambda (new old lat)
        (cond
            ((null? lat) (quote ()))
            (else 
                (cond
                    ((eq? (car lat) old) (cons new lat))
                    (else (cons (car lat) (insertL new old (cdr lat)))))))))

; subst looks at a list of S-expressions, and compares each to the second provided S-expression. If 
; it finds a matching S-expression, it replaces that first occurrence of the matching S-expression 
; in the list with the first provided S-expression.
(define subst
    (lambda (new old lat)
        (cond
            ((null? lat) (quote ()))
            (else 
                (cond
                    ((eq? (car lat) old) (cons new (cdr lat)))
                    (else (cons (car lat) (subst new old (cdr lat)))))))))

; subst2 looks at a list of S-expressions, and compares each to the second and third provided 
; S-expressions. If it finds a matching S-expression, it replaces that first occurrence of the 
; matching S-expression in the list with the first provided S-expression.
(define subst2
    (lambda (new o1 o2 lat)
        (cond
            ((null? lat) (quote()))
            (else 
                (cond
                    (or ((eq? (car lat) o1) (cons new (cdr lat))) 
                        ((eq? (car lat) o2) (cons new (cdr lat))))
                    (else (cons (car lat) (subst2 new o1 o2 (cdr lat))))))))
)
; multirember looks at each S-expression in a list, in turn, and compares it to the S-expression 
; provided. Every time it discovers an S-expression in that list that matches the provided 
; S-expression, it removes that S-expression from the list.
(define multirember
    (lambda (a lat)
        (cond
            ((null? lat) (quote()))
            (else 
                (cond
                    ((eq? (car lat) a) (multirember a (cdr lat)))
                    (else (cons (car lat) (multirember a (cdr lat)))))))))

; multiinsertR looks at a list of S-expressions, and compares each to the second provided 
; S-expression. For each matching S-expression it finds, it inserts the first provided S-expression 
; after that matching S-expression in the list.
(define multiinsertR
    (lambda (new old lat)
        (cond
            ((null? lat) (quote ()))
            (else 
                (cond
                    ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
                    (else (cons old (multiinsertR new old (cdr lat)))))))))

; multiinsertL looks at a list of S-expressions, and compares each to the second provided 
; S-expression. For each matching S-expression it finds, it inserts the first provided S-expression 
; before that matching S-expression in the list.
(define multiinsertL
    (lambda (new old lat)
        (cond
            ((null? lat) (quote()))
            (else 
                (cond
                    ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
                    (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

; multisubst looks at a list of S-expressions, and compares each to the second provided 
; S-expression. For each matching S-expression it finds, it replaces that matching S-expression in 
; the list with the first provided S-expression.
(define multisubst
    (lambda (new old lat)
        (cond
            ((null? lat) (quote()))
            (else
                (cond
                    ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
                    (else (cons (car lat) (multisubst new old (cdr lat)))))))))
