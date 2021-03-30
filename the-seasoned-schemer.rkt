#lang racket

;;--------------------------------------------------------------------------------------------------

; Chapter 11: Welcome Back to the Show

; two-in-a-row? determines whether any atom occurs twice in a row in a list of atoms.
(define two-in-a-row?
    (lambda (lat)
        (cond
            ((null? lat) #f)
            (else (or (is-first? (car lat) (cdr lat)) (two-in-a-row? (cdr lat)))))))

; is-first? determines if an atom and the car of a lat are equal to one another.
(define is-first?
    (lambda (a lat)
        (cond
            ((null? lat) #f)
            (else (eq? (car lat) a)))))

; two-in-a-row-revised? is a revised version of two-in-a-row?.
(define two-in-a-row-revised?
    (lambda (lat)
        (cond
            ((null? lat) #f)
            (else is-first-b? (car lat) (cdr lat)))))

; is-first-b? is a revised version of is-first?.
(define is-first-b?
    (lambda (a lat)
        (cond
            ((null? lat) #f)
            (else (or (eq? (car lat) a) (two-in-a-row-b? lat))))))

; two-in-a-row-b? is a revised version of two-in-a-row?.
(define two-in-a-row-b?
    (lambda (preceding lat)
        (cond
            ((null? lat) #f)
            (else (or (eq? (car lat) preceding) (two-in-a-row-b? (car lat) (cdr lat)))))))

; two-in-a-row-final is a revised version of two-in-a-row?.
(define two-in-a-row-final?
    (lambda (lat)
        (cond
            ((null? lat) #f)
            (else (two-in-a-row-b? (car lat) (cdr lat))))))

; sum-of-prefixes takes a tuple as an argument, and creates a tuple consisting of the sum of all 
; previous numbers in the original tuple up to that point.
(define sum-of-prefixes
    (lambda (tup)
        (sum-of-prefixes-b 0 tup)))

; sum-of-prefixes-b
(define sum-of-prefixes-b
    (lambda (sonssf tup)
        (cond
            ((null? tup) (quote ()))
            (else (cons (o+ sonssf (car tup)) 
                (sum-of-prefixes-b (o+ sonssf (car tup)) (cdr tup)))))))

; scramble takes a non-empty tup in which no number is greater than its own index, and returns a tup
; of the same length. Each number in the argument is treated as a backward index from its own 
; position to a point earlier in the tup. The result at each position is found by counting backward 
; from the current position according to this index.
(define scramble
    (lambda (tup)
        (scramble-b tup (quote ()))))

; scramble-b receives a tup and the reverse of its proper prefix. If the tup is empty, it returns 
; the empty list. Otherwise, it constructs the reverse of the complete prefix and uses the first 
; element of tup as a backward index into this list. It then processes the rest of the tup and 
; conses the two results together.
(define scramble-b
    (lambda (tup rev-prev)
        (cond
            ((null? tup) (quote ()))
            (else (cons (pick (car tup) (cons (car tup) rev-prev)) 
                (scramble-b (cdr tup) (cons (car tup) rev-prev)))))))
                