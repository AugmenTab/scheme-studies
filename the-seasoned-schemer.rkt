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

;;--------------------------------------------------------------------------------------------------

; Chapter 12: Take Cover

; multirember-Y works exactly like multirember, except that it uses a Y-combinator.
(define multirember-Y
    (lambda (a lat)
        ((Y (lambda (mr)
            (lambda (lat)
                (cond
                    ((null? lat) (quote ()))
                    ((eq? a (car lat)) (mr (cdr lat)))
                    (else (cons (car lat) (mr (cdr lat))))))))
        lat)))

; length-Y works exactly like length, except that it uses a Y-combinator.
(define length-Y
    (Y (lambda (length)
        (lambda (l)
            (cond
                ((null? l) 0)
                (else (add1 (length-Y (cdr l)))))))))

; multirember-letrec works exactly like multirember, except that it uses letrec.
(define multirember-letrec
    (lambda (a lat)
        ((letrec
            ((mr (lambda (lat)
                (cond
                    ((null? lat) '())
                    ((eq? a (car lat)) (mr (cdr lat)))
                    (else (cons (car lat) (mr (cdr lat))))))))
                mr)
            lat)))

; multirember-f accepts a function test? and returns a new function, which takes an atom a and a
; list of atoms lat and traverses the latter. Any atom b in lat for which (test? a b) is true is
; removed.
(define multirember-f
    (lambda (test?)
        (letrec
            ((m-f
                (lambda (a lat)
                    (cond
                        ((null? lat) (quote ()))
                        ((test? (car lat) a) (m-f a (cdr lat)))
                        (else (cons (car lat) (m-f a (cdr lat))))))))
            m-f)))

; member?-letrec works exactly like member?, except that it uses letrec.
(define member?-letrec
    (lambda (a lat)
        (letrec
            ((yes? (lambda (l)
                (cond
                    ((null? l) #f)
                    ((eq? (car l) a) #t)
                    (else (yes? (cdr l)))))))
            (yes? lat))))

; union-letrec works exactly the same as union, except that it uses letrec. It defines another
; function U that cdrs down set, consing up all elements that are not a member of set2. Eventually U
; will cons all these elements onto set2. Second, union-letrec applies U to set1.
(define union-letrec
    (lambda (set1 set2)
        (letrec
            ((U (lambda (set)
                (cond
                    ((null? set) set2)
                    ((member? (car set) set2) (U (cdr set)))
                    (else (cons (car set) (U (cdr set))))))))
            (U set1))))

; member?-rev works exactly the same as member?, except that it accepts its arguments in reverse.
; That is, it accepts a lat first, and then an atom second.
(define member?-rev
    (lambda (lat a)
        (cond
            ((null? lat) #f)
            ((eq? (car lat) a) #t)
            (else (member?-rev (cdr lat) a)))))

; union-letrec-2 assumes the above version of member? is the default.
(define union-letrec-2
    (lambda (set1 set2)
        (letrec
            ((U (lambda (set)
                (cond
                    ((null? set) set2)
                    ((M? (car set) set2) (U (cdr set)))
                    (else (cons (car set) (U (cdr set)))))))
            (M? (lambda (a lat)
                (letrec
                    ((N? (lambda (lat)
                        (cond
                            ((null? lat) #f)
                            ((eq? (car lat) a) #t)
                            (else (N? (cdr lat)))))))
                (N? lat)))))
            (U set1))))

; two-in-a-row-b? works exactly the same as two-in-a-row?, except that we have hidden it like the
; other functions in this chapter.
(define two-in-a-row-b?
    (letrec
        ((W (lambda (a lat)
            (cond
                ((null? lat) #f)
                (else (or (eq? (car lat) a) (W (car lat) (cdr lat))))))))
        (lambda (lat)
            (cond
                ((null? lat) #f)
                (else (W (car lat) (cdr lat)))))))

; sum-of-prefixes-b works exactly the same as sum-of-prefixes, except that we have hidden it like
; the other functions in this chapter.
(define sum-of-prefixes-b
    (lambda (tup)
        (letrec
            ((S (lambda (sss tup)
                (cond
                    ((null? tup) (quote ()))
                    (else (cons (o+ sss (car tup)) (S (o+ sss (car tup)) (cdr tup))))))))
            (S 0 tup))))

; scramble-b works exactly the same as sum-of-prefixes, except that we have hidden it like the
; other functions in this chapter.
(define scramble-b
    (lambda (tup)
        (letrec
            ((P (lambda (tup rp)
                (cond
                    ((null? tup) (quote ()))
                    (else 
                        (cons 
                            (pick (car tup) (cons (car tup) rp))
                            (P (cdr tup) (cons (car tup) rp))))))
            (P tup (quote ())))))
