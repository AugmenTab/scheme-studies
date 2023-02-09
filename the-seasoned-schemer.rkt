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
                    ((member?-letrec (car set) set2) (U (cdr set)))
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

;;--------------------------------------------------------------------------------------------------

; Chapter 13: Hop, Skip, and Jump

; intersect-letrec works exactly like intersect, except that we have hidden it like the functions in
; chapter 12.
(define intersect-letrec
    (lambda (set1 set2)
        (letrec
            ((I (lambda (set 1)
                (cond
                    ((null? set1) (quote ()))
                    ((member?-rev (car set1) set2) (cons (car set1) (I (cdr set1))))
                    (else (I (cdr set1)))))))
            (cond
                ((null? set2) (quote ()))
                (else (I set1))))))

; intersectall-letrec works exactly like intersectall, except that we have hidden it like the
; functions in chapter 12.
(define intersectall-letrec
    (lambda (lset)
        (letrec
            ((A (lambda (lset)
                (cond
                    ((null? (cdr lset)) (car lset))
                    (else (intersect-letrec (car lset) (A (cdr lset))))))))
            (cond
                ((null? lset) (quote ()))
                (else (A lset))))))

; intersectall-letcc works exactly like intersectall-letrec, except that it is using letcc to avoid
; having to intersect each set with the empty set if an empty set is present in lset.
(define intersectall-letcc
    (lambda (lset)
        (letcc hop
            (letrec
                ((A (lambda (lset)
                    (cond
                        ((null? (car lset)) (hop (quote ())))
                        ((null? (cdr lset) (car lset))
                        (else (intersect-letrec (car lset) (A (cdr lset))))))))
                (cond
                    ((null? lset) (quote ()))
                    (else (A lset))))))))

; intersectall-minor works exactly like intersectall-letcc, except that it defines intersect-letrec
; as a minor function inside.
(define intersectall-minor
    (lambda (lset)
        (letcc hop
            (letrec
                ((A (lambda (lset)
                    (cond
                        ((null? (car lset)) (hop (quote ())))
                        ((null? (cdr lset)) (car lset))
                        (else (I (car lset))))))
                (I (lambda (s1 s2)
                    (letrec
                        ((J (lambda (s1)
                            (cond
                                ((null? s1) (quote ()))
                                ((member?-rev (car s1) s2) (J (cdr s1)))
                                (else (cons (car s1) (J (cdr s1))))))))
                        (cond
                            ((null? s2) (hop (quote ())))
                            (else (J s1)))))))
                (cond
                    ((null? lset) (quote ()))
                    (else (A lset)))))))

; rember-letrec works exactly like rember, except that we have hidden it like the functions in
; chapter 12.
(define rember-letrec
    (lambda (a lat)
        (letrec
            ((R (lambda (lat)
                (cond
                    ((null? lat) (quote ()))
                    ((eq? (car lat) a) (cdr lat))
                    (else (cons (car lat) (R (cdr lat))))))))
            (R lat))))

; rember-beyond-first takes an atom a and a lat and, if a occurs in the lat, removes all atoms from
; the lay beyond and including the first occurrence of a.
(define rember-beyond-first
    (lambda (a lat)
        (letrec
            ((R (lambda (lat)
                (cond
                    ((null? lat) (quote ()))
                    ((eq? (car lat) a) (quote ()))
                    (else (cons (car lat) (R (cdr lat))))))))
            (R lat))))

; rember-upto-last takes an atom a and a lat and removes all the atoms from the lat up to and
; including the last occurrence of a. If there are no occurrences of a, it returns the lat.
(define rember-upto-last
    (lambda (a lat)
        (letcc skip
            (letrec
                ((R (lambda (lat)
                    (cond
                        ((null? lat) (quote ()))
                        ((eq? (car lat) a) (skip (R (cdr lat))))
                        (else (cons (car lat) (R (cdr lat))))))))
                (R lat)))))

;;--------------------------------------------------------------------------------------------------

; Chapter 14: Let There Be Names

; leftmost* works exactly like leftmost, except that where leftmost finds the leftmost atom in a
; non-empty list of S-expressions that does not contain the empty list, leftmost* is able to find
; the leftmost atom in a non-empty list of S-expressions without restricting the shape of its
; argument.
(define leftmost*
    (lambda (l)
        (cond
            ((null? l) (quote ()))
            ((atom? (car l)) (car l))
            (else
                (let ((a (leftmost* (car l))))
                    (cond
                        ((atom? a) a)
                        (else (leftmost* (cdr l)))))))))

; rember1* takes an atom a and a list of S-expressions l, and goes through the list. When there is a
; list in the car, it attempts to remove a from the car. If the car remains the same, a is not in
; the car, rember1* must continue. When rember1* finds an atom in the list, and the atom is equal to
; a, it is removed.
(define rember1*
    (lambda (a l)
        (letrec
            ((R (lambda (l)
                (cond
                    ((null? l) (quote ()))
                    ((atom? (car l))
                        (cond
                            ((eq? (car l) a) (cdr l))
                            (else (cons (car l) (R (cdr l))))))
                    (else
                        (let ((av (R (car l))))
                            (cond
                                ((eqlist? (car l) av) (cons (car l) (R (cdr l))))
                                (else (cons av (cdr l))))))))))
            (R l))))

; depth* finds the maximum depth of an S-expression.
(define depth*
    (lambda (l)
        (cond
            ((null? l) 1)
            ((atom? (car l)) (depth* (cdr l)))
            (else
                (let ((a (add1 (depth* (car l)))) (d (depth* (cdr l))))
                    (if (o> d a) d a))))))

; max returns the larger of the two numbers passed to it as its arguments.
(define max
    (lambda (n m)
        (if (o> n m) n m)))

; depth*-max works exactly the same as depth*, except that it uses max as a helper function.
(define depth*-max
    (lambda (l)
        (cond
            ((null? l) 1)
            ((atom? (car l)) (depth*-max (cdr l)))
            (else (max (add1 (depth*-max (car l))) (depth*-max (cdr l)))))))

; scramble-let works exactly the same as scramble-b, except that it uses let.
(define scramble-let
    (lambda (tup)
        (letrec
            ((P (lambda (tup rp)
                (cond
                    ((null? tup) (quote ()))
                    (else
                        (let ((rp (cons (car tup) rp)))
                            (cons (pick (car tup) rp) (P (cdr tup) rp))))))))
            (P tup (quote ())))))

; leftmost-lm works exactly like leftmost, except that it uses a helper function lm to help it skip
; unnecessary steps.
(define leftmost-lm
    (lambda (l)
        (letcc skip
            (lm l skip))))

; lm serves as a helper function for leftmost-lm.
(define lm
    (lambda (l out)
        (cond
            ((null? l) (quote ()))
            ((atom? (car l)) (out (car l)))
            (else (let ()
                (lm (car l) out)
                (lm (cdr l) out))))))

; leftmost-hidden works exactly like leftmost, except that it hides the helper function lm inside.
; It determines the values of (lm l), then looks at every atom in l from left to right until it
; finds an atom, and then uses skip to return this atom abruptly and promptly.
(define leftmost-hidden
    (lambda (l)
        (letcc skip
            (letrec
                ((lm (lambda (l)
                    (cond
                        ((null? l) (quote ()))
                        ((atom? (car l)) (skip (car l)))
                        (else
                            (let ()
                                (lm (car l))
                                (lm (cdr l))))))))
                (lm l)))))

; rm
(define rm
    (lambda (a l oh)
        (cond
            ((null? l) (oh (quote no)))
            ((atom? (car l))
                (if (eq? (car l) a) (cdr l) (cons (car l) (rm a (cdr l) oh))))
            (else
                (let ((new-car
                    (letcc oh (rm a (car l) oh))))
                        (if (atom? new-car) (cons (car l) (rm a (cdr l) oh))
                            (cons new-car (cdr l))))))))

; rember1*-rm works exactly the same as rember1*, except that it uses rm as a helper function.
(define rember1*-rm
    (lambda (a l)
        (let ((new-l (letcc oh (rm a l oh))))
            (if (atom? new-l)
                l
                new-l))))

; rember1*-try works exactly the same as rember1*, except that it uses try.
(define rember1*-try
    (lambda (a l)
        (try oh (rm a l oh) l)))

; rm-try works exactly like rm, except that it uses try.
(define rm-try
    (lambda (a l oh)
        (cond
            ((null? l) (oh (quote no)))
            ((atom? (car l))
                (if (eq? (car l) a)
                    (cdr l)
                    (cons (car l) (rm a (cdr l) oh))))
            (else
                (try oh2
                    (cons (rm a (car l) oh2) (cdr l))
                    (cons (car l) (rm a (cdr l) oh)))))))

;;--------------------------------------------------------------------------------------------------

; Chapter 15: The Difference Between Men and Boys...

;
(set! x (quote rings))  ; First instance in chapter; set to other values for other examples.

(define gourmet
    (lambda (food)
        (cons food (cons x (quote ())))))

(define gourmand
    (lambda (food)
        (set! x food)
        (cons food (cons x (quote ())))))

(define diner
    (lambda (food)
        (cons (quote milkshake) (cons food (quote ())))))

(define dinerR
    (lambda (food)
        (set! x food)
        (cons (quote milkshake) (cons food (quote ())))))

(define omnivore
    (let ((x (quote minestrone)))
        (lambda (food)
            (set! x food)
            (cons food (cons x (quote ()))))))

(define gobbler
    (let ((x (quote minestrone)))
        (lambda (food)
            (set! x food)
            (cons food (cons x (quote ()))))))

(define nibbler
    (lambda (food)
        (let ((x (quote donut)))
            (set! x food)
            (cons food (cons x (quote ()))))))

(define food (quote none))

(define glutton
    (lambda (x)
        (set! food x)
        (cons (quote more) (cons x (cons (quote more) (cons x (quote ())))))))

(define chez-nous
    (lambda ()
        (let ((a food))
            (set! food x)
            (set! x a))))

;;--------------------------------------------------------------------------------------------------

; Chapter 16: Ready, Set, Bang!

(define sweet-toothL
    (lambda (food)
        (set! last food)
        (cons food (cons (quote cake) (quote ())))))

(define sweet-toothR
    (lambda (food)
        (set! ingredients (cons food ingredients))
        (cons food (cons (quote cake) (quote ())))))

(define deep
    (lambda (m)
        (cond
            ((zero? m) (quote pizza))
            (else (cons (deepM (sub1 m)) (quote ()))))))

(define Ns (quote ()))
(define Rs (quote ()))

(define deepR
    (lambda (n)
        (let ((result (deep n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)))

(define find
    (lambda (n Ns Rs)
        (letrec
            ((A (lambda (ns rs)
                (cond
                    ((null? ns) #f)
                    ((o= (car ns) n) (car rs))
                    (else (A (cdr ns) (cdr rs)))))))
             (A Ns Rs))))

(define deepM
    (let ((Rs (quote ()))
          (Ns (quote ())))
        (lambda (n)
            (let ((exists (find n Ns Rs)))
                (if (atom? exists)
                    (let ((result (deep n)))
                        (set! Rs (cons result Rs))
                        (set! Ns (cons n Ns))
                        result)
                    exists)))))

(set! Ns (cdr Ns))
(set! Rs (cdr Rs))

(define L
    (lambda (length)
        (lambda (l)
            (cond
                ((null? l) 0)
                (else (add1 (length (cdr l))))))))

(define length
    (let ((h (lambda (l) 0)))
        (set! h (L (lambda (arg) (h arg))))
        h))

(define Y!
    (lambda (L)
        (let ((h (lambda (l) (quote ()))))
            (set! h (L (lambda (arg) (h arg))))
            h)))

(define Y-bang
    (lambda (f)
        (letrec
            ((h (f (lambda (arg) (h arg)))))
            h)))

(define length (Y! L))

(define D
    (lambda (depth*)
        (lambda (s)
            (cond
                ((null? s) 1)
                ((atom? (car s)) (depth* (cdr s)))
                (else (max (add1 (depth* (car s))) (depth* (cdr s))))))))

(define depth* Y! D)

(define biz
    (let ((x 0))
        (lambda (f)
            (set! x (add1 x))
            (lambda (a)
                (if (o= a x) 0 (f a))))))

;;--------------------------------------------------------------------------------------------------

; Chapter 17: We Change, Therefore We Are!

(define deep-if
    (lambda (m)
        (if (zero? m)
            (quote pizza)
            (cons (deep-if (sub1 m)) (quote ())))))

(define deepM
    (let ((Rs (quote ()))
          (Ns (quote ())))
        (lambda (n)
            (let ((exists (find n Ns Rs)))
                (if (atom? exists)
                    (let ((result
                            (if (zero? n)
                                (quote pizza)
                                (cons (deepM (sub1 n)) (quote ())))))
                        (set! Rs (cons result Rs))
                        (set! Ns (cons n Ns))
                        result)
                    exists)))))

(define counter)

(define consC
    (let ((N 0))
        (set! counter (lambda () N))
        (lambda (x y)
            (set! N (add1 N))
            (cons x y))))

(define deep
    (lambda (m)
        (if (zero? m)
            (quote pizza)
            (consC (deep (sub1 m)) (quote ())))))

(define supercounter
    (lambda (f)
        (letrec
            ((S (lambda (n)
                (if (zero? n)
                    (f n)
                    (let ()
                        (f n)
                        (S (sub1 n)))))))
            (S 1000)
            (counter))))

(define counter)
(define set-counter)
(define consC
    (let ((N 0))
        (set! counter (lambda () N))
        (set! set-counter
            (lambda (x)
                (set! N x)))
        (lambda (x y)
            (set! N (add1 N))
            (cons x y))))

(define deepM
    (let ((Rs (quote ()))
          (Ns (quote ())))
        (lambda (n)
            (let ((exists (find n Ns Rs)))
                (if (atom? exists)
                    (let ((result
                            (if (zero? n)
                                (quote pizza)
                                (consC (deepM (sub1 n)) (quote ())))))
                        (set! Rs (cons result Rs))
                        (set! Ns (cons n Ns))
                        result)
                    exists)))))

(define rember1*C
    (lambda (a l)
        (letrec
            ((R (lambda (l oh)
                (cond
                    ((null? l) (oh (quote no)))
                    ((atom? (car l))
                        (if (eq? (car l) a)
                            (cdr l)
                            (consC (car l) (R (cdr l) oh))))
                    (else
                        (let ((new-car
                                (letrec oh
                                    (R (car l) oh))))
                            (if (atom? new-car)
                                (consC (car l) (R (cdr l) oh))
                                (consC new-car (cdr l)))))))))
            (let ((new-l (letcc oh (R l oh))))
                (if (atom? new-l)
                    l
                    new-l)))))

(define rember1*C2
    (lambda (a l)
        (letrec
            ((R (lambda (l)
                (cond
                    ((null? l) (quote ()))
                    ((atom? (car l))
                        (if (eq? (car l) a)
                            (cdr l)
                            (consC (car l) (R (cdr l)))))
                    (else
                        (let ((av (R (car l))))
                            (if (eqlist? (car l) av)
                                (consC (car l) (R (cdr l)))
                                (consC av (cdr l)))))))))
            (R l))))

;;--------------------------------------------------------------------------------------------------

; Chapter 18: We Change, Therefore We Are the Same!

(define lots
    (lambda (m)
        (cond
            ((zero? m) (quote ()))
            (else kons (quote egg) (lots (sub1 m))))))

(define lenkth
    (lambda (l)
        (cond
            ((null? l) 0)
            (else (add1 (lenkth (kdr l)))))))

(define add-at-end
    (lambda (l)
        (cond
            ((null? (kdr l)) (konsC (kar l) (kons (quote egg) (quote ()))))
            (else (konsC (kar l) (add-at-end (kdr l)))))))

(define add-at-end-too
    (lambda (l)
        (letrec
            ((A (lambda (ls)
                (cond
                    ((null? (kdr ls)) (set-kdr ls (kons (quote egg) (quote ())))))
                    (else (A (kdr ls))))))
            (A l)
            l)))

(define kons
    (lambda (a d)
        (let ((c (bons a)))
            (set-kdr c d)
            c)))

(define kar
    (lambda (c)
        (c (lambda (s a d) a))))

(define kdr
    (lambda (c)
        (c (lambda (s a d) d))))

(define bons
    (lambda (kar)
        (let ((kdr (quote ())))
            (lambda (selector)
                (selector (lambda (x) (set! kdr x)) kar kdr)))))

(define eklist?
    (lambda (ls1 ls2)
        (cond
            ((null? ls1) (null? ls2))
            ((null? ls2) #f)
            (else
                (and (eq? (kar ls1) (kar ls2)) (eklist? (kdr ls1) (kdr ls2)))))))

(define same?
    (lambda (c1 c2)
        (let ((t1 (kdr c1))
              (t2 (kdr c2)))
            (set-kdr c1 1)
            (set-kdr c2 2)
            (let ((v (o= (kdr c1) (kdr c2))))
                (set-kdr c1 t1)
                (set-kdr c2 t2)
                v))))

(define last-kons
    (lambda (ls)
        (cond
            ((null? (kdr ls)) ls)
            (else (last-kons (kdr ls))))))

(define finite-lenkth
    (lambda (p)
        (letcc infinite
            (letrec
                ((C (lambda (p q)
                    (cond
                        ((same? p q) (infinite #f))
                        ((null? q) 0)
                        ((null? (kdr q)) 1)
                        (else (o+ (C (sl p) (qk q)) 2)))))
                    (qk (lambda (x) (kdr (kdr x))))
                    (sl (lambda (x) (kdr x))))
                (cond
                    ((null? p) 0)
                    (else (add1 (C p (kdr p)))))))))

(define mongo
    (kons (quote pie)
        (kons (quote à)
            (kons (quote la)
                (kons (quote mode)
                    (quote ()))))))
(set-kdr (kdr (kdr (kdr mongo))) (kdr mongo))

;;--------------------------------------------------------------------------------------------------

; Chapter 19: Absconding with the Jewels

(define six-layers
    (lambda (p)
        (cons
            (cons
                (cons
                    (cons
                        (cons
                            (cons p (quote ()))
                            (quote ()))
                        (quote ()))
                    (quote ()))
                (quote ()))
            (quote ()))))

(define four-layers
    (lambda (p)
        (cons
            (cons
                (cons
                    (cons (quote ()))
                (quote ()))
            (quote ()))
        (quote ()))))

(define toppings)

(define deepB
    (lambda (m)
        (cond
            ((zero? m)
             (letcc jump
                (set! toppings jump)
                (quote pizza)))
            (else (cons (deepB (sub1 m)) (quote ()))))))

(define deep&co
    (lambda (m k)
        (cond
            ((zero? m) (k (quote pizza)))
            (else
                (deep&co (sub1 m)
                    (lambda (x)
                        (k (cons x (quote ())))))))))

(define two-layers
    (lambda (p)
        (cons
            (cons p (quote ()))
            (quote ()))))

(define deep&coB
    (lambda (m k)
        (cond
            ((zero? m)
             (let ()
                (set! toppings k)
                (k (quote pizza))))
            (else
                (deep&coB (sub1 m)
                    (lambda (x)
                        (k (cons x (quote ())))))))))

(define leave)

(define walk
    (lambda (l)
        (cond
            ((null? l) (quote ()))
            ((atom? (car l)) (leave (car l)))
            (else
                (let ()
                    (walk (car l))
                    (walk (cdr l)))))))

(define start-it
    (lambda (l)
        (letcc here
            (set! leave here)
            (walk l))))

(define fill)

(define waddle
    (lambda (l)
        (cond
            ((null? l) (quote ()))
            ((atom? (car l))
                (let ()
                    (letcc rest
                        (set! fill rest)
                        (leave (car l)))
                    (waddle (cdr l))))
            (else (let ()
                (waddle (car l))
                (waddle (cdr l)))))))

(define start-it2
    (lambda (l)
        (letcc here
            (set! leave here)
            (waddle l))))

(define get-next
    (lambda (x)
        (letcc here-again
            (set! leave here-again)
            (fill (quote go)))))

(define get-first
    (lambda (l)
        (letcc here
            (set! leave here)
            (waddle l)
            (leave (quote ())))))

(define two-in-a-row*?
    (lambda (l)
        (let ((fst (get-first l)))
            (if (atom? fst)
                (two-in-a-row-b*? fst)
                (#f)))))

(define two-in-a-row-b*?
    (lambda (a)
        (let ((n (get-next (quote go))))
            (if (atom? a)
                (or (eq? n a) (two-in-a-row-b*? n))
                #f))))

(define two-in-a-row-2*?
    (letrec
        ((T? (lambda (a)
            (let ((n (get-next 0)))
                (if (atom? n)
                    (or (eq? n a) (T? n))
                    #f))))
        (get-next
            (lambda (x)
                (letcc here-again)
                    (set! leave here-again)
                    (fill (quote go)))))
        (fill (lambda (x) x))
        (waddle
            (lambda (l)
                (cond
                    ((null? l) (quote ()))
                    ((atom? (car l))
                     (let ()
                        (letcc rest
                            (set! fill rest)
                            (leave (car l)))
                        (waddle (cdr l))))
                    (else (let ()
                        (waddle (car l))
                        (waddle (cdr l)))))))
        (lambda (l)
            (let ((fst (letcc here
                    (set! leave here)
                    (waddle l)
                    (leave (quote ())))))
                (if (atom? fst) (T? fst) #f)))))

;;--------------------------------------------------------------------------------------------------

; Chapter 20: What's in Store?

(define lookup
    (lambda (table name)
        (table name)))

(define extend
    (lambda (name1 value table)
        (lambda (name2)
            (cond
                ((eq? name2 name1) value)
                (else (table name2))))))

(define define?
    (lambda (e)
        (cond
            ((atom? e) #f)
            ((atom? (car e)) (eq? (car e) (quote define)))
            (else #f))))

(define global-table)

(define *define
    (lambda (e)
        (set! global-table
            (extend
                (name-of e)
                (box (the-meaning (right-side-of e))) global-table))))

(define box
    (lambda (it)
        (lambda (sel)
            (sel it (lambda (new)
                        (set! it new))))))

(define setbox
    (lambda (box new)
        (box (lambda (it set) (set new)))))

(define unbox
    (lambda (box)
        (box (lambda (it set) it))))

(define the-meaning
    (lambda (e)
        (meaning e lookup-in-global-table)))

(define lookup-in-global-table
    (lambda (name)
        (lookup global-table name)))

(define meaning
    (lambda (e table)
        ((expression-to-action e) e table)))

(define *quote
    (lambda (e table)
        (text-of e)))

(define *identifier
    (lambda (e table)
        (unbox (lookup table e))))

(define *set
    (lambda (e table)
        (setbox
            (lookup table (name-of e))
            (meaning (right-side-of e) table))))

(define *lambda
    (lambda (e table)
        (lambda (args)
            (beglis (body-of e)
                (multi-extend
                    (formals-of e)
                    (box-all args)
                    table)))))

(define beglis
    (lambda (es table)
        (cond
            ((null? (cdr es)) (meaning (car es) table))
            (else ((lambda (val) (beglis (cdr es) table))
                   (meaning (car es) table))))))

(define box-all
    (lambda (vals)
        (cond
            ((null? vals) (quote ()))
            (else (cons (box (car vals)) (box-all (cdr vals)))))))

(define multi-extend
    (lambda (names values table)
        (cond
            ((null? names) table)
            (else
                (extend (car names) (car values)
                    (multi-extend
                        (cdr names)
                        (cdr values)
                        table))))))

(define odd?
    (lambda (n)
        (cond
            ((zero? n) #f)
            (else (even? (sub1 n))))))

(define even?
    (lambda (n)
        (cond
            ((zero? n) #t)
            (else (odd? (sub1 n))))))

(define *application
    (lambda (e table)
        ((meaning (function-of e) table)
         (evlis (arguments-of e) table))))

(define evlis
    (lambda (args table)
        (cond
            ((null? args) (quote ()))
            (else
                ((lambda (val)
                    (cons val (evlis (cdr args) table)))
                 (meaning (car args) table))))))

(define :car
    (lambda (args-in-a-list)
        (car (car args-in-a-list))))

(define a-prim
    (lambda (p)
        (lambda (args-in-a-list)
            (p (car args-in-a-list)))))

(define b-prim
    (lambda (p)
        (lambda (args-in-a-list)
            (p (car args-in-a-list)
               (car (cdr args-in-a-list))))))

(define *const
    (lambda (e table)
        (cond
            ((number? e) e)
            ((eq? e #t) #t)
            ((eq? e #f) #f)
            ((eq? e (quote cons)) (b-prim cons))
            ((eq? e (quote car)) (a-prim cons))
            ((eq? e (quote cdr)) (a-prim cdr))
            ((eq? e (quote eq?)) (b-prim eq?))
            ((eq? e (quote atom?)) (a-prim atom?))
            ((eq? e (quote null?)) (a-prim null?))
            ((eq? e (quote zero?)) (a-prim zero?))
            ((eq? e (quote add1)) (a-prim add1))
            ((eq? e (quote sub1)) (a-prim sub1))
            ((eq? e (quote number?)) (a-prim number?)))))

(define *const-let
    (let ((:cons (b-prim cons))
          (:car (a-prim car))
          (:cdr (a-prim cons))
          (:null? (a-prim null?))
          (:eq? (b-prim eq?))
          (:atom? (a-prim atom?))
          (:zero? (a-prim zero?))
          (:add1 (a-prim add1))
          (:sub1 (a-prim sub1))
          (:number? (a-prim number?)))
        (lambda (e table)
            (cond
                ((number? e) e)
                ((eq? e #t) #t)
                ((eq? e #f) #f)
                ((eq? e (quote cons)) :cons)
                ((eq? e (quote car)) :car)
                ((eq? e (quote cdr)) :cdr)
                ((eq? e (quote null?)) :null?)
                ((eq? e (quote eq?)) :eq?)
                ((eq? e (quote atom?)) :atom?)
                ((eq? e (quote zero?)) :zero?)
                ((eq? e (quote add1)) :add1)
                ((eq? e (quote sub1)) :sub1)
                ((eq? e (quote number?)) :number?)))))

(define *const-no-let
    ((lambda (:cons :car :cdr :null? :eq? :atom? :zero? :add1 :sub1 :number?)
        (lambda (e table)
            (cond
                ((number? e) e)
                ((eq? e #t) #t)
                ((eq? e #f) #f)
                ((eq? e (quote cons)) :cons)
                ((eq? e (quote car)) :car)
                ((eq? e (quote cdr)) :cdr)
                ((eq? e (quote null?)) :null?)
                ((eq? e (quote eq?)) :eq?)
                ((eq? e (quote atom?)) :atom?)
                ((eq? e (quote zero?)) :zero?)
                ((eq? e (quote add1)) :add1)
                ((eq? e (quote sub1)) :sub1)
                ((eq? e (quote number?)) :number?))))
     (b-prim cons)
     (a-prim car)
     (a-prim cons)
     (a-prim null?)
     (b-prim eq?)
     (a-prim atom?)
     (a-prim zero?)
     (a-prim add1)
     (a-prim sub1)
     (a-prim number?)))

(define *cond
    (lambda (e table)
        (evcon (cond-lines-of e) table)))

(define evcon
    (lambda (lines table)
        (cond
            ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
            ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
            (else (evcon (cdr lines) table)))))

(define *letcc
    (lambda (e table)
        (letcc skip
            (beglis (ccbody-of e)
                (extend
                    (name-of e)
                    (box (a-prim skip))
                    table)))))

(define abort)

(define value
    (lambda (e)
        (letcc the-end
            (set! abort the-end)
            (cond
                ((define? e) (*define e))
                (else (the-meaning e))))))

(define the-empty-table
    (lambda (name)
        (abort (cons (quote no-answer) (cons name (quote ()))))))

(define expression-to-action
    (lambda (e)
        (cond
            ((atom? e) (atom-to-action e))
            (else (list-to-action e)))))

(define atom-to-action
    (lambda (e)
        (cond
            ((number? e) *const)
            ((eq? e #t) *const)
            ((eq? e #f) *const)
            ((eq? e (quote cons)) *const)
            ((eq? e (quote car)) *const)
            ((eq? e (quote cdr)) *const)
            ((eq? e (quote null?)) *const)
            ((eq? e (quote eq?)) *const)
            ((eq? e (quote atom?)) *const)
            ((eq? e (quote zero?)) *const)
            ((eq? e (quote add1)) *const)
            ((eq? e (quote sub1)) *const)
            ((eq? e (quote number?)) *const)
            (else *identifier))))

(define list-to-action
    (lambda (e)
        (cond
            ((atom? (car e))
                (cond
                    ((eq? (car e) (quote quote)) *quote)
                    ((eq? (car e) (quote lambda)) *lambda)
                    ((eq? (car e) (quote letcc)) *letcc)
                    ((eq? (car e) (quote set!)) *set!)
                    ((eq? (car e) (quote cond)) *cond)
                    (else *application)))
            (else *application))))

(define text-of
    (lambda (x)
        (car (cdr x))))

(define formals-of
    (lambda (x)
        (car (cdr x))))

(define body-of
    (lambda (x)
        (cdr (cdr x))))

(define ccbody-of
    (lambda (x)
        (cdr (cdr x))))

(define name-of
    (lambda (x)
        (car (cdr x))))

(define right-side-of
    (lambda (x)
        (cond
            ((null? (cdr (cdr x))) 0)
            (else (car (cdr (cdr x)))))))

(define cond-lines-of
    (lambda (x)
        (cdr x)))

(define else?
    (lambda (x)
        (cond
            ((atom? x) (eq? x (quote else)))
            (else #f))))

(define question-of
    (lambda (x)
        (car x)))

(define answer-of
    (lambda (x)
        (car (cdr x))))

(define function-of
    (lambda (x)
        (car x)))

(define arguments-of
    (lambda (x)
        (cdr x)))
