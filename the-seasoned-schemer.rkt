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
