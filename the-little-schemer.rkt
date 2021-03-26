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
; S-expression, it removes that S-expression from the list. Simplified in chapter 5.
(define rember
    (lambda (s l)
        (cond
            ((null? l) (quote ()))
            ((equal? (car l) s) (cdr l))
            (else (cons (car l) (rember s (cdr l)))))))

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

;;--------------------------------------------------------------------------------------------------

; Chapter 4: Numbers Games

; o+ takes two numbers as arguments, and reduces the second until it hits zero. It adds one to the 
; first number as many times as it reduced the second one in order to reach zero, thus adding the 
; two numbers together.
(define o+
    (lambda (n m)
        (cond
            ((zero? m) n)
            (else (add1 (o+ n (sub1 m)))))))

; o- takes two numbers as arguments, and reduces the second until it hits zero. It subtracts one 
; from the first number as many times as it did to cause the second one to reach zero, thus 
; subtracting the second number from the first one.
(define o-
    (lambda (n m)
        (cond
            ((zero? m) n)
            (else (sub1 (o- n (sub1 m)))))))

; addtup builds a number by totaling all the numbers in a tup.
(define addtup
    (lambda (tup)
        (cond
            ((null? tup) 0)
            (else o+ (car tup) (addtup (cdr tup))))))

; o* takes two numbers as arguments, and reduces the second until it hits zero. It adds the first 
; number to the first number as many times as it reduced the second one in order to reach zero, thus
; multiplying the two numbers together.
(define o*
    (lambda (n m)
        (cond
            ((zero? m) 0)
            (else (o+ n (o* n (sub1 m)))))))

; tup+ takes two tups as arguments, and adds the first number of tup1 to the first number of tup2. 
; Then, it adds the second number of tup1 to the second number of tup2, and so on, building a tup of
; the answers, for tups of the same length.
(define tup+
    (lambda (tup1 tup2)
        (cond
            ((null? tup1) tup2) 
            ((null? tup2) tup1)
            (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

; o> takes two numbers as arguments, and subtracts one from each of them until one equals zero. If 
; the first number is equal to zero, o> is false. If the second number is equal to zero, o> is true.
(define o>
    (lambda (n m)
        (cond
            ((zero? n) #f)
            ((zero? m) #t)
            (else (o> (sub1 n) (sub1 m))))))

; o< takes two numbers as arguments, and subtracts one from each of them until one equals zero. If 
; the second number is equal to zero, o< is false. If the first number is equal to zero, o< is true.
(define o<
    (lambda (n m)
        (cond
            ((zero? m) #f)
            ((zero? n) #t)
            (else (o< (sub1 n) (sub1 m))))))

; o= takes two numbers as arguments, and checks if either o> or o< is true when passed the two 
; arguments. If neither comes back true, then o= is true.
(define o=
    (lambda (n m)
        (cond
            ((o> n m) #f)
            ((o< n m) #f)
            (else #t))))  ; maybe (cond (not (or (((o> n m) #f) ((o< n m) #f)))))?

; o^ takes two numbers as arguments, and reduces the second until it hits zero. If multiplies the 
; first number by the first number as many times as it reduced the second one in order to reach 
; zero, thus raising the first number to the power of the second number.
(define o^
    (lambda (n m)
        (cond
            ((zero? m) 1)
            (else (o* n (o^ n (sub1 m)))))))

; o/ takes two numbers as arguments, and checks how many times the second number fits into the first
; number, thus dividing the first number by the second number and providing the quotient.
(define o/
    (lambda (n m)
        (cond
            ((o< n m) 0)
            (else (add1 (o/ (o- n m) m))))))

; length takes a list as its argument, and counts the number of elements in that list.
(define length
    (lambda (lat)
        (cond
            ((null? lat) 0)
            (else (add1 (length (cdr lat)))))))

; pick takes in a number and a list as its arguments, and provides the member of that list that 
; appears in the position of the first argument.
(define pick
    (lambda (n lat)
        (cond
            ((zero? (sub1 n)) (car lat))
            (else (pick (sub1 n) (cdr lat))))))

; rempick takes in a number and a list as its arguments, and provides the list with the member of 
; that list that appears in the position of the first argument removed.
(define rempick
    (lambda (n lat)
        (cond
            ((one? n) (cdr lat))
            (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

; no-nums takes a list as its argument, which provides the original list with all of the numbers 
; removed.
(define no-nums
    (lambda (lat)
        (cond
            ((null? lat) (quote()))
            (else
                (cond
                    ((number? (car lat)) (no-nums (cdr lat)))
                    (else (cons (car lat) (no-nums (cdr lat)))))))))

; all-nums takes a list as its argument, and creates a tup by extracting all of the numbers from the
; original list.
(define all-nums
    (lambda (lat)
        (cond
            ((null? lat) (quote ()))
            (else
                (cond
                    ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
                    (else (all-nums (cdr lat))))))))

; eqan? takes in two atoms, and checks if they are the same atom.
(define eqan?
    (lambda (a1 a2)
        (cond
            ((and (number? a1) (number? a2)) (o= a1 a2))
            ((or (number? a1) (number? a2)) #f)
            (else (eq? a1 a2)))))

; occur takes in an atom and a list as its arguments, and counts the number of times that atom 
; appears in the list.
(define occur
    (lambda (a lat)
        (cond
            ((null? lat) 0)
            (else
                (cond
                    ((eq? (car lat) a) (add1 (occur a (cdr lat))))
                    (else (occur a (cdr lat))))))))

; one? takes a number as its argument, and checks if it is equal to zero if it is reduced by one.
(define one?
    (lambda (n)
        (o= n 1)))

;;--------------------------------------------------------------------------------------------------

; Chapter 5: *Oh My Gawd*: It's Full of Stars

; rember* works exactly like rember, except that it recurs down every list that is within the list 
; l.
(define rember*
    (lambda (a l)
        (cond
            ((null? l) (quote ())
            ((atom? a) (car l)
            (cond
                ((eq? (car l) a) (rember* a (cdr l)))
                (else (cons (car l) (rember* a (cdr l)))))))
            (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

; insertR* works exactly like insertR, except that it recurs down every list that is within the list
; l.
(define insertR*
    (lambda (new old l)
        (cond
            ((null? l) (quote ()))
            ((atom? (car l)) (cond
                ((eq? (car l) old) (cons old (cons new) (insertR* new old (cdr l)))
                (else (cons (car l) (insertR* new old (cdr l)))))))
            (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

; occur* works exactly like occur, except that it recurs down every list that is within the list l.
(define occur*
    (lambda (a l)
        (cond
            ((null? l) 0)
            ((atom? (car l)) (cond
                ((eq? (car l) a) (add1 (occur* a (cdr l))))
                (else (occur* a (cdr l)))))
            (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

; subst* works exactly like subst, except that it recurs down every list that is within the list l.
(define subst*
    (lambda (new old l)
        (cond
            ((null? l) (quote ()))
            ((atom? (car l)) (cond
                ((eq? (car l) old) (cons new (subst* new old (cdr l))))
                (else (cons (car l) subst* new old (cdr l)))))
            (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

; insertL* works exactly like insertL, except that it recurs down every list that is within the list
; l.
(define insertL*
    (lambda (new old l)
        (cond
            ((null? l) (quote ()))
            ((atom? (car l)) (cond
                ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
                (else (cons (car l) (insertL* new old (cdr l)))))
            (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

; member* works exactly like member, except that it recurs down every list that is within the list 
; l.
(define member*
    (lambda (a l)
        (cond
            ((null? l) #f)
            ((atom? (car l)) (or (eq? (car l) a) (member* a (cdr l))))
            (else (or (member* a (car l)) (member* a (cdr l)))))))

; leftmost finds the leftmost atom in a non-empty list of S-expressions that does not contain the 
; empty list.
(define leftmost
    (lambda (l)
        (cond
            ((atom? (car l) (car l))
            (else (leftmost (car l)))))))

; eqlist? checks if the two lists passed as arguments are equal to one another.
(define eqlist?
    (lambda (l1 l2)
        (cond
            ((and (null? l1) (null? l2)) #t)
            ((or (null? l1) (null? l2)) #f)
            (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

; equal? checks if the two S-expressions pass as arguments are equal to one another.
(define equal?
    (lambda (s1 s2)
        (cond
            ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
            ((or (atom? s1) (atom? s2)) #f)
            (else (eqlist? s1 s2)))))

;;--------------------------------------------------------------------------------------------------

; Chapter 6: Shadows

; numbered? determines whether a representation of an arithmetic expression contains only numbers 
; besides the o+, ox, and o^.
(define numbered?
    (lambda (aexp)
        (cond
            ((atom? aexp) (numbered? aexp))
            (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

; value interprets and evaluates an arithmetic expression.
(define value
    (lambda (nexp)
        (cond
            ((atom? nexp) nexp)
            ((eq? (car (cdr nexp)) (quote o+))
                (o+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
            ((eq? (car (cdr nexp)) (quote o*))
                (o* (value (car nexp)) (value (car (cdr (cdr nexp))))))
            (else (o^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

; value-prefix works exactly like value, except that it interprets and evaluates arithmetic 
; expressions that are written in prefix notation.
(define value-prefix
    (lambda (nexp)
        (cond
            ((atom? nexp) nexp)
            ((eq? (operator nexp) (quote o+)) 
                (o+ (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp))))
            ((eq? (operator nexp) (quote o*))
                (o* (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp))))
            (else (o^ (value-prefix (1st-sub-exp nexp)) (value-prefix (2nd-sub-exp nexp)))))))

; 1st-sub-exp provides the first number from an arithmetic expression.
(define 1st-sub-exp
    (lambda (aexp)
        (car aexp)))

; 2nd-sub-exp provides the second number from an arithmetic expression.
(define 2nd-sub-exp
    (lambda (aexp)
        (car (cdr (cdr aexp)))))

; operator provides the operator for an arithmetic expression.
(define operator
    (lambda (aexp)
        (car (cdr aexp))))

; sero? tests if a value passed into it is 0, using a list of empty lists in place of Arabic 
; characters to represent numbers.
(define sero?
    (lambda (n)
        (null? n)))

; edd1 adds 1 to a list provided, using a list of empty lists in place of Arabic characters to 
; represent numbers.
(define edd1
    (lambda (n)
        (cons (quote ()) n)))

; zub1 subtracts 1 from a list provided, using a list of empty lists in place of Arabic characters 
; to represent numbers.
(define zub1
    (lambda (n)
        (cdr n)))

; lo+ works exactly like o+ when using a list of empty lists in place of Arabic characters to 
; represent numbers.
(define lo+
    (lambda (n m)
        (cond
            ((sero? m) n)
            (else (edd1 (lo+ n (zub1 m)))))))

;;--------------------------------------------------------------------------------------------------

; Chapter 7: Friends and Relations

; set? determines if the lat passed as an argument is a set: a list of unique S-expressions.
(define set?
    (lambda (lat)
        (cond
            ((null? lat) #t)
            ((member? (car lat) (cdr lat)) #f)
            (else (set? (cdr lat))))))

; makeset creates a set from a lat by remembering to cons the first atom in the lat onto the result 
; of the natural recursion, after removing all occurrences of the first atom from the rest of the 
; lat.
(define makeset
    (lambda (lat)
        (cond
            ((null? lat) (quote ()))
            (else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

; subset? determines if the second set passed to it is a subset (a set where every S-expression 
; within it is also within the first set) of the first set passed to it.
(define subset?
    (lambda (set1 set2)
        (cond
            ((null? set1) #t)
            (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

; eqset? determines if two sets are equal to one another, by checking if both sets are subsets of 
; each other.
(define eqset?
    (lambda (set1 set2)
        (and (subset? set1 set2) (subset? set2 set1))))

; intersect provides all of the elements that are present in both of the sets provided as arguments. 
(define intersect
    (lambda (set1 set2)
        (cond
            ((null? set1) (quote ()))
            ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
            (else (intersect (cdr set1) set2)))))

; union creates a new set containing all of the distinct elements from the two sets passed to it as 
; arguments.
(define union
    (lambda (set1 set2)
        (cond
            ((null? set1) set2)
            ((member? (car set1) set2) (union (cdr set1) set2))
            (else (cons (car set1) (union (cdr set1) set2))))))

; difference returns all of the atoms in set1 that are not in set2.
(define difference
    (lambda set1 set2)
        (cond
            ((null? set1) (quote ()))
            ((member? (car set1) set2) (difference (cdr set1) set2))
            (else (cons (car set1) (difference (cdr set1) set2)))))

; intersectall works exactly like intersect, except that it finds the intersection of any number of 
; sets within a list provided as its argument.
(define intersectall
    (lambda (l-set)
        (cond
            ((null? (cdr l-set)) (car l-set))
            (else (intersect (car l-set) (intersectall (cdr l-set)))))))

; a-pair? determines if a provided list is a pair (a list with only two S-expressions).
(define a-pair?
    (lambda (x)
        (cond
            ((atom? x) #f)
            ((null? x) #f)
            ((null? (cdr x)) #f)
            ((null? (cdr (cdr x))) #t)
            (else #f))))

; first provides the first item in a list or pair.
(define first
    (lambda (p)
        (car p)))

; second provides the second item in a list or pair.
(define second
    (lambda (p)
        (car (cdr p))))

; build creates a pair from two S-expressions provided as arguments.
(define build
    (lambda (s1 s2)
        (cons s1 (cons s2 (quote ())))))

; third provides the third item in a list.
(define third
    (lambda (l)
        (car (car (cdr l)))))

; fun? determines if something is a function.
(define fun?
    (lambda (rel)
        (set? (firsts rel))))

; revrel reverses a relation.
(define revrel
    (lambda (rel)
        (cond
            ((null? rel) (quote ()))
            (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

; revpair switches the items in a pair.
(define revpair
    (lambda (pair)
        (build (second pair) (first pair))))

; fullfun? determines if the seconds of a list of pairs is a set.
(define fullfun?
    (lambda (fun)
        (set? (seconds fun))))

; one-to-one? works exactly like fullfun?.
(define one-to-one?
    (lambda (fun)
        (fun? (revrel fun))))
