(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (caar l) (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (cadar l) (seconds (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? o1 (car lat))
           (eq? o2 (car lat)))
       (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
       (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
       (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
       (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define add1 (lambda (n) (+ n 1)))

(define sub1 (lambda (n) (- n 1)))

(define o+
  (lambda (m n)
    (cond
      ((zero? m) n)
      (else (o+ (sub1 m) (add1 n))))))
; (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else (o- (sub1 m) (sub1 n))))))
; (else (sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define o*
  (lambda (m n)
    (cond
      ((zero? m) 0)
      (else (o+ n (o* (sub1 m) n))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o> (sub1 m) (sub1 n))))))

(define o<
  (lambda (m n)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o< (sub1 m) (sub1 n))))))

(define o=
  (lambda (m n)
    (and (not (o> m n))
         (not (o< m n)))))

(define power
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (o* m (power m (sub1 n)))))))

(define o/
  (lambda (m n)
    (cond
      ((o< m n) 0)
      (else (add1 (o/ (- m n) n))))))

(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one? (lambda (n) (o= n 1)))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l))
                (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? a (car l))
           (member* a (cdr l))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (eql? (car l1) (car l2)) ; first elements of two lists equal?
                 (eqlist? (cdr l1) (cdr l2))))))) ; recursively ask cdr of lists

(define eql?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f) ; if one of them is not an atom
      (else (eqlist? s1 s2))))) ; now they are both lists

(define rember-2
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? s (car l)) (cdr l))
      (else (cons (car l) (rember-2 s (cdr l)))))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp))
                 (numbered? (caddr aexp)))))))

;;; e.g.: (1 + (2 * (3 ^ 4)))
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? '+ (cadr nexp)) (o+ (value (car nexp))
                                (value (caddr nexp))))
      ((eq? '* (cadr nexp)) (o* (value (car nexp))
                                (value (caddr nexp))))
      (else (power (value (car nexp)) (value (caddr nexp)))))))

(define 1st-sub-exp
  (lambda (aexp)
    (cadr aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (caddr aexp)))

(define operator (lambda (aexp) (car aexp)))

(define value-2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? '+ (operator nexp)) (o+ (value-2 (1st-sub-exp nexp))
                                    (value-2 (2nd-sub-exp nexp))))
      ((eq? '* (operator nexp)) (o* (value-2 (1st-sub-exp nexp))
                                    (value-2 (2nd-sub-exp nexp))))
      (else (power (value-2 (1st-sub-exp nexp))
                   (value-2 (2nd-sub-exp nexp)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
                 (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cddr x)) #t)
      (else #f))))

(define first
  (lambda (x)
    (car x)))

(define second
  (lambda (x)
    (cadr x)))

(define third
  (lambda (x)
    (caddr x)))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))

(define one-to-one?
  (lambda (rel)
    (fun? (revrel rel))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l)) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l)) (cons new l))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? old (car l)) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define seqrem
  (lambda (new old l)
    l))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) o+)
      ((eq? x '*) o*)
      (else power))))

(define value-3
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp))
             (value-3 (1st-sub-exp nexp))
             (value-3 (2nd-sub-exp nexp)))))))


