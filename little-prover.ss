;;; load necessary files

(load "j-bob/scheme/j-bob-lang.scm")
(load "j-bob/scheme/j-bob.scm")
(load "j-bob/scheme/little-prover.scm")

;;; axioms

;; axioms of cons

(dethm atom/cons (x y)
       (equal (atom (cons x y)) 'nil))

(dethm car/cons (x y)
       (equal (car (cons x y)) x))

(dethm cdr/cons (x y)
       (equal (cdr (cons x y)) y))

(dethm cons/car+cdr (x)
       (if (atom x)
           't
           (equal (cons (car x) (cdr x)) x)))

;; axioms of equal

(dethm equal-same (x)
       (equal (equal x x) 't))

(dethm equal-swap (x y)
       (equal (equal x y)
              (equal y x)))

(dethm equal-if (x y)
       (if (equal x y) (equal x y) 't))

;; axioms of if

(dethm if-true (x y)
       (equal (if 't x y) x))

(dethm if-false (x y)
       (equal (if 'nil x y) y))

(dethm if-same (x y)
       (equal (if x y y) y))

(dethm if-nest-A (x y z)
       (if x
           (equal (if x y z) y)
           't))

(dethm if-nest-E (x y z)
       (if x
           't
           (equal (if x y z) z)))

(defun pair (x y)
  (cons x (cons y '())))

(defun first-of (x)
  (car x))

(defun second-of (x)
  (car (cdr x)))

(dethm first-of-pair (a b)
       (equal (first-of (pair a b)) a))

(dethm second-of-pair (a b)
       (equal (second-of (pair a b)) b))

(defun in-pair? (xs)
  (if (equal (first-of xs) '?)
      't
      (equal (second-of xs) '?)))

(dethm in-first-of-pair (b)
       (equal (in-pair? (pair '? b)) '?))

(dethm in-second-of-pair (a)
       (equal (in-pair? (pair a '?)) '?))

;;; proofs

(defun prelude+first-second-of-pair ()
  (J-Bob/define (prelude)
                '(((defun pair (x y)
                     (cons x (cons y '())))
                   nil)
                  ((defun first-of (x) (car x)) nil)
                  ((defun second-of (x) (car (cdr x))) nil)

                  ;; proof of first-pair
                  ((dethm first-of-pair (a b)
                          (equal (first-of (pair a b)) a))
                   nil
                   ((1 1) (pair a b))
                   ((1) (first-of (cons a (cons b '()))))
                   ((1) (car/cons a (cons b '())))
                   (() (equal-same a)))
                  
                  ;; proof of second-of-pair
                  ((dethm second-of-pair (a b)
                          (equal (second-of (pair a b)) b))
                   nil
                   ((1 1) (pair a b))
                   ((1) (second-of (cons a (cons b '()))))
                   ((1 1) (cdr/cons a (cons b '())))
                   ((1) (car/cons b '()))
                   (() (equal-same b))))))

;(J-Bob/prove (prelude)
;             '(((defun memb? (xs)
;                  (if (atom xs)
;                      'nil
;                      (if (equal (car xs) '?)
;                          't
;                          (memb? (cdr xs)))))
;                (size xs))
;               ((defun remb (xs)
;                  (if (atom xs)
;                      '()
;                      (if (equal (car xs) '?)
;                          (remb (cdr xs))
;                          (cons (car xs) (remb (cdr xs))))))
;                (size xs))))

