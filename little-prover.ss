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

;;; axioms of size

(dethm natp/size (x)
       (equal (natp (size x)) 't))

(dethm size/car (x)
       (if (atom x)
           't
           (equal (< (size (car x)) (size x))
                  't)))

(dethm size/cdr (x)
       (if (atom x)
           't
           (equal (< (size (cdr x)) (size x))
                  't)))

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

(defun defun-list? ()
  (J-Bob/define (prelude+first-second-of-pair)
                '(((defun list? (x)
                     (if (atom x)
                         (equal x '())
                         (list? (cdr x))))
                   (size x)
                   ((Q) (natp/size x))
                   (() (if-true (if (atom x)
                                    't
                                    (< (size (cdr x))
                                       (size x))) 'nil))
                   ((E) (size/cdr x))
                   (() (if-same (atom x) 't))))))

(defun defun-sub ()
  (J-Bob/define (prelude)
                '(((defun sub (x y)
                     (if (atom y)
                         (if (equal y '?)
                             x
                             y)
                         (cons (sub x (car y))
                               (sub x (cdr y)))))
                   (size y)
                   ((Q) (natp/size y))
                   (() (if-true
                        (if (atom y)
                            't
                            (if (< (size (car y)) (size y))
                                (< (size (cdr y)) (size y))
                                'nil))
                        'nil))
                   ((E Q) (size/car y))
                   ((E A) (size/cdr y))
                   ((E) (if-true 't 'nil))
                   (() (if-same (atom y) 't))))))

(defun defun-memb?-and-remb ()
  (J-Bob/define (prelude) ; prove two theorems inside one function

                ;; proof of memb?
                '(((defun memb? (xs)
                     (if (atom xs)
                         'nil
                         (if (equal (car xs) '?)
                             't
                             (memb? (cdr xs)))))
                   (size xs)
                   ((Q) (natp/size xs))
                   (() (if-true
                        (if (atom xs)
                            't
                            (if (equal (car xs) '?)
                                't
                                (< (size (cdr xs)) (size xs))))
                        'nil))
                   ((E E) (size/cdr xs))
                   ((E) (if-same (equal (car xs) '?) 't))
                   (() (if-same (atom xs) 't)))

                  ;; proof of remb
                  ((defun remb (xs)
                     (if (atom xs)
                         '()
                         (if (equal (car xs) '?)
                             (remb (cdr xs))
                             (cons (car xs) (remb (cdr xs))))))
                   (size xs)
                   ((Q) (natp/size xs))
                   (() (if-true
                        (if (atom xs)
                            't
                            (< (size (cdr xs)) (size xs)))
                        'nil))
                   ((E) (size/cdr xs))
                   (() (if-same (atom xs) 't))))))

(defun defun-memb?/remb0 ()
  (J-Bob/define (defun-memb?-and-remb)
                '(((dethm memb?/remb0 ()
                          (equal (memb? (remb '())) 'nil))
                   nil
                   ((1 1) (remb '())) ; expand remb
                   ((1 1 Q) (atom '()))
                   ((1 1) (if-true '()
                                   (if (equal (car '()) '?)
                                       (remb (cdr '()))
                                       (cons (car '())
                                             (remb (cdr '()))))))
                   ((1) (memb? '())) ; expand memb?
                   ((1 Q) (atom '()))
                   ((1) (if-true
                         'nil
                         (if (equal (car '()) '?)
                             't
                             (memb? (cdr '())))))
                   (() (equal-same 'nil))))))

;;; misc

(defun list0? (x)
  (equal x '()))

(defun list1? (x)
  (if (atom x)
      'nil
      (list0? (cdr x))))

(defun list2? (x)
  (if (atom x)
      'nil
      (list1? (cdr x))))

(defun list? (x)
  (if (atom x)
      (equal x '())
      (list? (cdr x))))

(defun size (x)
  (if (atom x)
      '0
      (+ '1 (size (car x)) (size (cdr x)))))

(defun sub (x y)
  (if (atom y)
      (if (equal y '?)
          x
          y)
      (cons (sub x (car y))
            (sub x (cdr y)))))

(defun memb? (xs)
  (if (atom xs)
      'nil
      (if (equal (car xs) '?)
          't
          (memb? (cdr xs)))))

(defun remb (xs)
  (if (atom xs)
      '()
      (if (equal (car xs) '?)
          (remb (cdr xs))
          (cons (car xs) (remb (cdr xs))))))

(dethm memb?/remb0 ()
       (equal (memb? (remb '())) 'nil))