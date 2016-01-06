;;(mergesort lst predicate) consumes a list lst and a comparator function
;;and produces a list of sublists
;;Mergesort: (listof Any) (Any Any -> Boolean) -> (listof (listof Any))
;;Examples:
(check-expect (mergesort (list 1 2 3 4) >) (list 4 3 2 1))
(check-expect (mergesort (list 3 2 1) <) (list 1 2 3))


;;Helper function (merge los1 los2 predicate)
;;consumes two lists los1 and los2 and a comparator function predicate
;;and produce a new list which merged los1 and los2
;;Merge: (listof Any) (listof Any) -> (listof Any)
;;Cite from modulo 6
(define (merge los1 los2 predicate)
  (cond [(and (empty? los1) (cons? los2)) los2]
        [(and (cons? los1) (empty? los2)) los1]
        [(and (cons? los1) (cons? los2))
         (cond [(predicate (first los1) (first los2))
                (cons (first los1) (merge (rest los1) los2 predicate))]
               [else (cons (first los2)
                           (merge los1 (rest los2) predicate))])]))


;;Helper function (spl lst l) consumes one list lst
;;and the length of lst l, and produce one sublist of lst
;;Split: (listof Any) Nat -> (listof Any)
;;Require: length of lst is greater than 1
(define (spl lst l)
  (cond [(zero? l) empty]
        [else (cons (first lst) (spl (rest lst) (sub1 l)))]))


;;Helper function (split lst) consumes one list lst
;;and produce two sublists of lst
;;Split: (listof Any) -> (list (listof Any) (listof Any))
;;Require: length of lst is greater than 1
(define (split lst r)
  (cond [(even? r)
         (list (spl lst (/ r 2)) (reverse (spl (reverse lst) (/ r 2))))]
        [else
         (list (spl lst (ceiling (/ r 2)))
               (reverse (spl (reverse lst) (floor (/ r 2)))))]))


(define (mergesort lst predicate)
    (cond [(or (empty? lst) (empty? (rest lst))) lst]
          [else
           (local [;;define the constant after-split
                   (define after-split (split lst (length lst)))]
             (merge (mergesort (first after-split) predicate)
                    (mergesort (second after-split) predicate)
                    predicate))]))

;;Tests:
(check-expect (mergesort (list 1) >) (list 1))
(check-expect (mergesort (list 21) <) (list 21))
(check-expect (mergesort empty >) empty)
(check-expect (mergesort (list 3 2 1 5) <) (list 1 2 3 5))
(check-expect (mergesort (list 1 2 3 6 5) <) (list 1 2 3 5 6))
(check-expect (mergesort (list 1 3 2 1) >) (list 3 2 1 1))
(check-expect (mergesort (list 1 2 5 3 4) >) (list 5 4 3 2 1))
(check-expect (mergesort (list 1 2 5 3 4) >=) (list 5 4 3 2 1))
(check-expect (mergesort (list "f" "d" "a") string<?) (list "a" "d" "f"))
(check-expect (mergesort (list #\y #\f #\r) char<?) (list #\f #\r #\y))


