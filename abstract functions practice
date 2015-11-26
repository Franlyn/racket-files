;;Language: Intermediate student with lambda
;;Require: Do not use explicit recursion


;;(intersection los1 los2) consumes two lists los1 los2, and produces
;;a list of all values shared by bith lists, with no duplicates
;;intersection: (listof Any) (listof Any) -> (listof Any)
;;Examples:
(check-expect (intersection '(3 4 2 1) '(1 5 7 3)) '(3 1))
(check-expect (intersection '(3 4 2 1) '(1 3 7 3)) '(3 1))


(define (intersection los1 los2)
  (filter (lambda (x) (cond [(member? x los2) true]
                            [else false]))
          (foldr (lambda (a base)
                   (cond[(not (member? a base)) (cons a base)]
                        [else base])) empty los1)))

;;Tests:
(check-expect (intersection empty empty) empty)
(check-expect (intersection empty '(1 5 7 3)) empty)
(check-expect (intersection '(3 4 2 1) empty) empty)
(check-expect (intersection '(1 2 3 4) '(1 2 3 4)) '(1 2 3 4))
(check-expect (intersection '(1 1 1 1) '(1 1 1 1)) '(1))
(check-expect (intersection '(1 2 1 1) '(1 1 2 1)) '(2 1))
(check-expect (intersection '(a b a a) '(a a b a)) '(b a))



;;Q2 (b)
;;(union los1 los2) consumes two lists los1 and los2, and produces
;;a list containing all the elements of both lists with no duplicates
;;Union: (listof Any) (listof Any) -> (listof Any)
;;Examples:
(check-expect (union '(3 4 2 1) '(1 5 7 3)) '(5 7 3 4 2 1))
(check-expect (union '(3 4 2 2) '(1 5 7 3)) '(1 5 7 3 4 2))


(define (union los1 los2)
  (foldr (lambda (a base)
           (cond [(not (member? a base)) (cons a base)]
                 [else base]))
         empty (foldr (lambda (l los) (cons l los))
                      los1 los2)))

;;Tests:
(check-expect (union empty empty) empty)
(check-expect (union empty '(1 5 7 3)) '(1 5 7 3))
(check-expect (union '(3 4 2 2) empty) '(3 4 2))
(check-expect (union '(3 2 2 1) '(1 5 7 3)) '(5 7 3 2 1))
(check-expect (union '(a b c c) '(c b a)) '(a b c))



;;Q2 (c)
;;(unique-fn los predicate) consumes a list los and
;;a predicate equality function, and produces the same list
;;that all duplicates, according to
;;the provided equality predicate, are removed
;;Unique-fn: (listof Any) Predicate -> (listof Any)
;;Examples:
(check-expect (unique-fn '(3 1 3) =) '(3 1))
(check-expect (unique-fn '(3 1 3 1) =) '(3 1))


(define (unique-fn los predicate)
  (foldr (lambda (x y)
           (cons x (filter (lambda (z)
                             (not (predicate x z))) y))) empty los))

;;Tests:
(check-expect (unique-fn '("a" "b" "a") string=?) '("a" "b"))
(check-expect (unique-fn (list 'a 'b 'a ) symbol=?) (list 'a 'b))
(check-expect (unique-fn (list 1 2 3 4 3 2 1) >) (list 1 2 3 4))
(check-expect (unique-fn '(1 1.05 2 1.2)
                         (lambda (x y) (> 0.1 (abs (- x y))))) '(1 2 1.2))
(check-expect (unique-fn '(1 2 3)
                         (lambda (x y) (= 1 (abs (- x y))))) '(1))
(check-expect (unique-fn '(1 2 3 4 5 5) =) '(1 2 3 4 5))
(check-expect empty empty)



;;Q2 (d)
;;(cross los1 los2) consumes two lists los1 and los2, and produces
;;a list of all possible pairs of elements from the two lists
;;Cross: (listof Any) (listof Any) -> (listof (listof Any))
;;Examples:
(check-expect (cross '(1 2 3 4) '(3 2))
              '((1 3) (1 2) (2 3) (2 2) (3 3) (3 2) (4 3) (4 2)))
(check-expect (cross '(1 2 2 4) '(3 2))
              '((1 3) (1 2) (2 3) (2 2) (4 3) (4 2)))


(define (cross los1 los2)
  (foldr (lambda (a base1)
           (cond[(not (member? a base1)) (cons a base1)]
                [else base1]))
         empty (foldr (lambda (x b)
                        (foldr (lambda (lst1 lst2)
                                 (cons lst1 lst2))
                               b (foldr (lambda (y base)
                                          (cons (list x y) base))
                                        empty los2))) empty los1)))

;;Tests:
(check-expect (cross empty '(3 2)) empty)
(check-expect (cross '(1 2 3) empty) empty)
(check-expect (cross '(1 2 3) '(3 2))
              '((1 3) (1 2) (2 3) (2 2) (3 3) (3 2)))
(check-expect (cross '(4 5) '(3 2)) '((4 3) (4 2) (5 3) (5 2)))



;;Q2 (e)
;;(jaccard arg1 arg2) consumes two non-empty list of numbers arg1 arg2
;;and produces a number that represents how similar they are
;;Jaccard: (listof Num) (listof Num) -> Num
;;Require: arg1 and arg2 are non-empty (of equal length)
;;Examples:
(check-expect (jaccard '(3 4 2 1) '(1 5 7 3)) 1/3)
(check-expect (jaccard '(3 4 2 1) '(1 5 5 3)) 2/5)


(define (jaccard arg1 arg2)
  (/ (length (intersection arg1 arg2)) (length (union arg1 arg2))))


;;Tests:
(check-expect (jaccard '(1 2 3 4) '(1 2 3 4)) 1)
(check-expect (jaccard '(3 4 2 1) '(5 6 7 8)) 0)
(check-expect (jaccard '(3 4 2 1 5) '(1 5 7 3)) 1/2)



;;Q2 (f)
;;(take los n) consumes a list los and a natural number n
;;and produces a list containing only the first n elements from the list
;;or the entire list if it contains fewer than n elements
;;take: (listof Any) Nat -> (listof Any)
;;Examples:
(check-expect (take '(a b c d e f g) 3) '(a b c))
(check-expect (take '(a b) 3) '(a b))


(define (take los n)
  (cond [(= n 0) empty]
        [(>= n (length los)) los]
        [else (cons (first los) (take (rest los) (sub1 n)))]))

;;Tests:
(check-expect (take empty 2) empty)
(check-expect (take '(a b c d e f g) 0) empty)
(check-expect (take '(a b c d e f g) 5) '(a b c d e))
(check-expect (take '(a b c d e f g) 7) '(a b c d e f g))
(check-expect (take '(a b c d e f g) 8) '(a b c d e f g))


;;************************************ Q3 **************************************

;; A Feature Vector (FV) is a (listof Num)

;; A Document Identifier (DI) is a String

;; A Document Vector (DV) is a (list DI FV)

;; A Feature-Vector Association List (FV-AL) is
;; * empty
;; * (cons DV FV-AL)
;; require: each FV must be of the same length
;; each DV must be unique

;; A Document Pair Tuple (DPT) is a (list DI DI Num)
;; where Num corresponds to some similarity between
;; the feature vectors associated with each DI


;;Q3 (a)
;;(cmp-with-sim dv fv-al measure) consumes a DV dv, a FY-AL fv-al, and
;;a similarity measure measure, and produces a list of DPTs
;;one for each DV in the FV-AL
;;Cmp-with-sim: DV FV-AL Predicate -> (listof DPTs)
;;Examples:
(check-expect (cmp-with-sim (list "t1" '(1 2 3 4))
                            '(("t1" (1 2 3 4)) ("t2" (2 5 1 6))) jaccard)
              '(("t1" "t1" 1) ("t1" "t2" 1/3)))
(check-expect (cmp-with-sim (list "t1" '(1 2 3 4))
                            '(("t1" (1 2 3 4)) ("t2" (2 5 1 6))
                                               ("t3" (1 1 1 1))) jaccard)
              '(("t1" "t1" 1) ("t1" "t2" 1/3) ("t1" "t3" 1/4)))


(define (cmp-with-sim dv fv-al measure)
  (foldr (lambda (x y)
           (cons (list (first dv) (first x)
                       (measure (second dv) (second x))) y)) empty fv-al))


;;Tests:
(check-expect (cmp-with-sim (list "t1" '(1 2 3 4))
                            '(("t1" (1 2 3 4)) ("t2" (2 5 1 6))
                                               ("t3" (1 2 3 5))) jaccard)
              '(("t1" "t1" 1) ("t1" "t2" 1/3) ("t1" "t3" 3/5)))
(check-expect (cmp-with-sim '("t4" (7 8 9 6)) '
                            (("t1" (1 2 3 4)) ("t2" (2 5 1 6))
                                              ("t3" (1 2 3 5))) jaccard)
              '(("t4" "t1" 0) ("t4" "t2" 1/7) ("t4" "t3" 0)))
(check-expect (cmp-with-sim (list "t1" '(1 2 3 4)) empty jaccard) empty)
(check-expect (cmp-with-sim (list "t1" '(1 2 3 4))
                            '(("t2" (2 5 1 6)) ("t3" (1 2 3 5))) jaccard)
              '(("t1" "t2" 1/3) ("t1" "t3" 3/5)))



;;Q3 (b)
;;(find-all-exact fv-al measure) consumes a FV-AL fv-al and a similarity
;;measure, and produces the list of DPTs that correspond to all ordered
;;pairs of DIs in FV-AL that are exact duplicates of each other
;;and report their similarity as a Num, excluding DPTs with
;;identical IDs should not be included in the produced list
;;Find-all-exact:
;;FV-AL ((listof Any)(listof Any) -> Num) -> (listof DPT)
;;Examples:
(check-expect (find-all-exact '(("t1" (1 2 3 4)) ("t3" (4 3 2 1)))
                              jaccard) '(("t3" "t1" 1) ("t1" "t3" 1)))
(check-expect (find-all-exact empty jaccard) empty)


;;Define constant similarity s
(define s 1)


(define (find-all-exact fv-al measure)
  (foldr (lambda (first-al base)
           (union
            (foldr (lambda (f-filter b)
                     (cons (list (first first-al) (first f-filter)
                                 (measure (second first-al)
                                            (second f-filter))) b))
                   empty (filter (lambda (f-al)
                                   (and (not (string=? (first f-al)
                                                       (first first-al)))
                                        (= s (measure
                                              (second f-al)
                                              (second first-al))))) fv-al))
            base)) empty fv-al))

;;Tests:
(check-expect (find-all-exact empty jaccard) empty)
(check-expect (find-all-exact '(("t1" (1 2 3 4)) ("t3" (1 2 2 1)))
                              jaccard) empty)
(check-expect (find-all-exact '(("t1" (1 2 3 4)) ("t3" (4 3 2 1))
                                                 ("t4" (4 3 2 1)))
                              jaccard) '(("t4" "t1" 1)
                                         ("t4" "t3" 1)
                                         ("t3" "t1" 1)
                                         ("t3" "t4" 1)
                                         ("t1" "t3" 1)
                                         ("t1" "t4" 1)))
(check-expect (find-all-exact '(("t1" (1 2 3 4)) ("t3" (4 3 2 1))
                                                 ("t4" (4 3 2 1))
                                                 ("t5" (1 3 2 1)))
                              jaccard) '(("t4" "t1" 1)
                                         ("t4" "t3" 1)
                                         ("t3" "t1" 1)
                                         ("t3" "t4" 1)
                                         ("t1" "t3" 1)
                                         ("t1" "t4" 1)))


;;Q3 (c)
;;(redundant? dpt1 dpt2) consumes two DPTs and determines if the tuples
;;are redundant(represent the same set of DIs)
;;Redundant?: DPT DPT -> Boolean
;;Examples:
(check-expect (redundant? '("t1" "t2" 1) '("t2" "t1" 1)) true)
(check-expect (redundant? '("t1" "t2" 1) '("t2" "t3" 1)) false)


(define (redundant? dpt1 dpt2)
  (andmap (lambda (x) (ormap (lambda (a) (equal? x a)) dpt1)) dpt2))

;;Tests:
(check-expect (redundant? empty empty) true)
(check-expect (redundant? empty '("t2" "t3" 1)) false)
(check-expect (redundant? '("t1" "t2" 1) empty) true)
(check-expect (redundant? '("t1" "t2" 1) '("t2" "t4" 1)) false)
(check-expect (redundant? '("t1" "t2" 1) '("t1" "t2" 1)) true)
(check-expect (redundant? '("t3" "t4" 1/2) '("t4" "t3" 1/2)) true)
(check-expect (redundant? '("t3" "t4" 1/2) '("t4" "t5" 1/3)) false)


;;Q3 (d)
;;(find-similar-within-d fv-al d measure) consumes an FV-AL fv-al
;;a threshold d ∈ [0, 1], and a similarity measure measure
;;and produces the DPTs of all combinations of pairs in the provided FV-AL
;;whose similarity is above the threshold d
;;Find-similar-within-d:
;;FV-AL Num ((listof Any)(listof Any) -> Num) -> (listof DPT)
;;Require: d ∈ [0, 1]
;;Examples:
(check-expect (find-similar-within-d '(("t1" (1 2 3 4))
                                       ("t2" (2 5 1 6))
                                       ("t3" (7 2 3 1))) 0.5 jaccard)
              '(("t3" "t1" 0.6)))
(check-expect (find-similar-within-d '(("t1" (1 2 3 4))
                                       ("t2" (2 5 1 6))) 0.5 jaccard)
              empty)


(define (find-similar-within-d fv-al d measure)
  (unique-fn
   (foldr (lambda (first-al base)
            (union
             (foldr (lambda (f-filter b)
                      (cons (list (first first-al) (first f-filter)
                                  (measure (second first-al)
                                           (second f-filter))) b))
                    empty (filter (lambda (f-al)
                                    (and (not (string=? (first f-al)
                                                        (first first-al)))
                                         (> (measure
                                             (second f-al)
                                             (second first-al)) d))) fv-al))
             base)) empty fv-al) redundant?))

;;Tests:
(check-expect (find-similar-within-d empty 0.5 jaccard) empty)
(check-expect (find-similar-within-d '(("t1" (1 2 3 4))
                                       ("t3" (2 5 1 6))) 0.5 jaccard)
              empty)
(check-expect (find-similar-within-d '(("t1" (1 2 3 4))
                                       ("t2" (2 4 1 6))
                                       ("t3" (1 2 3 1))) 0.5 jaccard)
              '(("t3" "t1" 0.75) ("t2" "t1" 0.6)))
(check-expect (find-similar-within-d '(("t1" (1 2 3 4))
                                       ("t2" (2 5 1 6))
                                       ("t3" (2 7 8 9))) 0.5 jaccard)
              empty)
(check-expect (find-similar-within-d '(("t1" (1 2 3 4))
                                       ("t2" (2 4 1 6))
                                       ("t3" (1 2 3 1))) 0.3 jaccard)
              '(("t3" "t1" 0.75) ("t3" "t2" 0.4) ("t2" "t1" 0.6)))


;;Q3 (e)
;;(find-k-similar-within-d fv-al d k measure) consumes an FV-AL
;;a threshold d ∈ [0, 1], a positive integer k, and a similarity measure.
;;and produces a list of DPTs for the k most similar pairs of FVs in the
;;FV-AL whose similarity is above the threshold d.
;;Find-k-similar-within-d:
;;FV-AL Num Num ((listof Any)(listof Any) -> Num) -> (listof DPT)
;;Example:
(check-expect (find-k-similar-within-d '(("t1" (1 2 3 4))
                                         ("t2" (5 3 2 1))
                                         ("t3" (4 3 2 1))) .5 2 jaccard)
              '(("t2" "t1" 0.6) ("t3" "t1" 1)))
(check-expect (find-k-similar-within-d '(("t1" (1 2 3 4))
                                         ("t2" (2 5 1 6))
                                         ("t3" (4 2 3 1))
                                         ("t4" (7 2 3 1))) 0.5 1 jaccard)
              '(("t3" "t1" 1)))


(define (find-k-similar-within-d fv-al d k measure)
  (foldr (lambda (f-los base)
           (cond [(< (length base) k) (cons f-los base)]
                 [else base]))
         empty (sort (find-similar-within-d fv-al d measure)
                     (lambda (x y) (cond[(< (third x) (third y)) true]
                                        [else false])))))

;;Tests:
(check-expect (find-k-similar-within-d empty .5 1 jaccard) empty)
(check-expect (find-k-similar-within-d '(("t1" (1 2 3 4))
                                         ("t2" (5 3 2 1))
                                         ("t3" (4 3 2 1))) .5 1 jaccard)
              '(("t3" "t1" 1)))
(check-expect (find-k-similar-within-d '(("t1" (1 2 3 4))
                                         ("t2" (2 5 1 6))
                                         ("t3" (4 2 3 1))
                                         ("t4" (7 2 3 1))) 0.5 2 jaccard)
              '(("t4" "t3" 0.6) ("t3" "t1" 1)))
(check-expect (find-k-similar-within-d '(("t1" (1 2 3 4))
                                         ("t2" (5 3 2 1))
                                         ("t3" (4 3 2 1))) .5 5 jaccard)
              '(("t3" "t2" 0.6) ("t2" "t1" 0.6) ("t3" "t1" 1)))
(check-expect (find-k-similar-within-d '(("t1" (1 2 3 4))
                                         ("t2" (5 3 2 1))
                                         ("t3" (4 3 2 1))) .5 3 jaccard)
              '(("t3" "t2" 0.6) ("t2" "t1" 0.6) ("t3" "t1" 1)))

