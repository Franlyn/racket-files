;;Language: Intermediate student with lambda

;;(singletons los) consumes a list of number los, and produce a list
;;containing only those numbers that had no duplicates in the original list
;;Singletons: (listof Num) -> (listof Num)
;;Examples:
(check-expect (singletons '(1 2 2 3 4 3 2)) '(1 4))
(check-expect (singletons '(1 1 1)) empty)


(define (singletons los)
  (filter (lambda (x) (not (empty? x)))
          (foldr (lambda (f-los base)
                   (cons
                    (cond [(= 1 (length
                                 (filter (lambda(f-fil)
                                           (= f-los f-fil)) los))) f-los]
                          [else empty]) base)) empty los)))

;;Tests:
(check-expect (singletons empty) empty)
(check-expect (singletons '(1 2 1)) (list 2))
(check-expect (singletons '(1 2 2 3 4 3 2 5)) '(1 4 5))
(check-expect (singletons '(1 1 1 4 2 2 2 3)) '(4 3))



;;(duplicates los) consumes a list of number los, and produce a list
;;containing only those numbers that had duplicates in the original list
;;Singletons: (listof Num) -> (listof Num)
;;Examples:
(check-expect (duplicates '(1 2 2 3 4 3 2)) '(3 2))
(check-expect (duplicates '(1 1 2 2 3 4 3 2)) '(1 3 2))


(define (duplicates lst)
  (local [;;(remove-dup lst) consumes a list of number lst, and produce
          ;;a list without duplicates
          ;;Remove-dup: (listof (listof Num)) -> (listof (listof Num))
          (define (remove-dup los)
           (foldr (lambda (a base)
                   (cond[(not (member? a base)) (cons a base)]
                        [else base])) empty los))]
    (remove-dup (filter (lambda (x)
                          (not (member? x (singletons lst)))) lst))))

;;Tests:
(check-expect (duplicates '(1 1 2 2 3 3 4 5 5)) '(1 2 3 5))
(check-expect (duplicates empty) empty)
(check-expect (duplicates '(1 1 2 2 3 3 4 4 5 5)) '(1 2 3 4 5))
(check-expect (duplicates '(1 2 3 4 5 5)) '(5))
(check-expect (duplicates '(1 2 3)) empty)
