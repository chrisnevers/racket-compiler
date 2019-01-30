(define-type List A
    [Nil A]
    [Cons (Vector A List)])

(define (cons?) : (Forall A (List -> Bool))
    (Lambda A
        (lambda ([x : List]) : Bool
            (case x
                [(Cons a) #t]
                [(Nil  b) #f]))))

(let ([my-list ((inst Cons Int) (vector 6 ((inst Nil Int) 5)))])
    ((inst cons? Int) my-list))
