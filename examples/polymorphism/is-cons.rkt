(define-type List A
    [Nil A]
    [Cons (Vector A List)])

(define (cons?) : (Forall A (List -> Bool))
    (Lambda A
        (lambda ([x : List]) : Bool
            (case x
                [(Nil  b) #f]
                [(Cons a) #t]))))

(let ([my-list ((inst Cons Int) (vector 6 ((inst Nil Int) 5)))])
    ((inst cons? Int) my-list))
