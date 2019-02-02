(define-type List A
    [Nil]
    [Cons (Vector A List)])

(define (cons?) : (Forall A (List -> Bool))
    (Lambda A
        (lambda ([x : List]) : Bool
            (case x
                [(Nil  b) #f]
                [(Cons a) #t]))))

(let ([my-list ((inst Cons Int) (vector 6 ((inst Nil Int))))])
    ((inst cons? Int) my-list))
