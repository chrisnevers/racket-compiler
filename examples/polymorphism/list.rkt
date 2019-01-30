(define-type List A
    [Nil A]
    [Cons (Vector A List)])

(define (id) : (Forall A (List -> List))
    (Lambda A
        (lambda ([x : List]) : List x)))

(let ([my-list ((inst Cons Int) (vector 6 ((inst Nil Int) 5)))])
    ((inst id Int) my-list))
